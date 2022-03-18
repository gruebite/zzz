//! zzz format serializer and deserializer. Public domain.
//!
//! SPARSE SPEC
//! (zzz text is escaped using Zig's multiline string: \\)
//!
//! zzz text describes a tree of strings. Special characters and spaces are used to go up and down
//! the tree. The tree has an implicit empty root node.
//!
//! Descending the tree:
//! \\grandparent:parent:child:grandchild
//! Output:
//! null -> "grandparent" -> "parent" -> "child" -> "grandchild"
//!
//! Traversing the children of root (siblings):
//! \\sibling1,sibling2,sibling3
//! Output:
//! null -> "sibling1"
//!      -> "sibling2"
//!      -> "sibling3"
//!
//! Going up to the parent:
//! \\parent:child;anotherparent
//! Output:
//! null -> "parent" -> "child"
//!      -> "anotherparent"
//!
//! White space and newlines are significant. A newline will take you back to the root:
//! \\parent:child
//! \\anotherparent
//! Output:
//! null -> "parent" -> "child"
//!      -> "anotherparent"
//!
//! Exactly two spaces are used to to go down a level in the tree:
//! \\parent:child
//! \\  siblingtend
//! null -> "parent" -> "child"
//!                  -> "sibling"
//!
//! You can only go one level deeper than the previous line's depth. Anything more is an error:
//! \\parent:child
//! \\    sibling
//! Output: Error!
//!
//! Trailing commas, semicolons, and colons are optional. So the above (correct one) can be written
//! as:
//! \\parent
//! \\  child
//! \\  sibling
//! Output:
//! null -> "parent" -> "child"
//!                  -> "sibling"
//!
//! zzz can contain strings, integers (i32), floats (f32), boolean, and nulls:
//! \\string:42:42.0:true::
//! Output:
//! null -> "string" -> 42 -> 42.0 -> true -> null
//!
//! strings are trimmed, they may still contain spaces:
//! \\parent:     child:      grand child      ;
//! Output:
//! null -> "parent" -> "child" -> "grand child"
//!
//! strings can be quoted with double quotes or Lua strings:
//! \\"parent":[[ child ]]:[==[grand child]=]]==];
//! Output:
//! null -> "parent" -> " child " -> "grand child]=]"
//!
//! Lua strings will skip the first empty newline:
//! \\[[
//! \\some text]]
//! Output:
//! null -> "some text"
//!
//! Strings are not escaped and taken "as-is".
//! \\"\n\t\r"
//! Output:
//! null -> "\n\t\r"
//!
//! Comments begin with # and run up to the end of the line. Their indentation follows the same
//! rules as nodes.
//! \\# A comment
//! \\a node
//! \\  # Another comment
//! \\  a sibling
//! Output:
//! null -> "a node" -> "a sibling"

const std = @import("std");
const mem = std.mem;

/// The only output of the tokenizer.
pub const ZNodeToken = struct {
    const Self = @This();
    /// 0 is root, 1 is top level children.
    depth: usize,
    /// The extent of the slice.
    start: usize,
    end: usize,
};

/// Parses text outputting ZNodeTokens. Does not convert strings to numbers, and all strings are
/// "as is", no escaping is performed.
pub const StreamingParser = struct {
    const Self = @This();
    state: State,
    start_index: usize,
    current_index: usize,
    // The maximum node depth.
    max_depth: usize,
    // The current line's depth.
    line_depth: usize,
    // The current node depth.
    node_depth: usize,
    /// Level of multiline string.
    open_string_level: usize,
    /// Current level of multiline string close.
    close_string_level: usize,
    /// Account for any extra spaces trailing at the end of a word.
    trailing_spaces: usize,

    pub const Error = error{
        TooMuchIndentation,
        InvalidWhitespace,
        OddIndentationValue,
        InvalidQuotation,
        InvalidMultilineOpen,
        InvalidMultilineClose,
        InvalidNewLineInString,
        InvalidCharacterAfterString,
        SemicolonWentPastRoot,
        UnexpectedEof,
    };

    pub const State = enum {
        /// Whether we're starting on an openline.
        OpenLine,
        ExpectZNode,
        Indent,
        OpenCharacter,
        Quotation,
        SingleLineCharacter,
        MultilineOpen0,
        MultilineOpen1,
        MultilineLevelOpen,
        MultilineLevelClose,
        MultilineClose0,
        MultilineCharacter,
        EndString,
        OpenComment,
        Comment,
    };

    /// Returns a blank parser.
    pub fn init() Self {
        var self: StreamingParser = undefined;
        self.reset();
        return self;
    }

    /// Resets the parser back to the beginning state.
    pub fn reset(self: *Self) void {
        self.state = .OpenLine;
        self.start_index = 0;
        self.current_index = 0;
        self.max_depth = 0;
        self.line_depth = 0;
        self.node_depth = 0;
        self.open_string_level = 0;
        self.close_string_level = 0;
        self.trailing_spaces = 0;
    }

    pub fn completeOrError(self: *const Self) !void {
        switch (self.state) {
            .ExpectZNode, .OpenLine, .EndString, .Comment, .OpenComment, .Indent => {},
            else => return Error.UnexpectedEof,
        }
    }

    /// Feeds a character to the parser. May output a ZNode. Check "hasCompleted" to see if there
    /// are any unfinished strings.
    pub fn feed(self: *Self, c: u8) Error!?ZNodeToken {
        defer self.current_index += 1;
        //std.debug.print("FEED<{}> {} {} ({c})\n", .{self.state, self.current_index, c, c});
        switch (self.state) {
            .OpenComment, .Comment => switch (c) {
                '\n' => {
                    self.start_index = self.current_index + 1;
                    // We're ending a line with nodes.
                    if (self.state == .Comment) {
                        self.max_depth = self.line_depth + 1;
                    }
                    self.node_depth = 0;
                    self.line_depth = 0;
                    self.state = .OpenLine;
                },
                else => {
                    // Skip.
                },
            },
            // All basically act the same except for a few minor differences.
            .ExpectZNode, .OpenLine, .EndString, .OpenCharacter => switch (c) {
                '#' => {
                    if (self.state == .OpenLine) {
                        self.state = .OpenComment;
                    } else {
                        defer self.state = .Comment;
                        if (self.state == .OpenCharacter) {
                            return ZNodeToken{
                                .depth = self.line_depth + self.node_depth + 1,
                                .start = self.start_index,
                                .end = self.current_index - self.trailing_spaces,
                            };
                        }
                    }
                },
                // The tricky character (and other whitespace).
                ' ' => {
                    if (self.state == .OpenLine) {
                        if (self.line_depth >= self.max_depth) {
                            return Error.TooMuchIndentation;
                        }
                        self.state = .Indent;
                    } else if (self.state == .OpenCharacter) {
                        self.trailing_spaces += 1;
                    } else {

                        // Skip spaces when expecting a node on a closed line,
                        // including this one.
                        self.start_index = self.current_index + 1;
                    }
                },
                ':' => {
                    defer self.state = .ExpectZNode;
                    const node = ZNodeToken{
                        .depth = self.line_depth + self.node_depth + 1,
                        .start = self.start_index,
                        .end = self.current_index - self.trailing_spaces,
                    };
                    self.start_index = self.current_index + 1;
                    self.node_depth += 1;
                    // Only return when we're not at end of a string.
                    if (self.state != .EndString) {
                        return node;
                    }
                },
                ',' => {
                    defer self.state = .ExpectZNode;
                    const node = ZNodeToken{
                        .depth = self.line_depth + self.node_depth + 1,
                        .start = self.start_index,
                        .end = self.current_index - self.trailing_spaces,
                    };
                    self.start_index = self.current_index + 1;
                    // Only return when we're not at end of a string.
                    if (self.state != .EndString) {
                        return node;
                    }
                },
                ';' => {
                    if (self.node_depth == 0) {
                        return Error.SemicolonWentPastRoot;
                    }
                    defer self.state = .ExpectZNode;
                    const node = ZNodeToken{
                        .depth = self.line_depth + self.node_depth + 1,
                        .start = self.start_index,
                        .end = self.current_index - self.trailing_spaces,
                    };
                    self.start_index = self.current_index + 1;
                    self.node_depth -= 1;
                    // Only return when we're not at end of a string, or in semicolons
                    // special case, when we don't have an empty string.
                    if (self.state != .EndString and node.start < node.end) {
                        return node;
                    }
                },
                '"' => {
                    if (self.state == .EndString) {
                        return Error.InvalidCharacterAfterString;
                    }
                    // Don't start another string.
                    if (self.state == .OpenCharacter) {
                        return null;
                    }
                    // We start here to account for the possibility of a string being ""
                    self.start_index = self.current_index + 1;
                    self.state = .Quotation;
                },
                '[' => {
                    if (self.state == .EndString) {
                        return Error.InvalidCharacterAfterString;
                    }
                    // Don't start another string.
                    if (self.state == .OpenCharacter) {
                        return null;
                    }
                    self.open_string_level = 0;
                    self.state = .MultilineOpen0;
                },
                '\n' => {
                    defer self.state = .OpenLine;
                    const node = ZNodeToken{
                        .depth = self.line_depth + self.node_depth + 1,
                        .start = self.start_index,
                        .end = self.current_index - self.trailing_spaces,
                    };
                    self.start_index = self.current_index + 1;
                    // Only reset on a non open line.
                    if (self.state != .OpenLine) {
                        self.max_depth = self.line_depth + 1;
                        self.line_depth = 0;
                    }
                    self.node_depth = 0;
                    // Only return something if there is something. Quoted strings are good.
                    if (self.state == .OpenCharacter) {
                        return node;
                    }
                },
                '\t', '\r' => {
                    return Error.InvalidWhitespace;
                },
                else => {
                    // We already have a string.
                    if (self.state == .EndString) {
                        return Error.InvalidCharacterAfterString;
                    }
                    // Don't reset if we're in a string.
                    if (self.state != .OpenCharacter) {
                        self.start_index = self.current_index;
                    }
                    self.trailing_spaces = 0;
                    self.state = .OpenCharacter;
                },
            },
            .Indent => switch (c) {
                ' ' => {
                    self.start_index = self.current_index + 1;
                    self.line_depth += 1;
                    self.state = .OpenLine;
                },
                else => {
                    return Error.OddIndentationValue;
                },
            },
            .Quotation => switch (c) {
                '"' => {
                    self.state = .EndString;
                    const node = ZNodeToken{
                        .depth = self.line_depth + self.node_depth + 1,
                        .start = self.start_index,
                        .end = self.current_index,
                    };
                    // Reset because we're going to expecting nodes.
                    self.start_index = self.current_index + 1;
                    return node;
                },
                else => {
                    self.state = .SingleLineCharacter;
                },
            },
            .SingleLineCharacter => switch (c) {
                '"' => {
                    self.state = .EndString;
                    const node = ZNodeToken{
                        .depth = self.line_depth + self.node_depth + 1,
                        .start = self.start_index,
                        .end = self.current_index,
                    };
                    // Reset because we're going to expecting nodes.
                    self.start_index = self.current_index + 1;
                    return node;
                },
                '\n' => {
                    return Error.InvalidNewLineInString;
                },
                else => {
                    // Consume.
                },
            },
            .MultilineOpen0, .MultilineLevelOpen => switch (c) {
                '=' => {
                    self.open_string_level += 1;
                    self.state = .MultilineLevelOpen;
                },
                '[' => {
                    self.start_index = self.current_index + 1;
                    self.state = .MultilineOpen1;
                },
                else => {
                    return Error.InvalidMultilineOpen;
                },
            },
            .MultilineOpen1 => switch (c) {
                ']' => {
                    self.state = .MultilineClose0;
                },
                '\n' => {
                    // Skip first newline.
                    self.start_index = self.current_index + 1;
                },
                else => {
                    self.state = .MultilineCharacter;
                },
            },
            .MultilineCharacter => switch (c) {
                ']' => {
                    self.close_string_level = 0;
                    self.state = .MultilineClose0;
                },
                else => {
                    // Capture EVERYTHING.
                },
            },
            .MultilineClose0, .MultilineLevelClose => switch (c) {
                '=' => {
                    self.close_string_level += 1;
                    self.state = .MultilineLevelClose;
                },
                ']' => {
                    if (self.close_string_level == self.open_string_level) {
                        self.state = .EndString;
                        return ZNodeToken{
                            .depth = self.line_depth + self.node_depth + 1,
                            .start = self.start_index,
                            .end = self.current_index - self.open_string_level - 1,
                        };
                    }
                    self.state = .MultilineCharacter;
                },
                else => {
                    return Error.InvalidMultilineClose;
                },
            },
        }
        return null;
    }

    /// Parses the stream, outputting ZNodeTokens which reference the text.
    pub fn parse(self: *Self, idx: *usize, text: []const u8) !?ZNodeToken {
        while (idx.* <= text.len) {
            // Insert an extra newline at the end of the stream.
            const node = if (idx.* == text.len) try self.feed('\n') else try self.feed(text[idx.*]);
            idx.* += 1;
            if (node) |n| {
                return n;
            }
        }
        return null;
    }
};

fn testNextTextOrError(stream: *StreamingParser, idx: *usize, text: []const u8) ![]const u8 {
    while (idx.* < text.len) {
        const node = try stream.feed(text[idx.*]);
        idx.* += 1;
        if (node) |n| {
            //std.debug.print("TOKEN {}\n", .{text[n.start..n.end]});
            return text[n.start..n.end];
        }
    }
    return error.ExhaustedLoop;
}

test "parsing slice output" {
    const testing = std.testing;

    const text =
        \\# woo comment
        \\mp:10
        \\[[sy]]
        \\  # another
        \\  : n : "en"  ,  [[m]]
        \\    "sc"   :  [[10]]   ,    g #inline
        \\  [[]]:[==[
        \\hi]==]
    ;
    var idx: usize = 0;
    var stream = StreamingParser.init();
    try testing.expectEqualSlices(u8, "mp", try testNextTextOrError(&stream, &idx, text));
    try testing.expectEqualSlices(u8, "10", try testNextTextOrError(&stream, &idx, text));
    try testing.expectEqualSlices(u8, "sy", try testNextTextOrError(&stream, &idx, text));
    try testing.expectEqualSlices(u8, "", try testNextTextOrError(&stream, &idx, text));
    try testing.expectEqualSlices(u8, "n", try testNextTextOrError(&stream, &idx, text));
    try testing.expectEqualSlices(u8, "en", try testNextTextOrError(&stream, &idx, text));
    try testing.expectEqualSlices(u8, "m", try testNextTextOrError(&stream, &idx, text));
    try testing.expectEqualSlices(u8, "sc", try testNextTextOrError(&stream, &idx, text));
    try testing.expectEqualSlices(u8, "10", try testNextTextOrError(&stream, &idx, text));
    try testing.expectEqualSlices(u8, "g", try testNextTextOrError(&stream, &idx, text));
    try testing.expectEqualSlices(u8, "", try testNextTextOrError(&stream, &idx, text));
    try testing.expectEqualSlices(u8, "hi", try testNextTextOrError(&stream, &idx, text));
}

fn testNextLevelOrError(stream: *StreamingParser, idx: *usize, text: []const u8) !usize {
    while (idx.* < text.len) {
        const node = try stream.feed(text[idx.*]);
        idx.* += 1;
        if (node) |n| {
            return n.depth;
        }
    }
    return error.ExhaustedLoop;
}

test "parsing depths" {
    const testing = std.testing;

    const text =
        \\# woo comment
        \\mp:10
        \\[[sy]]
        \\  # another
        \\  : n : "en"  ,  [[m]]
        \\    # more
        \\
        \\    # even more
        \\
        \\    "sc"   :  [[10]]   ,    g #inline
        \\  [[]]:[==[
        \\hi]==]
    ;
    var idx: usize = 0;
    var stream = StreamingParser.init();

    try testing.expectEqual(try testNextLevelOrError(&stream, &idx, text), 1);
    try testing.expectEqual(try testNextLevelOrError(&stream, &idx, text), 2);
    try testing.expectEqual(try testNextLevelOrError(&stream, &idx, text), 1);
    try testing.expectEqual(try testNextLevelOrError(&stream, &idx, text), 2);
    try testing.expectEqual(try testNextLevelOrError(&stream, &idx, text), 3);
    try testing.expectEqual(try testNextLevelOrError(&stream, &idx, text), 4);
    try testing.expectEqual(try testNextLevelOrError(&stream, &idx, text), 4);
    try testing.expectEqual(try testNextLevelOrError(&stream, &idx, text), 3);
    try testing.expectEqual(try testNextLevelOrError(&stream, &idx, text), 4);
    try testing.expectEqual(try testNextLevelOrError(&stream, &idx, text), 4);
    try testing.expectEqual(try testNextLevelOrError(&stream, &idx, text), 2);
    try testing.expectEqual(try testNextLevelOrError(&stream, &idx, text), 3);
}

/// Represents a node in a static tree. Nodes have a parent, child, and sibling pointer.
pub const ZNode = struct {
    const Self = @This();
    value: []const u8 = "",
    parent: ?*ZNode = null,
    sibling: ?*ZNode = null,
    child: ?*ZNode = null,

    /// Returns the next Node in the tree. Will return Null after reaching root. For nodes further
    /// down the tree, they will bubble up, resulting in a negative depth. Depth will be modified
    /// relative to self: 0 for siblings, -1 or less for parents, 1 for children.
    pub fn next(self: *const Self, depth: ?*isize) ?*ZNode {
        if (self.child) |c| {
            if (depth) |d| {
                d.* += 1;
            }
            return c;
        } else if (self.sibling) |c| {
            return c;
        } else {
            // Go up and forward.
            var iter: ?*const ZNode = self;
            while (iter != null) {
                iter = iter.?.parent;
                if (iter != null) {
                    if (depth) |d| {
                        d.* -= 1;
                    }
                    if (iter.?.sibling) |c| {
                        return c;
                    }
                }
            }
            return null;
        }
    }

    /// Iterates this node's children. Pass null to start. `iter = node.nextChild(iter);`
    pub fn nextChild(self: *const Self, iter: ?*const ZNode) ?*ZNode {
        if (iter) |it| {
            return it.sibling;
        } else {
            return self.child;
        }
    }

    /// Returns the next descendant. Pass null to start.
    pub fn nextDescendant(self: *const Self, iter: ?*const ZNode, depth: ?*isize) ?*ZNode {
        if (iter == null) {
            if (self.child) |c| {
                if (depth) |d| {
                    d.* += 1;
                }
                return c;
            }
        } else if (iter.?.child) |c| {
            if (depth) |d| {
                d.* += 1;
            }
            return c;
        } else if (iter.?.sibling) |sib| {
            return sib;
        } else {
            // Go up and forward.
            if (depth) |d| {
                d.* -= 1;
            }
            var it = iter.?.parent;
            while (it != null) {
                // Back.
                if (it == self) {
                    return null;
                }

                // Nope try going forward.
                if (it.?.sibling) |sib| {
                    return sib;
                } else {
                    if (depth) |d| {
                        d.* -= 1;
                    }
                    it = it.?.parent;
                }
            }
        }
        return null;
    }

    /// Returns the nth child. O(n)
    pub fn getNthChild(self: *const Self, nth: usize) ?*ZNode {
        var count: usize = 0;
        var iter: ?*ZNode = self.child;
        while (iter) |n| {
            if (count == nth) {
                return n;
            }
            count += 1;
            iter = n.sibling;
        }
        return null;
    }

    /// Returns the nth child's value. Or null if neither the node or child exist.
    pub fn getNthChildValue(self: *const Self, nth: usize) ?[]const u8 {
        if (self.getNthChild(nth)) |child| {
            return child.value;
        }
        return null;
    }

    /// Returns the number of children. O(n)
    pub fn getChildCount(self: *const Self) usize {
        var count: usize = 0;
        var iter: ?*ZNode = self.child;
        while (iter) |n| {
            count += 1;
            iter = n.sibling;
        }
        return count;
    }

    /// Finds the nth child node with a specific value.
    pub fn findNthChild(self: *const Self, nth: usize, value: []const u8) ?*ZNode {
        var count: usize = 0;
        var iter: ?*ZNode = self.child orelse return null;
        while (iter) |n| {
            if (mem.eql(u8, n.value, value)) {
                if (count == nth) {
                    return n;
                }
                count += 1;
            }
            iter = n.sibling;
        }
        return null;
    }

    /// Finds the first child with the specified value.
    pub fn findChild(self: *const Self, value: []const u8) ?*ZNode {
        return self.findNthChild(0, value);
    }

    /// Finds the next child after the given iterator. This is good for when you can guess the order
    /// of the nodes, which can cut down on starting from the beginning. Passing null starts over
    /// from the beginning. Returns the found node or null (it will loop back around).
    pub fn findNextChild(self: *const Self, start: ?*const ZNode, value: []const u8) ?*ZNode {
        var iter: ?*ZNode = self.child;
        if (start) |si| {
            iter = si.sibling;
        }
        while (iter != start) {
            if (iter) |it| {
                if (mem.eql(u8, it.value, value)) {
                    return it;
                }
                iter = it.sibling;
            } else {
                // Loop back.
                iter = self.child;
            }
        }
        return null;
    }

    /// Traverses descendants until a node with the specific value is found.
    pub fn findNthDescendant(self: *const Self, nth: usize, value: []const u8) ?*ZNode {
        var count: usize = 0;
        var iter: ?*const ZNode = null;
        while (self.nextDescendant(iter, null)) |n| : (iter = n) {
            if (mem.eql(u8, n.value, value)) {
                if (count == nth) {
                    return n;
                }
                count += 1;
            }
        }
        return null;
    }

    /// Traverses descendants until a node with the specific value is found.
    pub fn findDescendant(self: *const Self, value: []const u8) ?*ZNode {
        return self.findNthDescendant(0, value);
    }

    /// Returns true if node has more than one descendant (child, grandchild, etc).
    fn _moreThanOneDescendant(self: *const Self) bool {
        var count: usize = 0;
        var iter: ?*const ZNode = null;
        while (self.nextDescendant(iter, null)) |n| : (iter = n) {
            count += 1;
            if (count > 1) {
                return true;
            }
        }
        return false;
    }

    /// Outputs a `ZNode`s children on multiple lines. Excludes this node as root.
    fn stringify(self: *const Self, out_stream: anytype) @TypeOf(out_stream).Error!void {
        var depth: isize = 0;
        var last_depth: isize = 0;
        var iter: ?*const ZNode = null;
        while (self.nextDescendant(iter, &depth)) |n| : (iter = n) {
            // Special case for root.
            if (last_depth == 0) {
                last_depth = depth;
            } else if (depth > last_depth) {
                last_depth = depth;
                try out_stream.writeAll(":");
                // Likely an array.
                if (mem.eql(u8, n.parent.?.value, "")) {
                    try out_stream.writeAll(" ");
                } else if (n.parent.?._moreThanOneDescendant()) {
                    try out_stream.writeAll("\n");
                    try out_stream.writeByteNTimes(' ', 2 * @bitCast(usize, depth - 1));
                } else {
                    try out_stream.writeAll(" ");
                }
            } else if (depth < last_depth) {
                while (depth < last_depth) {
                    last_depth = depth;
                }
                try out_stream.writeAll("\n");
                try out_stream.writeByteNTimes(' ', 2 * @bitCast(usize, depth - 1));
            } else {
                try out_stream.writeAll("\n");
                try out_stream.writeByteNTimes(' ', 2 * @bitCast(usize, depth - 1));
            }

            var multiline_or_quote =
                mem.indexOf(u8, n.value, "\n") != null or
                mem.indexOf(u8, n.value, "\"") != null;
            var contains_control =
                mem.indexOf(u8, n.value, " ") != null or
                mem.indexOf(u8, n.value, "\t") != null or
                mem.indexOf(u8, n.value, ":") != null or
                mem.indexOf(u8, n.value, ",") != null or
                mem.indexOf(u8, n.value, ";") != null;
            // Handle empty strings without children.
            var empty_no_children = n.child == null and n.value.len == 0;
            if (multiline_or_quote) {
                try out_stream.writeAll("[[\n");
            } else if (contains_control or empty_no_children) {
                try out_stream.writeAll("\"");
            }
            try out_stream.writeAll(n.value);
            if (multiline_or_quote) {
                try out_stream.writeAll("]]");
            } else if (contains_control or empty_no_children) {
                try out_stream.writeAll("\"");
            }
        }
        try out_stream.writeAll("\n");
    }

    /// Stringifies to standard out.
    pub fn show(self: *const Self) void {
        self.stringify(std.io.getStdOut().writer()) catch {};
    }
};

pub const ZStaticTreeError = error{
    TreeFull,
} || StreamingParser.Error;

/// Represents a static tree.  Does not manage memory for strings, only references it.
pub fn ZStaticTree(comptime S: usize) type {
    return struct {
        const Self = @This();

        root: ZNode = ZNode{},
        nodes: [S]ZNode = [_]ZNode{.{}} ** S,
        node_count: usize = 0,

        /// Returns the number of nodes remaining in the tree.
        pub fn nodesRemaining(self: *const Self) usize {
            return S - self.node_count;
        }

        /// Returns a boolean indicating if nodes from text can fit within the tree.
        pub fn canAppend(self: *const Self, text: []const u8) bool {
            var count = countTextNodes(text) catch return false;
            return count <= self.nodesRemaining();
        }

        /// Adds a node to given a parent. Null parent uses the root.
        pub fn appendValue(self: *Self, parent: ?*ZNode, value: []const u8) ZStaticTreeError!*ZNode {
            if (self.node_count >= S) {
                return ZStaticTreeError.TreeFull;
            }
            var true_parent = if (parent == null) &self.root else parent;
            var node = &self.nodes[self.node_count];
            self.node_count += 1;
            node.value = value;
            node.parent = true_parent;
            node.sibling = null;
            node.child = null;
            // Add to end.
            if (true_parent) |p| {
                if (p.child) |child| {
                    var iter = child;
                    while (iter.sibling) |sib| : (iter = sib) {}
                    iter.sibling = node;
                } else {
                    p.child = node;
                }
            }

            return node;
        }

        /// Clears the entire tree.
        pub fn clear(self: *Self) void {
            self.node_count = 0;
            self.root.child = null;
        }
    };
}

test "error fills tree" {
    const testing = std.testing;

    var tree = ZStaticTree(6){};
    // Using 2 nodes.
    try appendText(&tree, null, "foo:bar");
    try testing.expectEqual(@as(usize, 2), tree.node_count);
    try testing.expectError(ZStaticTreeError.TreeFull, appendText(&tree, null, "bar:foo:baz:ha:ha"));
    try testing.expectEqual(@as(usize, 6), tree.node_count);
}

test "static tree" {
    const testing = std.testing;
    const text =
        \\max_particles: 100
        \\texture: circle
        \\en: Foo
        \\systems:
        \\  : name:Emitter
        \\    params:
        \\      some,stuff,hehe
        \\  : name:Fire
    ;

    var tree = ZStaticTree(100){};
    try appendText(&tree, null, text);

    var iter = tree.root.findNextChild(null, "max_particles");
    try testing.expect(iter != null);
    iter = tree.root.findNextChild(iter, "texture");
    try testing.expect(iter != null);
    iter = tree.root.findNextChild(iter, "max_particles");
    try testing.expect(iter != null);
    iter = tree.root.findNextChild(iter, "systems");
    try testing.expect(iter != null);
    iter = tree.root.findNextChild(iter, "42");
    try testing.expect(iter == null);
}

test "node appending and searching" {
    const testing = std.testing;

    var tree = ZStaticTree(100){};
    var root = try tree.appendValue(null, "");

    _ = try tree.appendValue(root, "");
    _ = try tree.appendValue(root, "Hello");
    _ = try tree.appendValue(root, "foo");
    _ = try tree.appendValue(root, "42");
    _ = try tree.appendValue(root, "3.14");
    _ = try tree.appendValue(root, "true");

    try testing.expectEqual(@as(usize, 6), root.getChildCount());
    try testing.expect(root.findNthChild(0, "") != null);

    try testing.expect(root.findNthChild(0, "Hello") != null);
    try testing.expect(root.findNthChild(0, "foo") != null);
    try testing.expect(root.findNthChild(1, "Hello") == null);
    try testing.expect(root.findNthChild(1, "foo") == null);

    try testing.expect(root.findNthChild(0, "42") != null);
    try testing.expect(root.findNthChild(0, "41") == null);
    try testing.expect(root.findNthChild(1, "42") == null);

    try testing.expect(root.findNthChild(0, "3.14") != null);
    try testing.expect(root.findNthChild(0, "3.13") == null);
    try testing.expect(root.findNthChild(1, "3.14") == null);

    try testing.expect(root.findNthChild(0, "true") != null);
    try testing.expect(root.findNthChild(1, "true") == null);
}

test "appending node" {
    const testing = std.testing;

    var tree0 = ZStaticTree(8){};
    var tree1 = ZStaticTree(8){};

    try appendText(&tree0, null, "foo:bar");
    tree1.root.value = "ROOT";
    try appendText(&tree1, null, "biz:baz");

    var new_root = try copyNode(&tree0, null, &tree1.root);

    try testing.expectEqualSlices(u8, "ROOT", new_root.value);

    var root = &tree0.root;
    try testing.expectEqualSlices(u8, "foo", root.child.?.value);
    try testing.expectEqualSlices(u8, "bar", root.child.?.child.?.value);
    try testing.expectEqualSlices(u8, "ROOT", root.child.?.sibling.?.value);
    try testing.expectEqualSlices(u8, "biz", root.child.?.sibling.?.child.?.value);
    try testing.expectEqualSlices(u8, "baz", root.child.?.sibling.?.child.?.child.?.value);
}

pub const ZDynamicTreeError = error{
    OutOfMemory,
} || StreamingParser.Error;

pub const ZDynamicTree = struct {
    const Self = @This();

    arena: std.heap.ArenaAllocator,
    root: ZNode = ZNode{},
    node_count: usize = 0,

    pub fn init(allocator: std.mem.Allocator) Self {
        return Self{ .arena = std.heap.ArenaAllocator.init(allocator) };
    }

    pub fn deinit(self: *Self) void {
        self.arena.deinit();
    }

    /// Appends a raw value allocated with this struct's arena allocator.
    pub fn appendRawValue(self: *Self, parent: ?*ZNode, value: []const u8) ZDynamicTreeError!*ZNode {
        var true_parent = if (parent == null) &self.root else parent.?;
        var node = try self.arena.allocator().create(ZNode);
        self.node_count += 1;
        node.value = value;
        node.parent = true_parent;
        node.sibling = null;
        node.child = null;
        // Add to end.
        if (true_parent.child) |child| {
            var iter = child;
            while (iter.sibling) |sib| : (iter = sib) {}
            iter.sibling = node;
        } else {
            true_parent.child = node;
        }
        return node;
    }

    /// Appends a value, duping it.
    pub fn appendValue(self: *Self, parent: ?*ZNode, value: []const u8) ZDynamicTreeError!*ZNode {
        var duped = try self.arena.allocator().dupe(u8, value);
        errdefer self.arena.allocator().free(duped);
        return self.appendRawValue(parent, duped);
    }

    /// Appends anytype to the tree by converting it to a string with allocPrint.
    pub fn appendAnytype(self: *Self, parent: ?*ZNode, value: anytype) ZDynamicTreeError!*ZNode {
        return self.appendPrint(parent, "{any}", .{value});
    }

    /// Appends anytype to the tree by converting it to a string with allocPrint.
    pub fn appendPrint(self: *Self, parent: ?*ZNode, comptime fmt: []const u8, value: anytype) ZDynamicTreeError!*ZNode {
        var string = try std.fmt.allocPrint(self.arena.allocator(), fmt, value);
        errdefer self.arena.allocator().free(string);
        return self.appendRawValue(parent, string);
    }
};

const ZError = ZDynamicTreeError || ZStaticTreeError;

/// Adds text under a parent node.  Passing null will put the text under root.
pub fn appendText(tree: anytype, parent: ?*ZNode, text: []const u8) ZError!void {
    const tree_type = @TypeOf(tree);
    const tree_type_info = @typeInfo(tree_type);
    if (tree_type_info != .Pointer) {
        @compileError("copyNode expects a zzz tree pointer");
    }

    var current: *ZNode = if (parent == null) &tree.root else parent.?;
    var current_depth: usize = 0;

    var stream = StreamingParser.init();
    var idx: usize = 0;
    while (try stream.parse(&idx, text)) |token| {
        const slice = text[token.start..token.end];
        const value = if (slice.len == 0) "" else slice;
        const new_depth = token.depth;
        if (new_depth <= current_depth) {
            // Ascend.
            while (current_depth > new_depth) {
                current = current.parent orelse unreachable;
                current_depth -= 1;
            }
            // Sibling.
            const new = try tree.appendValue(current.parent, value);
            current.sibling = new;
            current = new;
        } else if (new_depth == current_depth + 1) {
            // Descend.
            current_depth += 1;
            const new = try tree.appendValue(current, value);
            current.child = new;
            current = new;
        } else {
            // Levels shouldn't increase by more than one.
            unreachable;
        }
    }

    try stream.completeOrError();
}

/// Copies a node under another parent.  Does not check if there are overlaps, like copying a
/// parent to be under a child.
pub fn copyNode(tree: anytype, parent: ?*ZNode, node: *const ZNode) ZError!*ZNode {
    const tree_type = @TypeOf(tree);
    const tree_type_info = @typeInfo(tree_type);
    if (tree_type_info != .Pointer) {
        @compileError("copyNode expects a zzz tree pointer");
    }

    var new_root = try tree.appendValue(parent, node.value);

    // Starts at 1 for direct children so we iterate siblings.
    var last_depth: isize = 1;
    var depth: isize = 0;
    var iter = node;
    var piter: ?*ZNode = new_root;
    var plast: ?*ZNode = null;
    while (iter.next(&depth)) |next| : (iter = next) {
        // If depth comes back to 0 or less, we're siblings or parent of node.
        if (depth <= 0) {
            break;
        } else if (depth > last_depth) {
            piter = plast;
            last_depth = depth;
        } else if (depth < last_depth) {
            plast = piter;
            while (last_depth != depth) {
                piter = piter.?.parent;
                last_depth -= 1;
            }
        } else {
            // Sibling, keep using current parent (piter).
        }
        plast = try tree.appendValue(piter, next.value);
    }
    return new_root;
}

/// Parses and counts the number of nodes in a text.
pub fn countTextNodes(text: []const u8) !usize {
    var count: usize = 0;

    var stream = StreamingParser.init();
    var idx: usize = 0;

    while (try stream.parse(&idx, text)) |_| {
        count += 1;
    }

    try stream.completeOrError();

    return count;
}

test "dynamic tree" {
    const testing = std.testing;

    var tree0 = ZDynamicTree.init(testing.allocator);
    defer tree0.deinit();

    try appendText(&tree0, null,
        \\arbitrary:data:that:can:be:as:long:as:memory:can:hold
    );

    _ = try tree0.appendAnytype(null, 42);

    try testing.expectEqual(@as(usize, 12), tree0.node_count);
}

/// Tries to constuct an instance of given type by parsing a tree node
/// slices (i.e. strings) are copied, free the result with `zzz.free`
pub fn parseTreeAlloc(comptime T: type, allocator: std.mem.Allocator, node: ZNode) !T {
    var res: T = undefined;
    switch (T) {
        []const u8 => {
            if (node.child) |value_node| {
                res = try allocator.dupe(u8, value_node.value);
            } else {
                res = try allocator.dupe(u8, node.value);
            }
        },
        else => switch (@typeInfo(T)) {
            .Struct => |s_info| {
                var child_iter: ?*ZNode = null;
                while (node.nextChild(child_iter)) |child| : (child_iter = child) {
                    inline for (s_info.fields) |field| {
                        if (std.mem.eql(u8, field.name, child.value)) {
                            const field_value = try parseTreeAlloc(field.field_type, allocator, child.*);
                            @field(res, field.name) = field_value;
                        }
                    }
                }
            },
            .Pointer => |p_info| switch (p_info.size) {
                .Slice => {
                    const count = node.getChildCount();

                    res = try allocator.alloc(p_info.child, count);

                    var i: usize = 0;
                    var child_iter: ?*ZNode = null;
                    while (node.nextChild(child_iter)) |child| : (child_iter = child) {
                        res[i] = try parseTreeAlloc(p_info.child, allocator, child.*);
                        i += 1;
                    }
                },
                else => @compileError("Unsupported type " ++ @typeName(T)),
            },
            else => @compileError("Unsupported type " ++ @typeName(T)),
        },
    }
    return res;
}

/// Frees memory allocated by parseTreeAlloc
pub fn free(allocator: std.mem.Allocator, value: anytype) void {
    const T = @TypeOf(value);
    switch (T) {
        []const u8 => {
            allocator.free(value);
        },
        else => switch (@typeInfo(T)) {
            .Struct => |s_info| {
                inline for (s_info.fields) |field| {
                    free(allocator, @field(value, field.name));
                }
            },
            .Pointer => |p_info| switch (p_info.size) {
                .Slice => {
                    for (value) |elem| free(allocator, elem);
                    allocator.free(value);
                },
                else => @compileError("Unsupported type " ++ @typeName(T)),
            },
            else => @compileError("Unsupported type " ++ @typeName(T)),
        },
    }
}

test "parseTreeAlloc/free" {
    const testing = std.testing;

    var test_tree = ZStaticTree(1000){};
    try appendText(
        &test_tree,
        null,
        \\a: "testing, testing..."
        \\bs:
        \\  1
        \\  2
        \\  3
        \\c:
        \\  foo: "foo'd"
        \\  bar: "bar'd"
        \\  baz: "baz'd"
        \\
        ,
    );

    const foo = try parseTreeAlloc(
        struct {
            a: []const u8,
            bs: [][]const u8,
            c: struct{
                foo: []const u8,
                bar: []const u8,
                baz: []const u8,
            },
        },
        testing.allocator,
        test_tree.root,
    );
    defer free(testing.allocator, foo);

    try testing.expectEqualSlices(u8, "testing, testing...", foo.a);

    try testing.expect(foo.bs.len == 3);
    try testing.expectEqualSlices(u8, "1", foo.bs[0]);
    try testing.expectEqualSlices(u8, "2", foo.bs[1]);
    try testing.expectEqualSlices(u8, "3", foo.bs[2]);

    try testing.expectEqualSlices(u8, "foo'd", foo.c.foo);
    try testing.expectEqualSlices(u8, "bar'd", foo.c.bar);
    try testing.expectEqualSlices(u8, "baz'd", foo.c.baz);
}
