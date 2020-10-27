//! zzz format serializer and deserializer. public domain.
//!
//! StreamingParser inspired by Zig's JSON parser.
//!
//! SPARSE SPEC
//! (zzz text is escaped using Zig's multiline string: \\)
//!
//! zzz text describes a tree of strings. Special characters (and spaces) are used to go up and down
//! the tree. The tree has an implicit null root node.
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
//! \\a nodetend
//! \\  # Another comment
//! \\  a sibling
//! Output:
//! null -> "a node"
//!      -> "a sibling"

const std = @import("std");

/// The only output of the tokenizer.
pub const ZNodeToken = struct {
    const Self = @This();
    /// 0 is top level children.
    /// TODO: This is different from other interfaces which take an isize, and start from 1, 0
    /// being root.
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

    pub const Error = error {
        TooMuchIndentation,
        InvalidWhitespace,
        OddIndentationValue,
        InvalidQuotation,
        InvalidMultilineOpen,
        InvalidMultilineClose,
        InvalidNewLineInString,
        InvalidCharacterAfterString,
        SemicolonWentPastRoot,
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

    pub fn hasCompleted(self: *const Self) bool {
        switch (self.state) {
            .ExpectZNode, .OpenLine, .EndString, .Comment, .OpenComment, .Indent => return true,
            else => return false,
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
                }
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
                                .depth = self.line_depth + self.node_depth,
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
                        .depth = self.line_depth + self.node_depth,
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
                        .depth = self.line_depth + self.node_depth,
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
                        .depth = self.line_depth + self.node_depth,
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
                        .depth = self.line_depth + self.node_depth,
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
                }
            },
            .Indent => switch (c) {
                ' ' => {
                    self.start_index = self.current_index + 1;
                    self.line_depth += 1;
                    self.state = .OpenLine;
                },
                else => {
                    return Error.OddIndentationValue;
                }
            },
            .Quotation => switch (c) {
                '"' => {
                    self.state = .EndString;
                    const node = ZNodeToken{
                        .depth = self.line_depth + self.node_depth,
                        .start = self.start_index,
                        .end = self.current_index,
                    };
                    // Reset because we're going to expecting nodes.
                    self.start_index = self.current_index + 1;
                    return node;
                },
                else => {
                    self.state = .SingleLineCharacter;
                }
            },
            .SingleLineCharacter => switch (c) {
                '"' => {
                    self.state = .EndString;
                    const node = ZNodeToken{
                        .depth = self.line_depth + self.node_depth,
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
                }
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
                }
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
                }
            },
            .MultilineCharacter => switch (c) {
                ']' => {
                    self.close_string_level = 0;
                    self.state = .MultilineClose0;
                },
                else => {
                    // Capture EVERYTHING.
                }
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
                            .depth = self.line_depth + self.node_depth,
                            .start = self.start_index,
                            .end = self.current_index - self.open_string_level - 1,
                        };
                    }
                    self.state = .MultilineCharacter;
                },
                else => {
                    return Error.InvalidMultilineClose;
                }
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
    testing.expectEqualSlices(u8, "mp", try testNextTextOrError(&stream, &idx, text));
    testing.expectEqualSlices(u8, "10", try testNextTextOrError(&stream, &idx, text));
    testing.expectEqualSlices(u8, "sy", try testNextTextOrError(&stream, &idx, text));
    testing.expectEqualSlices(u8, "", try testNextTextOrError(&stream, &idx, text));
    testing.expectEqualSlices(u8, "n", try testNextTextOrError(&stream, &idx, text));
    testing.expectEqualSlices(u8, "en", try testNextTextOrError(&stream, &idx, text));
    testing.expectEqualSlices(u8, "m", try testNextTextOrError(&stream, &idx, text));
    testing.expectEqualSlices(u8, "sc", try testNextTextOrError(&stream, &idx, text));
    testing.expectEqualSlices(u8, "10", try testNextTextOrError(&stream, &idx, text));
    testing.expectEqualSlices(u8, "g", try testNextTextOrError(&stream, &idx, text));
    testing.expectEqualSlices(u8, "", try testNextTextOrError(&stream, &idx, text));
    testing.expectEqualSlices(u8, "hi", try testNextTextOrError(&stream, &idx, text));
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

    testing.expectEqual(try testNextLevelOrError(&stream, &idx, text), 0);
    testing.expectEqual(try testNextLevelOrError(&stream, &idx, text), 1);
    testing.expectEqual(try testNextLevelOrError(&stream, &idx, text), 0);
    testing.expectEqual(try testNextLevelOrError(&stream, &idx, text), 1);
    testing.expectEqual(try testNextLevelOrError(&stream, &idx, text), 2);
    testing.expectEqual(try testNextLevelOrError(&stream, &idx, text), 3);
    testing.expectEqual(try testNextLevelOrError(&stream, &idx, text), 3);
    testing.expectEqual(try testNextLevelOrError(&stream, &idx, text), 2);
    testing.expectEqual(try testNextLevelOrError(&stream, &idx, text), 3);
    testing.expectEqual(try testNextLevelOrError(&stream, &idx, text), 3);
    testing.expectEqual(try testNextLevelOrError(&stream, &idx, text), 1);
    testing.expectEqual(try testNextLevelOrError(&stream, &idx, text), 2);
}

/// Parses the stream, outputting ZNodeTokens which reference the text.
pub fn parseStream(stream: *StreamingParser, idx: *usize, text: []const u8) !?ZNodeToken {
    while (idx.* <= text.len) {
        // Insert an extra newline at the end of the stream.
        const node = if (idx.* == text.len) try stream.feed('\n') else try stream.feed(text[idx.*]);
        idx.* += 1;
        if (node) |n| {
            return n;
        }
    }
    return null;
}

/// A `ZNode`'s value.
pub const ZValue = union(enum) {
    const Self = @This();
    Null,
    String: []const u8,
    Int: i32,
    Float: f32,
    Bool: bool,

    /// Checks a ZValues equality.
    pub fn equals(self: Self, other: Self) bool {
        if (self == .Null and other == .Null) {
            return true;
        }
        if (self == .String and other == .String) {
            return std.mem.eql(u8, self.String, other.String);
        }
        if (self == .Int and other == .Int) {
            return self.Int == other.Int;
        }
        if (self == .Float and other == .Float) {
            return std.math.approxEq(f32, self.Float, other.Float, std.math.f32_epsilon);
        }
        if (self == .Bool and other == .Bool) {
            return self.Bool == other.Bool;
        }
        return false;
    }

    /// Outputs a value to the `out_stream`. This output is a parsable.
    pub fn stringify(self: Self, out_stream: anytype) @TypeOf(out_stream).Error!void {
        switch (self) {
            .Null => {
                // Skip.
            },
            .String => {
                const find = std.mem.indexOfScalar;
                const chars = "\"\n\t\r,:;";
                const chars_count = @sizeOf(@TypeOf(chars));
                var need_escape = false;
                var found = [_]bool{false} ** chars_count;
                var level: usize = 0;
                for ("\"\n\t\r,:;") |ch, i| {
                    const f = find(u8, self.String, ch);
                    if (f != null) {
                        found[i] = true;
                        need_escape = true;
                    }
                }
                // TODO: Escaping ]] in string.
                if (need_escape) {
                    // 0=" 1=\n
                    if (found[0] or found[1]) {
                        // Escape with Lua.
                        try out_stream.writeAll("[[");
                        const ret = try out_stream.writeAll(self.String);
                        try out_stream.writeAll("]]");
                        return ret;
                    } else {
                        // Escape with basic quotes.
                        try out_stream.writeAll("\"");
                        const ret = try out_stream.writeAll(self.String);
                        try out_stream.writeAll("\"");
                        return ret;
                    }
                }
                return try out_stream.writeAll(self.String);
            },
            .Int => {
                return std.fmt.formatIntValue(self.Int, "", std.fmt.FormatOptions{}, out_stream);
            },
            .Float => {
                return std.fmt.formatFloatScientific(self.Float, std.fmt.FormatOptions{}, out_stream);
            },
            .Bool => {
                return out_stream.writeAll(if (self.Bool) "true" else "false");
            }
        }
    }

    ///
    pub fn format(self: Self, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .Null => try std.fmt.format(writer, ".Null", .{}),
            .String => try std.fmt.format(writer, ".String({})", .{self.String}),
            .Int => try std.fmt.format(writer, ".Int({})", .{self.Int}),
            .Float => try std.fmt.format(writer, ".Float({})", .{self.Float}),
            .Bool => try std.fmt.format(writer, ".Bool({})", .{self.Bool}),
        }
    }
};

/// Transformer function. Level is tree depth, so top depth nodes have a depth of 1.
pub fn defaultTransformer(context: void, value: ZValue, depth: isize) anyerror!ZValue {
    if (value != .String) {
        return value;
    }
    // Try to cast to numbers, then true/false checks, then string.
    const slice = value.String;
    const integer = std.fmt.parseInt(i32, slice, 10) catch |_| {
        const float = std.fmt.parseFloat(f32, slice) catch |_| {
            if (std.mem.eql(u8, "true", slice)) {
                return ZValue{.Bool = true};
            } else if (std.mem.eql(u8, "false", slice)) {
                return ZValue{.Bool = false};
            }
            return value;
        };
        return ZValue{.Float = float};
    };
    return ZValue{.Int = integer};
}

/// ZTree errors.
pub const ZError = error {
    TreeFull,
    TooManyRoots,
};

/// Represents a node in a static tree. Nodes have a parent, child, and sibling pointer
/// to a spot in the array.
pub const ZNode = struct {
    const Self = @This();
    value: ZValue = .Null,
    parent: ?*ZNode = null,
    sibling: ?*ZNode = null,
    child: ?*ZNode = null,

    /// Returns the next Node in the tree. Will return Null after reaching root. For nodes further
    /// down the tree, they will bubble up, resulting in a negative depth.
    pub fn next(self: *const Self, depth: *isize) ?*ZNode {
        if (self.child) |c| {
            depth.* += 1;
            return c;
        } else if (self.sibling) |c| {
            return c;
        } else {
            // Go up and forward.
            var iter: ?*const ZNode = self;
            while (iter != null) {
                iter = iter.?.parent;
                if (iter != null) {
                    depth.* -= 1;
                    if (iter.?.sibling) |c| {
                        return c;
                    }
                }
            }
            return null;
        }
    }

    /// Returns the next node in the tree until reaching root or the stopper node.
    pub fn nextUntil(self: *const Self, stopper: *const ZNode, depth: *isize) ?*ZNode {
        if (self.child) |c| {
            if (c == stopper) { return null; }
            depth.* += 1;
            return c;
        } else if (self.sibling) |c| {
            if (c == stopper) { return null; }
            return c;
        } else {
            // Go up and forward.
            var iter: ?*const ZNode = self;
            while (iter != null) {
                iter = iter.?.parent;
                // All these checks. :/
                if (iter == stopper) { return null; }
                if (iter != null) {
                    depth.* -= 1;
                    if (iter.?.sibling) |c| {
                        if (c == stopper) { return null; }
                        return c;
                    }
                }
            }
            return null;
        }
    }

    /// Iterates this node's children.
    pub fn nextChild(self: *const Self, iter: ?*const ZNode) ?*ZNode {
        if (iter) |it| {
            return it.sibling;
        } else {
            return self.child;
        }
    }

    /// Returns the nth child's value. Or null if neither exist.
    pub fn getChildValue(self: *const Self, nth: usize) ?Value {
        var count: usize = 0;
        var iter: ?*ZNode = self.child;
        while (iter) |n| {
            if (count == nth) {
                if (n.child) |c| {
                    return c.value;
                } else {
                    return null;
                }
            }
            count += 1;
            iter = n.sibling;
        }
        return null;
    }

    /// Returns the nth child.
    pub fn getChild(self: *const Self, nth: usize) ?*ZNode {
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

    pub fn getChildCount(self: *const Self) usize {
        var count: usize = 0;
        var iter: ?*ZNode = self.child;
        while (iter) |n| {
            count += 1;
            iter = n.sibling;
        }
        return count;
    }

    /// Finds the next child after the given iterator. This is good for when you can guess the order
    /// of the nodes, which can cut down on starting from the beginning. Passing null starts over
    /// from the beginning. Returns the found node or null (it will loop back around).
    pub fn findNext(self: *const Self, start: ?*const ZNode, value: ZValue) ?*ZNode {
        var iter: ?*ZNode = self.child;
        if (start) |si| {
            iter = si.sibling;
        }
        while (iter != start) {
            if (iter) |it| {
                if (it.value.equals(value)) {
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

    /// Finds the nth child node with a specific tag.
    pub fn findNthAny(self: *const Self, nth: usize, tag: @TagType(ZValue)) ?*ZNode {
        var count: usize = 0;
        var iter: ?*ZNode = self.child;
        while (iter) |n| {
            if (n.value == tag) {
                if (count == nth) {
                    return n;
                }
                count += 1;
            }
            iter = n.sibling;
        }
        return null;
    }

    /// Finds the nth child node with a specific value.
    pub fn findNth(self: *const Self, nth: usize, value: ZValue) ?*ZNode {
        var count: usize = 0;
        var iter: ?*ZNode = self.child orelse return null;
        while (iter) |n| {
            if (n.value.equals(value)) {
                if (count == nth) {
                    return n;
                }
                count += 1;
            }
            iter = n.sibling;
        }
        return null;
    }

    /// Traverses descendants until a node with the tag is found.
    pub fn findNthAnyDescendant(self: *const Self, nth: usize, value: @TagType(ZValue)) ?*ZNode {
        var depth: isize = 0;
        var count: usize = 0;
        var iter: *ZNode = self;
        while (iter.nextUntil(self, &depth)) |n| : (iter = n) {
            if (n.value == tag) {
                if (count == nth) {
                    return n;
                }
                count += 1;
            }
        }
        return null;
    }

    /// Traverses descendants until a node with the specific value is found.
    pub fn findNthDescendant(self: *const Self, nth: usize, value: ZValue) ?*ZNode {
        var depth: isize = 0;
        var count: usize = 0;
        var iter: *ZNode = self;
        while (iter.nextUntil(self, &depth)) |n| : (iter = n) {
            if (n.value.equals(value)) {
                if (count == nth) {
                    return n;
                }
                count += 1;
            }
        }
        return null;
    }

    /// Iteratively transforms node values. Can pass a context, like an allocator. This can be used
    /// free resources too.
    pub fn transform(self: *Self, comptime C: type, context: C, transformer: fn(C, ZValue, isize) anyerror!ZValue) anyerror!void {
        var depth: isize = 0;
        var iter: *const ZNode = self;
        while (iter.nextUntil(self, &depth)) |c| : (iter = c) {
            c.value = try transformer(context, c.value, depth);
        }
    }

    /// Iteratively traverses the tree, passing the node.
    pub fn traverse(self: *Self, comptime C: type, context: C, traverser: fn(C, *ZNode, isize) anyerror!void) anyerror!void {
        var depth: isize = 0;
        var iter: *const ZNode = self;
        while (iter.nextUntil(self, &depth)) |c| : (iter = c) {
            try traverser(context, c, depth);
        }
    }

    /// Outputs a `ZNode` and its children on a single line. This can be parsed back.
    pub fn stringify(self: *const Self, out_stream: anytype) @TypeOf(out_stream).Error!void {
        // Likely not root.
        if (self.value != .Null) {
            try self.value.stringify(out_stream);
            try out_stream.writeAll(":");
        }
        var depth: isize = 0;
        var last_depth: isize = 1;
        var iter = self;
        while (iter.nextUntil(self, &depth)) |n| : (iter = n) {
            if (depth > last_depth) {
                last_depth = depth;
                try out_stream.writeAll(":");
            } else if (depth < last_depth) {
                while (depth < last_depth) {
                    try out_stream.writeAll(";");
                    last_depth -= 1;
                }
            } else if (depth > 1) {
                try out_stream.writeAll(",");
            }
            try n.value.stringify(out_stream);
        }
    }

    fn _moreThanOneDescendant(self: *const Self) bool {
        var depth: isize = 0;
        var count: usize = 0;
        var iter: *const ZNode = self;
        while (iter.nextUntil(self, &depth)) |n| : (iter = n) {
            count += 1;
            if (count > 1) {
                return true;
            }
        }
        return false;
    }

    fn _stringifyPretty(self: *const Self, out_stream: anytype) @TypeOf(out_stream).Error!void {
        //
        try self.value.stringify(out_stream);
        try out_stream.writeAll(":");
        if (self.getChildCount() > 1) {
            try out_stream.writeAll("\n  ");
        }
        var depth: isize = 0;
        var last_depth: isize = 1;
        var iter = self;
        while (iter.nextUntil(self, &depth)) |n| : (iter = n) {
            if (depth > last_depth) {
                last_depth = depth;
                try out_stream.writeAll(":");
                // Likely an array.
                if (n.parent.?.value == .Null) {
                    try out_stream.writeAll(" ");
                } else if (n.parent.?._moreThanOneDescendant()) {
                    try out_stream.writeAll("\n");
                    var i: isize = 0;
                    while (i < depth) : (i += 1) {
                        std.debug.print("  ", .{});
                    }
                } else {
                    try out_stream.writeAll(" ");
                }
            } else if (depth < last_depth) {
                while (depth < last_depth) {
                    last_depth -= 1;
                }
                try out_stream.writeAll("\n");
                var i: isize = 0;
                while (i < depth) : (i += 1) {
                    std.debug.print("  ", .{});
                }
            } else if (depth > 1) {
                try out_stream.writeAll(",");
            }
            try n.value.stringify(out_stream);
        }
    }

    /// Outputs a `ZNode` and its children on multiple lines.
    /// Arrays with children that have:
    /// - null elements, separate lines
    /// - non-null same line
    pub fn stringifyPretty(self: *const Self, out_stream: anytype) @TypeOf(out_stream).Error!void {
        // Assume root, so don't print this node.
        var iter: ?*const ZNode = self.child;
        while (iter) |n| {
            try n._stringifyPretty(out_stream);
            try out_stream.writeAll("\n");
            iter = n.sibling;
        }
    }

    pub fn show(self: *const Self) void {
        std.debug.print("{}\n", .{self.value});
        var depth: isize = 0;
        var iter: *const ZNode = self;
        while (iter.nextUntil(self, &depth)) |c| : (iter = c) {
            var i: isize = 0;
            while (i < depth) : (i += 1) {
                std.debug.print("  ", .{});
            }
            std.debug.print("{}\n", .{c.value});
        }
    }
};

/// Represents a static fixed-size zzz tree. Values are slices over the text passed.
pub fn ZTree(comptime R: usize, comptime S: usize) type {
    return struct {
        const Self = @This();
        roots: [R]*ZNode = undefined,
        root_count: usize = 0,
        nodes: [S]ZNode = [_]ZNode{.{}} ** S,
        node_count: usize = 0,

        /// Appends correct zzz text to the tree, creating a new root.
        pub fn appendText(self: *Self, text: []const u8) !*ZNode {
            var root = try self.addNode(null, .Null);
            var current = root;
            var current_depth: usize = 0;

            var stream = StreamingParser.init();
            var idx: usize = 0;
            while (try parseStream(&stream, &idx, text)) |token| {
                const slice = text[token.start..token.end];
                const value: ZValue = if (slice.len == 0) .Null else .{.String = slice};
                // Math works better with depth starting at one.
                const new_depth = token.depth + 1;
                if (new_depth <= current_depth) {
                    // Ascend.
                    while (current_depth > new_depth) {
                        current = current.parent orelse unreachable;
                        current_depth -= 1;
                    }
                    // Sibling.
                    const new = try self.addNode(current.parent, value);
                    current.sibling = new;
                    current = new;
                } else if (new_depth == current_depth + 1) {
                    // Descend.
                    current_depth += 1;
                    const new = try self.addNode(current, value);
                    current.child = new;
                    current = new;
                } else {
                    // Levels shouldn't increase by more than one.
                    unreachable;
                }
            }

            if (!stream.hasCompleted()) {
                return error.UnfinishedString;
            }

            return root;
        }

        /// Clears the entire tree.
        pub fn clear(self: *Self) void {
            self.root_count = 0;
            self.node_count = 0;
        }

        /// Returns a slice of active roots.
        pub fn rootSlice(self: *const Self) []const *ZNode {
            return self.roots[0..self.root_count];
        }

        /// Adds a node given a parent. Null parent starts a new root.
        pub fn addNode(self: *Self, parent: ?*ZNode, value: ZValue) ZError!*ZNode {
            if (self.node_count >= S) {
                return ZError.TreeFull;
            }
            var node = &self.nodes[self.node_count];
            self.node_count += 1;
            if (parent == null) {
                if (self.root_count >= R) {
                    return ZError.TooManyRoots;
                }
                self.roots[self.root_count] = node;
                self.root_count += 1;
            }
            node.value = value;
            node.parent = parent;
            node.sibling = null;
            node.child = null;
            // Add to end.
            if (parent) |p| {
                if (p.child) |child| {
                    var iter = child;
                    while (iter.sibling) |sib| : (iter = sib) { }
                    iter.sibling = node;
                } else {
                    p.child = node;
                }
            }
            return node;
        }

        pub fn show(self: *const Self) void {
            for (self.rootSlice()) |rt, i| {
                rt.show();
            }
        }
    };
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

    var tree = ZTree(1, 100){};
    const node = try tree.appendText(text);
    try node.transform(void, {}, defaultTransformer);

    var iter = node.findNext(null, .{.String = "max_particles"});
    testing.expect(iter != null);
    iter = node.findNext(iter, .{.String = "texture"});
    testing.expect(iter != null);
    iter = node.findNext(iter, .{.String = "max_particles"});
    testing.expect(iter != null);
    iter = node.findNext(iter, .{.String = "systems"});
    testing.expect(iter != null);
    iter = node.findNext(iter, .{.Int = 42});
    testing.expect(iter == null);
}

test "node conforming imprint" {
    const testing = std.testing;

    const ConformingEnum = enum {
        Foo,
    };

    const ConformingSubStruct = struct {
        name: []const u8 = "default",
        params: *const ZNode = undefined,
    };

    const ConformingStruct = struct {
        max_particles: ?i32 = undefined,
        texture: []const u8 = "default",
        systems: [20]?ConformingSubStruct = [_]?ConformingSubStruct{null} ** 20,
        en: ?ConformingEnum = null,
        exists: ?void = null,
    };

    const text =
        \\max_particles: 100
        \\texture: circle
        \\en: Foo
        \\systems:
        \\  : name:Emitter
        \\    params:
        \\      some,stuff,hehe
        \\  : name:Fire
        \\    params
        \\exists: anything here
    ;
    var tree = ZTree(1, 100){};
    var node = try tree.appendText(text);
    try node.transform(void, {}, defaultTransformer);

    var example = ConformingStruct{};
    try imprint(node, ImprintOptions{
        .ensure_node_exists = true, .ensure_value_exists = true,
        .ensure_correct_value_type = true,
    }, &example);
    testing.expectEqual(@as(i32, 100), example.max_particles.?);
    testing.expectEqualSlices(u8, "circle", example.texture);
    testing.expect(null != example.systems[0]);
    testing.expect(null != example.systems[1]);
    testing.expectEqual(@as(?ConformingSubStruct, null), example.systems[2]);
    testing.expectEqual(ConformingEnum.Foo, example.en.?);
    testing.expectEqualSlices(u8, "params", example.systems[0].?.params.value.String);
}

test "node nonconforming imprint" {
    const testing = std.testing;

    const NonConformingStruct = struct {
        max_particles: bool = undefined,
        no_exist: bool = undefined,
    };

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
    var tree = ZTree(1, 100){};
    var node = try tree.appendText(text);
    try node.transform(void, {}, defaultTransformer);

    var example = NonConformingStruct{};
    try imprint(node, ImprintOptions{.ensure_correct_value_type = false}, &example);
    testing.expectError(error.NodeDoesNotExist, imprint(node, ImprintOptions{.ensure_node_exists = true, .ensure_correct_value_type = false}, &example));
}

test "node appending and searching" {
    const testing = std.testing;


    var tree = ZTree(1, 100){};
    var root = try tree.addNode(null, .Null);

    var nullChild = try tree.addNode(root, .Null);
    var stringChild = try tree.addNode(root, .{.String = "Hello"});
    var fooChild = try tree.addNode(root, .{.String = "foo"});
    var integerChild = try tree.addNode(root, .{.Int = 42});
    var floatChild = try tree.addNode(root, .{.Float = 3.14});
    var boolChild = try tree.addNode(root, .{.Bool = true});

    testing.expectEqual(@as(usize, 6), root.getChildCount());
    testing.expect(root.findNth(0, .Null) != null);

    testing.expect(root.findNth(0, .{.String = "Hello"}) != null);
    testing.expect(root.findNth(0, .{.String = "foo"}) != null);
    testing.expect(root.findNth(1, .{.String = "Hello"}) == null);
    testing.expect(root.findNth(1, .{.String = "foo"}) == null);
    testing.expect(root.findNthAny(0, .String) != null);
    testing.expect(root.findNthAny(1, .String) != null);
    testing.expect(root.findNthAny(2, .String) == null);

    testing.expect(root.findNth(0, .{.Int = 42}) != null);
    testing.expect(root.findNth(0, .{.Int = 41}) == null);
    testing.expect(root.findNth(1, .{.Int = 42}) == null);
    testing.expect(root.findNthAny(0, .Int) != null);
    testing.expect(root.findNthAny(1, .Int) == null);

    testing.expect(root.findNth(0, .{.Float = 3.14}) != null);
    testing.expect(root.findNth(0, .{.Float = 3.13}) == null);
    testing.expect(root.findNth(1, .{.Float = 3.14}) == null);
    testing.expect(root.findNthAny(0, .Float) != null);
    testing.expect(root.findNthAny(1, .Float) == null);

    testing.expect(root.findNthAny(0, .Bool) != null);
    testing.expect(root.findNth(0, .{.Bool = true}) != null);
    testing.expect(root.findNthAny(1, .Bool) == null);
    testing.expect(root.findNth(1, .{.Bool = true}) == null);
}

test "parsing into nodes" {
    const testing = std.testing;
    const text1 =
        \\elements: fire,water,air,earth
        \\subelements:
        \\  fire: lightning
        \\  water: blood; ice
        \\  air: spirit
        \\  earth: [[metal]]
    ;
    const text2 =
        \\elements:fire,water,air,earth;
        \\subelements:fire:lightning;water:blood;ice,air:spirit,;earth:metal;;
    ;
    const text =
        \\name:wizard;
        \\stats
        \\  : health:10
        \\    mana:30
    ;
}

/// Options that can be enabled when calling `imprint` onto a struct.
pub const ImprintOptions = packed struct {
    /// Return an error when a struct field is missing from the node tree.
    ensure_node_exists: bool = false,
    /// Returns an error when a field's node exists, but the value doesn't.
    ensure_value_exists: bool = false,
    /// Returns an error when a node's value is of the wrong type.
    ensure_correct_value_type: bool = true,
    /// Enables number coersion.
    coerce_numbers: bool = true,
    /// Enabled coersion of ints to bools.
    coerce_ints_to_bools: bool = false,
    /// Returns an error when a node couldn't be converted to an enum.
    ensure_enum_converted: bool = true,
    /// Returns an error when passed invalid types.
    no_invalid_types: bool = true,
};

/// Imprints a node into a type. The only types allowed are zzz types, structs, fixed arrays,
/// optionals, and enums. This function performs no allocations and u8 slices refer to strings
/// by reference. Enums can be mapped from string or int. There are a few options:
///
/// - `.NoCheck` perform no opts, if something can fit it'll fit.
/// - `.CheckField`
///
/// TODO: Removing anyerror causes infinite loop.
pub fn imprint(self: *const ZNode, opts: ImprintOptions, onto_ptr: anytype) anyerror!void {
    if (@typeInfo(@TypeOf(onto_ptr)) != .Pointer) {
        @compileError("Passed struct must be a pointer.");
    }
    if (@typeInfo(@TypeOf(self)) != .Pointer) {
        @compileError("Passed node must be a pointer.");
    }
    const T = @typeInfo(@TypeOf(onto_ptr)).Pointer.child;
    switch (@typeInfo(T)) {
        .Void => { },
        .Bool => {
            onto_ptr.* = switch (self.value) {
                .Bool => |b| b,
                else => if (opts.ensure_correct_value_type) return error.ExpectedBool else return,
            };
        },
        .Float, .ComptimeFloat => {
            onto_ptr.* = switch (self.value) {
                .Float => |n| if (opts.coerce_numbers) @floatCast(T, n) else @floatCast(f32, n),
                .Int => |n| if (opts.coerce_numbers) @intToFloat(T, n) else return error.ExpectedFloat,
                else => if (opts.ensure_correct_value_type) return error.ExpectedFloat else return,
            };
        },
        .Int, .ComptimeInt => {
            onto_ptr.* = switch (self.value) {
                .Int => |n| if (opts.coerce_numbers) @intCast(T, n) else @intCast(i32, n),
                .Bool => |n| if (opts.coerce_ints_to_bools) @boolToInt(n) else return error.ExpectedInt,
                else => if (opts.ensure_correct_value_type) return error.ExpectedInt else return,
            };
        },
        .Enum => {
            switch (self.value) {
                .Int => |int| {
                    onto_ptr.* = try std.meta.intToEnum(T, int);
                },
                .String => {
                    if (std.meta.stringToEnum(T, self.value.String)) |e| {
                        onto_ptr.* = e;
                    } else {
                        return if (opts.ensure_enum_converted) error.CouldNotConvertStringToEnum;
                    }
                },
                else => if (opts.ensure_correct_value_type) return error.ExpectedIntOrString,
            }
        },
        .Optional => |opt_info| {
            // Special case for optional structs, otherwise fields won't be initialized.
            // TODO: Investigate other special cases.
            if (@typeInfo(opt_info.child) == .Struct) {
                var t: opt_info.child = opt_info.child{};
                var err = false;
                imprint(self, opts, &t) catch |e| {
                    if (e != error.ValueDoesNotExist) {
                        return e;
                    }
                    err = true;
                };
                if (!err) { onto_ptr.* = t; }
            } else if (@typeInfo(opt_info.child) != .Pointer) {
                var t = std.mem.zeroes(opt_info.child);
                var err = false;
                imprint(self, opts, &t) catch |e| {
                    if (e != error.ValueDoesNotExist) {
                        return e;
                    }
                    err = true;
                };
                if (!err) { onto_ptr.* = t; }
            } else {
                // TODO: This is okay because we error on value not existing.
                var t: opt_info.child = undefined;
                var err = false;
                imprint(self, opts, &t) catch |e| {
                    // Missing check.
                    err = true;
                };
                if (!err) { onto_ptr.* = t; }
            }
        },
        .Struct => |struct_info| {
            var iter: ?*const ZNode = null;

            inline for (struct_info.fields) |field, i| {
                if (field.name[0] == '_') {
                    continue;
                }
                const found = self.findNext(iter, .{.String = field.name});
                if (found) |child_field| {
                    // Found, set the iterator here.
                    iter = found;
                    // Special case for pointers, we just take the whole node.
                    const info = @typeInfo(field.field_type);
                    if ((info == .Optional
                            and @typeInfo(info.Optional.child) == .Pointer
                            and @typeInfo(info.Optional.child).Pointer.size == .One) or
                        (info == .Pointer and info.Pointer.size == .One)) {
                        try imprint(child_field, opts, &@field(onto_ptr, field.name));
                    } else {
                        if (child_field.getChild(0)) |child| {
                            try imprint(child, opts, &@field(onto_ptr, field.name));
                        } else if (opts.ensure_value_exists) {
                            return error.ValueDoesNotExist;
                        }
                    }
                } else if (opts.ensure_node_exists) {
                    return error.NodeDoesNotExist;
                }
            }
        },
        // Only handle [N]?T, where T is any other valid type.
        .Array => |array_info| {
            // Arrays are weird. They work on siblings, not children.
            if (self.parent) |parent| {
                var r = std.mem.zeroes(T);
                var i: usize = 0;
                while (i < r.len) : (i += 1) {
                    if (i >= parent.getChildCount()) {
                        break;
                    }
                    try imprint(parent.getChild(i).?, opts, &r[i]);
                }
                onto_ptr.* = r;
            }
        },
        // Only handle []const u8 and ZNode pointers.
        .Pointer => |ptr_info| {
            switch (ptr_info.size) {
                .One => {
                    if (ptr_info.child == ZNode) {
                        onto_ptr.* = self;
                        return;
                    }
                    if (opts.no_invalid_types) {
                        return error.InvalidPointerType;
                    }
                },
                .Slice => {
                    switch (self.value) {
                        .String, => {
                            if (ptr_info.child != u8) {
                                if (opts.no_invalid_types) {
                                    return error.InvalidNonStringSlice;
                                }
                            } else {
                                onto_ptr.* = self.value.String;
                            }
                        },
                        else => if (opts.ensure_correct_value_type) return error.ExpectedStringNode,
                    }
                    return;
                },
                else => if (opts.no_invalid_types) return error.InvalidType,
            }
        },
        else => if (opts.no_invalid_types) return error.InvalidType,
    }
}

pub fn make(comptime T: type, argz: *const ZNode) !T {
    var ret: T = .{};
    try imprint(&ret, ImprintOptions{}, argz);
    return ret;
}

pub fn create(comptime T: type, allocator: *std.mem.Allocator, argz: *const ZNode, fields: anytype) !*T {
    var ret = try allocator.create(T);
    ret.* = .{};
    try imprint(argz, ImprintOptions{}, ret);
    inline for (std.meta.fields(@TypeOf(fields))) |fld| {
        @field(ret, fld.name) = @field(fields, fld.name);
    }
    return ret;
}

/// A useful factory of dynamic objects via zzz nodes. Register types and instantiate them with
/// nodes. The type provided is an interface type that has field function pointers to be used
/// with `@fieldParentPtr`
pub fn ZFactory(comptime T: type) type {
    return struct {
        const Self = @This();

        const Ctor = struct {
            func: fn(allocator: *std.mem.Allocator, argz: *const ZNode) anyerror!*T,
        };

        registered: std.StringHashMap(Ctor),

        /// Create the factory. The allocator is for the internal HashMap. Instantiated objects
        /// can have their own allocator.
        pub fn init(allocator: *std.mem.Allocator) Self {
            return Self {
                .registered = std.StringHashMap(Ctor).init(allocator),
            };
        }

        ///
        pub fn deinit(self: *Self) void {
            self.registered.deinit();
        }

        /// Registers an implementor of the interface. Requires ZNAME and a zinit
        /// method:
        ///
        ///
        pub fn register(self: *Self, comptime S: anytype) !void {
            const SI = @typeInfo(S);
            if (SI != .Struct) {
                @compileError("Expected struct got: " ++ @typeName(S));
            }
            if (!@hasDecl(S, "ZNAME")) {
                @compileError("Missing `ZNAME` on registered struct: " ++ @typeName(S));
            }
            if (!@hasDecl(S, "zinit")) {
                @compileError("Missing `zinit` on registered struct: " ++ @typeName(S));
            }
            const ctor = Ctor{
                .func = S.zinit,
            };
            try self.registered.put(S.ZNAME, ctor);
        }

        /// Instantiates an object with ZNode. The node must have the first child node must be
        /// "name" with the child value being the name of the registered struct. Sibling fields
        /// are imprinted onto the struct.
        ///
        /// The caller is responsible for the memory.
        pub fn instantiate(self: *Self, allocator: *std.mem.Allocator, node: *const ZNode) !*T {
            const name = node.findNth(0, .{.String = "name"}) orelse return error.ZNodeMissingName;
            const value_node = name.getChild(0) orelse return error.ZNodeMissingValueUnderName;
            if (value_node.value != .String) {
                return error.ZNodeNameValueNotString;
            }
            const ctor = self.registered.get(value_node.value.String) orelse return error.StructNotFound;
            return try ctor.func(allocator, node);
        }
    };
}

pub const FooInterface = struct {
    const Self = @This();

    default: i32 = 100,
    fooFn: ?fn(*Self) void = null,

    // Should always have a deinit.
    deinitFn: fn(*const Self) void = undefined,

    pub fn foo(self: *Self) void {
        std.debug.print("{}\n", .{self.default});
        return self.fooFn.?(self);
    }
    pub fn deinit(self: *const Self) void {
        self.deinitFn(self);
    }
};

pub const FooBar = struct {
    const Self = @This();
    const ZNAME = "Foo";
    _allocator: *std.mem.Allocator = undefined,
    _interface: FooInterface = .{},
    bar: i32 = 0,

    pub fn zinit(allocator: *std.mem.Allocator, argz: *const ZNode) !*FooInterface {
        var self = try create(Self, allocator, argz, .{
            ._allocator = allocator,
            ._interface = .{
                .fooFn = foo,
                .deinitFn = deinit,
            }
        });
        return &self._interface;
    }

    pub fn deinit(interface: *const FooInterface) void {
        const self = @fieldParentPtr(Self, "_interface", interface);
        self._allocator.destroy(self);
    }

    pub fn foo(interface: *FooInterface) void {
        var self = @fieldParentPtr(FooBar, "_interface", interface);
        std.debug.print("{}\n", .{self.bar});
    }
};

test "factory" {
    const testing = std.testing;

    const text =
        \\name:Foo
        \\bar:42
    ;

    var tree = ZTree(1, 100){};
    var root = try tree.appendText(text);
    try root.transform(void, {}, defaultTransformer);

    var factory = ZFactory(FooInterface).init(testing.allocator);
    defer factory.deinit();

    try factory.register(FooBar);

    const foobar = try factory.instantiate(testing.allocator, root);
    foobar.foo();
    defer foobar.deinit();
}