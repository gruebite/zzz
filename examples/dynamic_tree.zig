pub const std = @import("std");
pub const zzz = @import("zzz");

const kobold = @embedFile("../example-data/kobold.zzz");
const json_example = @embedFile("../example-data/json-example-3.zzz");

pub fn main() zzz.ZError!void {
    // Dynamic trees will grow until they can't.
    var tree = zzz.DynamicTree.init(std.testing.allocator);
    defer tree.deinit();

    // Append the text to the tree.
    try zzz.appendText(&tree, null, kobold);
    // Debug print.
    tree.root.show();

    std.debug.print("Number of nodes: {d}\n", .{tree.node_count});
    // This function searches all the node's descendants.
    std.debug.print("Kobold's CON: {s}\n", .{tree.root.findDescendant("con").?.child.?.value});

    var big_tree = zzz.DynamicTree.init(std.testing.allocator);
    defer big_tree.deinit();
    try zzz.appendText(&big_tree, null, json_example);
    big_tree.root.show();

    // Find all servlet names.
    var node = &big_tree.root;
    while (node.next(null)) |next_node| : (node = next_node) {
        if (std.mem.eql(u8, next_node.value, "servlet-name")) {
            if (next_node.child) |child| {
                std.debug.print("servlet-name: {s}\n", .{child.value});
            }
        }
    }

    std.debug.print("Number of nodes: {}\n", .{big_tree.node_count});
}
