
pub const std = @import("std");
pub const zzz = @import("zzz");

const kobold = @embedFile("../example-data/kobold.zzz");
const json_example = @embedFile("../example-data/json-example-3.zzz");

pub fn main() !void {
    // The kobold has exactly 51 nodes.
    var tree = zzz.ZStaticTree(1, 51){};
    const node = try tree.appendText(kobold);
    try node.transform(void, {}, zzz.defaultTransformer);
    tree.show();

    std.debug.print("Number of nodes: {}\n", .{tree.node_count});
    std.debug.print("Kobold's CON: {}\n", .{node.findNthDescendant(0, .{.String = "con"}).?.child.?.value.Int});

    // The JSON example has exactly 161 nodes.
    var big_tree = zzz.ZStaticTree(1, 161){};
    const root = try big_tree.appendText(json_example);
    try root.transform(void, {}, zzz.defaultTransformer);
    big_tree.show();

    std.debug.print("Number of nodes: {}\n", .{big_tree.node_count});
}
