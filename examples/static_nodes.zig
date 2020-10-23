
pub const std = @import("std");
pub const zzz = @import("zzz");

const kobold = @embedFile("../example-data/kobold.zzz");

pub fn main() !void {
    // The kobold has exactly 51 nodes.
    var tree = zzz.ZStaticTree(1, 51){};
    const node = try tree.appendText(kobold);
    try node.transform(void, {}, zzz.defaultTransformer);
    tree.show();

    std.debug.print("Number of nodes: {}\n", .{tree.node_count});
    std.debug.print("Kobold's CON: {}\n", .{node.findNthDescendant(0, .{.String = "con"}).?.child.?.value.Int});
}
