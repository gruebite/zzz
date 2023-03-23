pub const std = @import("std");
pub const zzz = @import("zzz");

const particles = @embedFile("./example-data/particles.zzz");

pub fn main() !void {
    var tree = zzz.StaticTree(1000){};
    try zzz.appendText(&tree, null, particles);

    tree.root.show();
    std.debug.print("Node count: {}\n", .{tree.node_count});
}
