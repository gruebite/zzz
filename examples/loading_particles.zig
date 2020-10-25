
pub const std = @import("std");
pub const zzz = @import("zzz");

const particles = @embedFile("../example-data/particles.zzz");

pub fn main() !void {
    var tree = zzz.ZTree(1, 1000){};
    const root = try tree.appendText(particles);

    root.show();
    std.debug.print("Node count: {}\n", .{tree.node_count});
}
