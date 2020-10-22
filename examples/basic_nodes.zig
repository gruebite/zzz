
pub const std = @import("std");
pub const zzz = @import("zzz");

pub fn main() !void {
    // Create a root Node.
    var node = zzz.ZNode.initNull(std.testing.allocator);
    defer node.deinit();

    const print = std.debug.print;


    _ = try (try node.appendStringRef("name")).appendStringRef("wizard");
    var stats = try node.appendStringRef("stats");
    _ = try (try stats.appendStringRef("health")).appendInteger(10);
    _ = try (try stats.appendStringRef("mana")).appendInteger(30);

    var out = std.io.getStdOut().writer();
    try zzz.stringify(node, out);
}
