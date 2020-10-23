
pub const std = @import("std");
pub const zzz = @import("zzz");

pub fn main() !void {
    // Create a root Node.
    var node = zzz.ZNode.init(std.testing.allocator, .Null);
    defer node.deinit();

    const print = std.debug.print;


    _ = try (try node.append(.{.String = "name"})).append(.{.String = "name"});
    var stats = try node.append(.{.String = "stats"});
    _ = try (try stats.append(.{.String = "health"})).append(.{.Int = 10});
    _ = try (try stats.append(.{.String = "mana"})).append(.{.Int = 30});

    var out = std.io.getStdOut().writer();
    try zzz.stringify(node, out);
}
