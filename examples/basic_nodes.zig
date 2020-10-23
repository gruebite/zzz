
pub const std = @import("std");
pub const zzz = @import("zzz");

pub fn main() !void {
    // Create a root Node.
    var root = zzz.ZNode.init(std.testing.allocator, .Null);
    defer root.deinit();

    const print = std.debug.print;


    _ = try (try root.append(.{.String = "name"})).append(.{.String = "name"});
    var stats = try root.append(.{.String = "stats"});
    _ = try (try stats.append(.{.String = "health"})).append(.{.Int = 10});
    _ = try (try stats.append(.{.String = "mana"})).append(.{.Int = 30});

    var out = std.io.getStdOut().writer();
    try zzz.stringify(root, out);
}
