
pub const std = @import("std");
pub const zzz = @import("zzz");

pub fn main() !void {
    // Create a root Node. The allocator is only used for creating lists in the tree and will
    // not make an allocation on initialization.
    var root = zzz.ZNode.init(std.testing.allocator, .Null);
    defer root.deinit();

    const print = std.debug.print;

    // These can throw memory allocation errors.
    _ = try (try root.append(.{.String = "name"})).append(.{.String = "name"});
    // Can use the returns node to append more.
    var stats = try root.append(.{.String = "stats"});
    _ = try (try stats.append(.{.String = "health"})).append(.{.Int = 10});
    _ = try (try stats.append(.{.String = "mana"})).append(.{.Int = 30});

    // Basic output.
    var out = std.io.getStdOut().writer();
    try zzz.stringify(root, out);
}
