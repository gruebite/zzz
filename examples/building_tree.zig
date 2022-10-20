pub const std = @import("std");
pub const zzz = @import("zzz");

pub fn main() !void {
    // Creates a static tree that can contain 100 nodes total.
    var tree = zzz.StaticTree(100){};

    _ = try tree.appendValue(try tree.appendValue(null, ""), "baz");

    // Add some properties.
    _ = try tree.appendValue(try tree.appendValue(null, "name"), "Foobar");
    // Build an array by adding children to the same parent.
    var stats = try tree.appendValue(null, "stats");
    _ = try tree.appendValue(try tree.appendValue(stats, "health"), "10");
    _ = try tree.appendValue(try tree.appendValue(stats, "mana"), "10");

    tree.root.show();

    // Creates a dynamic tree that can contain any number of nodes.
    var dtree = zzz.DynamicTree.init(std.heap.page_allocator);
    defer dtree.deinit();

    _ = try dtree.appendAnytype(null, true);
    _ = try dtree.appendAnytype(null, 42);
    _ = try dtree.appendAnytype(null, 6.9);

    dtree.root.show();
}
