pub const std = @import("std");
pub const zzz = @import("zzz");

fn nodeApi() zzz.ZError!void {
    var tree = zzz.StaticTree(8){};
    try zzz.appendText(&tree, null, "foo:bar;biz:baz,boom");

    var iter: ?*zzz.Node = null;

    // Iterate nodes in the tree starting from some node.
    iter = &tree.root;
    while (iter) |node| : (iter = node.next(null)) {
        std.debug.print("nodeApi():next(): {s}\n", .{node.value});
    }

    // Iterate children.
    iter = null;
    while (tree.root.nextChild(iter)) |node| : (iter = node) {
        std.debug.print("nodeApi():nextChild(): {s}\n", .{node.value});
    }

    // Iterate descendants.
    iter = null;
    while (tree.root.nextDescendant(iter, null)) |node| : (iter = node) {
        std.debug.print("nodeApi():nextDescendant(): {s}\n", .{node.value});
    }

    // Number of children.
    std.debug.print("nodeApi():getChildCount(): {d}\n", .{tree.root.getChildCount()});

    // Get nth child.
    std.debug.print("nodeApi():getNthChild(): {s}\n", .{tree.root.getNthChild(0).?.value});
    std.debug.print("nodeApi():getNthChildValue(): {s}\n", .{tree.root.getNthChildValue(1)});

    // Find children/descendants.
    std.debug.print("nodeApi():findChild(): {s}\n", .{tree.root.findChild("biz").?.value});
    std.debug.print("nodeApi():findDescendant(): {s}\n", .{tree.root.findDescendant("boom").?.value});

    // Output.
    tree.root.show();
}

fn staticTreeApi() zzz.ZError!void {
    var tree = zzz.StaticTree(8){};
    try zzz.appendText(&tree, null, "foo:bar;biz:baz,boom");

    // Remaining nodes in the tree.
    std.debug.print("staticTreeApi():node_count: {d}\n", .{tree.node_count});
    std.debug.print("staticTreeApi():nodesRemaining(): {d}\n", .{tree.nodesRemaining()});

    // Check if we can append more text.
    std.debug.print("staticTreeApi():canAppend(): {s}\n", .{tree.canAppend("this:will:not:fit")});
    std.debug.print("staticTreeApi():canAppend(): {s}\n", .{tree.canAppend("this:will")});

    // Appending values.
    std.debug.print("staticTreeApi():appendValue(): {s}\n", .{(try tree.appendValue(null, "last one")).value});

    // Clearing.
    tree.clear();
}

fn dynamicTreeApi() zzz.ZError!void {
    var tree = zzz.DynamicTree.init(std.testing.allocator);
    defer tree.deinit();

    try zzz.appendText(&tree, null, "foo:bar;biz:baz,boom");

    // Appending values and types.
    std.debug.print("dynamicTreeApi():appendValue(): {s}\n", .{(try tree.appendValue(null, "some string")).value});
    std.debug.print("dynamicTreeApi():apndAnytype(): {s}\n", .{(try tree.appendAnytype(null, 42)).value});
}

fn moduleApi() zzz.ZError!void {
    var stree = zzz.StaticTree(8){};
    var dtree = zzz.DynamicTree.init(std.testing.allocator);
    defer dtree.deinit();

    // Append text.
    try zzz.appendText(&stree, null, "foo:bar;biz:baz,boom");
    try zzz.appendText(&dtree, null, "foo:bar;biz:baz,boom");

    // Copy nodes.
    _ = try zzz.copyNode(&dtree, null, stree.root.findDescendant("biz").?);

    dtree.root.show();

    // Count text nodes.
    std.debug.print("moduleApi():countTextNodes(): {d}\n", .{zzz.countTextNodes("this:has:four:nodes")});
}

pub fn main() zzz.ZError!void {
    try nodeApi();
    try staticTreeApi();
    try dynamicTreeApi();
    try moduleApi();
}
