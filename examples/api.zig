pub const std = @import("std");
pub const zzz = @import("zzz");

fn node_functions() !void {
    var tree = zzz.ZStaticTree(8){};
    try zzz.appendText(&tree, null, "foo:bar;biz:baz,boom");

    var iter: ?*zzz.ZNode = null;

    // Iterate nodes in the tree starting from some node.
    iter = &tree.root;
    while (iter) |it| : (iter = it.next(null)) {
        std.debug.print("node_functions():next(): {s}\n", .{it.*.value});
    }

    // Iterate children.
    iter = null;
    while (tree.root.nextChild(iter)) |it| : (iter = it) {
        std.debug.print("node_functions():nextChild(): {s}\n", .{it.*.value});
    }

    // Iterate descendants.
    iter = null;
    while (tree.root.nextDescendant(iter, null)) |it| : (iter = it) {
        std.debug.print("node_functions():nextDescendant(): {s}\n", .{it.*.value});
    }

    // Get nth child.
    std.debug.print("node_functions():getNthChild(): {s}\n", .{tree.root.getNthChild(0).?.value});
    std.debug.print("node_functions():getNthChildValue(): {s}\n", .{tree.root.getNthChildValue(1)});

    // Number of children.
    std.debug.print("node_functions():getChildCount(): {}\n", .{tree.root.getChildCount()});

    // Find children/descendants.
    std.debug.print("node_functions():findChild(): {s}\n", .{tree.root.findChild("biz").?.value});
    std.debug.print("node_functions():findDescendant(): {s}\n", .{tree.root.findDescendant("boom").?.value});

    // Output.
    tree.root.show();
}

fn static_tree_functions() !void {
    var tree = zzz.ZStaticTree(8){};
    try zzz.appendText(&tree, null, "foo:bar;biz:baz,boom");

    // Remaining nodes in the tree.
    std.debug.print("static_tree_functions():node_count: {}\n", .{tree.node_count});
    std.debug.print("static_tree_functions():nodesRemaining(): {}\n", .{tree.nodesRemaining()});

    // Check if we can append more text.
    std.debug.print("static_tree_functions():canAppend(): {}\n", .{tree.canAppend("this:will:not:fit")});
    std.debug.print("static_tree_functions():canAppend(): {}\n", .{tree.canAppend("this:will")});

    // Appending values.
    std.debug.print("static_tree_functions():appendValue(): {s}\n", .{(try tree.appendValue(null, "last one")).value});

    // Clearing.
    tree.clear();
}

fn dynamic_tree_functions() !void {
    var tree = zzz.ZDynamicTree.init(std.testing.allocator);
    defer tree.deinit();

    try zzz.appendText(&tree, null, "foo:bar;biz:baz,boom");

    // Appending values and types.
    std.debug.print("dynamic_tree_functions():appendValue(): {s}\n", .{(try tree.appendValue(null, "some string")).value});
    std.debug.print("dynamic_tree_functions():appendAnytype(): {s}\n", .{(try tree.appendAnytype(null, 42)).value});

}

fn module_functions() !void {
    var stree = zzz.ZStaticTree(8){};
    var dtree = zzz.ZDynamicTree.init(std.testing.allocator);
    defer dtree.deinit();

    // Append text.
    try zzz.appendText(&stree, null, "foo:bar;biz:baz,boom");
    try zzz.appendText(&dtree, null, "foo:bar;biz:baz,boom");

    // Copy nodes.
    _ = try zzz.copyNode(&dtree, null, stree.root.findDescendant("biz").?);

    dtree.root.show();

    // Count text nodes.
    std.debug.print("module_functions():countTextNodes(): {}\n", .{zzz.countTextNodes("this:has:four:nodes")});
}

pub fn main() !void {
    try node_functions();
    try static_tree_functions();
    try dynamic_tree_functions();
    try module_functions();
}
