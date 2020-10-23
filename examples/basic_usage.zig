
pub const std = @import("std");
pub const zzz = @import("zzz");

const kobold = @embedFile("../example-data/kobold.zzz");

pub fn main() !void {
    // Parse the text into node.
    const node = try zzz.parse(std.testing.allocator, kobold);
    defer node.deinit();

    const print = std.debug.print;

    // Grab the kobol's dexterity under "stats" -> "dex" -> integer
    print("Kobold's dexterity: {}\n", .{
        node.findNth(0, .{.String = "stats"}).?.findNth(0, .{.String = "dex"}).?.getChild(0).?.value.Int
    });

    // Grab the Kobold's first ability under "abilities" -> [first, second].
    const ability = node.findNth(0, .{.String = "abilities"}).?.getChild(0).?;
    print("Kobold's second ability: {}\n{}\n", .{
        // Get the name.
        ability.value.String,
        // Get the description.
        ability.getChild(0).?.value.String,
    });
}
