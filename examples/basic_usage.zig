
pub const std = @import("std");
pub const zzz = @import("zzz");

const kobold = @embedFile("../example-data/kobold.zzz");

pub fn main() !void {
    // Parse the text into node.
    const node = try zzz.parse(std.testing.allocator, &zzz.ParseOptions{}, kobold);
    defer node.deinit();

    const print = std.debug.print;

    // Grab the kobol's dexterity under "stats" -> "dex" -> integer
    print("Kobold's dexterity: {}\n", .{
        node.findString("stats", 0).?.findString("dex", 0).?.getChild(0).?.getInteger()
    });

    // Grab the Kobold's first ability under "abilities" -> [first, second].
    const ability = node.findString("abilities", 0).?.getChild(0).?;
    print("Kobold's second ability: {}\n{}\n", .{
        // Get the name.
        ability.getString(),
        // Get the description.
        ability.getChild(0).?.getString(),
    });
}
