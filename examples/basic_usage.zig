
pub const std = @import("std");
pub const zzz = @import("zzz");

const kobold = @embedFile("../example-data/kobold.zzz");

pub fn main() !void {
    const node = try zzz.parse(std.testing.allocator, kobold);
    defer node.deinit();

    const print = std.debug.print;

    print("Kobold's dexterity: {}\n", .{
        node.findString("stats", 0).?.findString("dex", 0).?.getChild(0).?.getInteger()
    });

    const ability = node.findString("abilities", 0).?.getChild(0).?;
    print("Kobold's second ability: {}\n{}\n", .{
        ability.getString(),
        ability.getChild(0).?.getString(),
    });
}
