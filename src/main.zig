const std = @import("std");
const lx = @import("lexer.zig");
const pr = @import("parser.zig");
const evl = @import("eval.zig");

pub fn main() !void {
    // Prints to stderr (it's a shortcut based on `std.io.getStdErr()`)
    std.debug.print("All your {s} are belong to us.\n", .{"codebase"});

    // stdout is for the actual output of your application, for example if you
    // are implementing gzip, then only the compressed bytes should be sent to
    // stdout, not any debugging messages.
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    const stdin_file = std.io.getStdIn();
    const stdin = stdin_file.reader();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var env = try evl.Environment.init(allocator);
    defer env.deinit();

    var promts = std.ArrayList([]const u8).init(allocator);
    defer promts.deinit();

    var input = try nextLine(stdin, allocator);
    while (!std.mem.eql(u8, input, "exit")) {
        try promts.append(input);

        var lexer = lx.Lexer.init(input);
        var parser = try pr.Parser.init(allocator, lexer);
        defer parser.deinit();
        var program = try parser.parse();
        defer program.destroy();

        const output = try evl.evalProgram(program, env);

        try stdout.print(">>> {s}\n", .{output});

        try bw.flush();

        input = try nextLine(stdin, allocator);
    }

    for (promts.items) |value| {
        allocator.free(value);
    }
}

fn nextLine(reader: anytype, allocator: std.mem.Allocator) ![]const u8 {
    var line = (try reader.readUntilDelimiterOrEofAlloc(allocator, '\n', 4096)) orelse return std.fmt.allocPrint(allocator, "", .{});
    // trim annoying windows-only carriage return character
    if (@import("builtin").os.tag == .windows) {
        return std.mem.trimRight(u8, line, "\r");
    } else {
        return line;
    }
}

test "simple test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}
