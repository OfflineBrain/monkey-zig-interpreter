const std = @import("std");

const Token = union(enum) {
    assign,
    plus,
    minus,
    asterisk,
    slash,
    bang,
    comma,
    dot,
    colon,
    semicolon,
    lparen,
    rparen,
    lbrase,
    rbrace,
    gt,
    gte,
    lt,
    lte,
    eq,
    noteq,
    if_literal,
    else_literal,
    return_literal,
    let_literal,
    fn_literal,
    true_literal,
    false_literal,
    eof,
    illegal,

    ident: []const u8,
    int: []const u8,

    fn keyword(ident: []const u8) Token {
        const map = std.ComptimeStringMap(Token, .{
            .{ "let", .let_literal },
            .{ "if", .if_literal },
            .{ "else", .else_literal },
            .{ "return", .return_literal },
            .{ "fn", .fn_literal },
            .{ "true", .true_literal },
            .{ "false", .false_literal },
        });

        return if (map.get(ident)) |k| blk: {
            break :blk k;
        } else .{ .ident = ident };
    }
};

fn isLetter(ch: u8) bool {
    return std.ascii.isAlphanumeric(ch) or ch == '_';
}

fn isInt(ch: u8) bool {
    return std.ascii.isDigit(ch) or ch == '_';
}

pub const Lexer = struct {
    const Self = @This();

    input: []const u8,
    position: usize = 0,
    read_position: usize = 0,
    ch: u8 = 0,

    pub fn init(input: []const u8) Self {
        var lexer = Self{
            .input = input,
        };

        lexer.readChar();

        return lexer;
    }

    pub fn nextToken(self: *Self) Token {
        self.skipWhitespaces();

        const token: Token = switch (self.ch) {
            '+' => .plus,
            '-' => .minus,
            '*' => .asterisk,
            '/' => .slash,
            '(' => .lparen,
            ')' => .rparen,
            '{' => .lbrase,
            '}' => .rbrace,
            ',' => .comma,
            '.' => .dot,
            ':' => .colon,
            ';' => .semicolon,

            '=' => if (self.peekChar() == '=') blk: {
                self.readChar();
                break :blk .eq;
            } else .assign,
            '!' => if (self.peekChar() == '=') blk: {
                self.readChar();
                break :blk .noteq;
            } else .bang,
            '>' => if (self.peekChar() == '=') blk: {
                self.readChar();
                break :blk .gte;
            } else .gt,
            '<' => if (self.peekChar() == '=') blk: {
                self.readChar();
                break :blk .lte;
            } else .lt,
            0 => .eof,

            'a'...'z', 'A'...'Z', '_' => Token.keyword(self.readIdentifier()),
            '0'...'9' => .{ .int = self.readInt() },
            else => .illegal,
        };

        self.readChar();

        return token;
    }

    fn skipWhitespaces(self: *Self) void {
        while (std.ascii.isWhitespace(self.ch)) {
            self.readChar();
        }
    }

    fn readIdentifier(self: *Self) []const u8 {
        const position = self.position;

        while (isLetter(self.input[self.read_position])) {
            self.readChar();
        }

        return self.input[position..self.read_position];
    }

    fn readInt(self: *Self) []const u8 {
        const position = self.position;

        while (isInt(self.input[self.read_position])) {
            self.readChar();
        }

        return self.input[position..self.read_position];
    }

    fn readChar(self: *Self) void {
        if (self.read_position >= self.input.len) {
            self.ch = 0;
        } else {
            self.ch = self.input[self.read_position];
        }

        self.position = self.read_position;
        self.read_position = self.read_position + 1;
    }

    fn peekChar(self: Self) u8 {
        if (self.read_position >= self.input.len) {
            return 0;
        } else {
            return self.input[self.read_position];
        }
    }
};

test "lexer" {
    const input =
        \\5 + 5;
        \\a * value;
        \\!true;
        \\-5;
        \\10 / 8;
        \\let add = fn(a, b) {
        \\  return a + b;
        \\}
        \\
        \\if (a < b) {
        \\  return true;
        \\} else {
        \\  return b > a;
        \\}
        \\
        \\1 == 11;
        \\1 != 10;
        \\1 <= 11;
        \\1 >= 11;
    ;

    const output = [_]Token{
        .{ .int = "5" },
        .plus,
        .{ .int = "5" },
        .semicolon,
        .{ .ident = "a" },
        .asterisk,
        .{ .ident = "value" },
        .semicolon,
        .bang,
        .true_literal,
        .semicolon,
        .minus,
        .{ .int = "5" },
        .semicolon,
        .{ .int = "10" },
        .slash,
        .{ .int = "8" },
        .semicolon,
        .let_literal,
        .{ .ident = "add" },
        .assign,
        .fn_literal,
        .lparen,
        .{ .ident = "a" },
        .comma,
        .{ .ident = "b" },
        .rparen,
        .lbrase,
        .return_literal,
        .{ .ident = "a" },
        .plus,
        .{ .ident = "b" },
        .semicolon,
        .rbrace,
        .if_literal,
        .lparen,
        .{ .ident = "a" },
        .lt,
        .{ .ident = "b" },
        .rparen,
        .lbrase,
        .return_literal,
        .true_literal,
        .semicolon,
        .rbrace,
        .else_literal,
        .lbrase,
        .return_literal,
        .{ .ident = "b" },
        .gt,
        .{ .ident = "a" },
        .semicolon,
        .rbrace,
        .{ .int = "1" },
        .eq,
        .{ .int = "11" },
        .semicolon,
        .{ .int = "1" },
        .noteq,
        .{ .int = "10" },
        .semicolon,
        .{ .int = "1" },
        .lte,
        .{ .int = "11" },
        .semicolon,
        .{ .int = "1" },
        .gte,
        .{ .int = "11" },
        .semicolon,
        .eof,
    };

    var lexer = Lexer.init(input);

    for (output) |expected_token| {
        const actual_token = lexer.nextToken();

        try std.testing.expectEqualDeep(expected_token, actual_token);
    }
}
