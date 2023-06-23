const l = @import("lexer.zig");
const std = @import("std");

const Statement = union(enum) {
    let_statement: *LetStatement,
    return_statement: *ReturnStatment,
    expr_statement: *ExpressionStatement,

    fn deinit(self: Statement, a: std.mem.Allocator) void {
        switch (self) {
            .let_statement => |value| {
                value.deinit(a);
            },
            .return_statement => |value| {
                value.deinit(a);
            },
            .expr_statement => |value| {
                value.deinit(a);
            },
        }
    }
};

const Expression = union(enum) {
    identifier: Identifier,
    default,
    fn deinit(self: Expression, a: std.mem.Allocator) void {
        _ = a;
        _ = self;
    }
};

const Program = struct {
    statements: std.ArrayList(Statement),
    allocator: std.mem.Allocator,

    pub fn destroy(program: *Program) void {
        defer program.statements.deinit();
        defer for (program.statements.items) |value| {
            value.deinit(t_allocator);
            switch (value) {
                .let_statement => |stmt| {
                    t_allocator.destroy(stmt);
                },
                .return_statement => |stmt| {
                    t_allocator.destroy(stmt);
                },
                .expr_statement => |stmt| {
                    t_allocator.destroy(stmt);
                },
            }
        };
    }
};

const LetStatement = struct {
    const Self = @This();
    identifier: Identifier,
    value: Expression,

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = options;
        _ = fmt;
        try writer.print("let {s} = {s}", .{ self.identifier, self.value });
    }

    fn deinit(self: Self, a: std.mem.Allocator) void {
        self.value.deinit(a);
    }
};

const ReturnStatment = struct {
    const Self = @This();
    value: Expression,

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = options;
        _ = fmt;
        try writer.print("return {s}", .{self.value});
    }

    fn deinit(self: Self, a: std.mem.Allocator) void {
        self.value.deinit(a);
    }
};

const ExpressionStatement = struct {
    const Self = @This();
    value: Expression,

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = options;
        _ = fmt;
        try writer.print("{s}", .{self.value});
    }

    fn deinit(self: Self, a: std.mem.Allocator) void {
        self.value.deinit(a);
    }
};

const Identifier = struct {
    token: l.Token,

    pub fn format(
        self: Identifier,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = options;
        _ = fmt;
        try writer.print("{s}", .{self.token});
    }
};

const Err = error{
    Parse,
    UnexpectedToken,
    UnexpectedStatement,
};

const PrefixExprFn = fn (*Parser) std.mem.Allocator.Error!Expression;
const InfixExprFn = fn (*Parser, Expression) std.mem.Allocator.Error!Expression;

const Presedence = enum(u8) {
    lowest = 0,
    equals,
    ltgt,
    sum,
    product,
    prefix,
    call,
};

pub const Parser = struct {
    const Self = @This();
    lex: l.Lexer,
    allocator: std.mem.Allocator,
    curr_t: l.Token,
    peek_t: l.Token,

    errors: std.ArrayList(Err),
    prefixFn: std.StringHashMap(*const PrefixExprFn),
    infixFn: std.StringHashMap(*const InfixExprFn),

    pub fn init(allocator: std.mem.Allocator, lexer: l.Lexer) !Self {
        var p = Parser{
            .lex = lexer,
            .allocator = allocator,
            .curr_t = l.Token.eof,
            .peek_t = l.Token.eof,
            .errors = std.ArrayList(Err).init(allocator),
            .prefixFn = std.StringHashMap(*const PrefixExprFn).init(allocator),
            .infixFn = std.StringHashMap(*const InfixExprFn).init(allocator),
        };

        try p.prefixFn.put("ident", &parseIdentifier);

        p.nextToken();
        p.nextToken();
        return p;
    }

    pub fn deinit(self: *Self) void {
        self.errors.deinit();
        self.prefixFn.deinit();
        self.infixFn.deinit();
    }

    pub fn parse(self: *Self) !Program {
        var statements = std.ArrayList(Statement).init(self.allocator);

        while (self.curr_t != l.Token.eof) {
            try statements.append(try self.parseStatement());
            self.nextToken();
        }

        return Program{
            .statements = statements,
            .allocator = self.allocator,
        };
    }

    fn parseStatement(self: *Self) !Statement {
        switch (self.curr_t) {
            l.Token.let_literal => return self.parseLetStatement(),
            l.Token.return_literal => return self.parseReturnStatement(),
            else => return self.parseExpressionStatement(),
        }
    }

    fn parseLetStatement(self: *Self) !Statement {
        if (!try self.expectPeek("ident")) {
            return Err.Parse;
        }

        const name = Identifier{ .token = self.curr_t };

        if (!try self.expectPeek("assign")) {
            return Err.Parse;
        }

        var stmt = try self.allocator.create(LetStatement);
        stmt.identifier = name;
        stmt.value = Expression.default;

        while (self.curr_t != l.Token.semicolon) {
            self.nextToken();
        }

        return Statement{ .let_statement = stmt };
    }

    fn parseReturnStatement(self: *Self) !Statement {
        self.nextToken();

        var stmt = try self.allocator.create(ReturnStatment);

        while (self.curr_t != l.Token.semicolon) {
            self.nextToken();
        }

        return Statement{ .return_statement = stmt };
    }

    fn parseExpressionStatement(self: *Self) !Statement {
        const expr = try self.parseExpression(.lowest);

        while (self.curr_t != l.Token.semicolon) {
            self.nextToken();
        }

        var stmt = try self.allocator.create(ExpressionStatement);
        stmt.value = expr;
        return Statement{ .expr_statement = stmt };
    }

    fn parseExpression(self: *Self, presedence: Presedence) !Expression {
        _ = presedence;
        const prefix = self.prefixFn.get(self.curr_t.tag());
        var left = if (prefix) |func| blk: {
            break :blk try func(self);
        } else {
            return Expression.default;
        };

        return left;
    }

    fn parseIdentifier(self: *Self) !Expression {
        return Expression{ .identifier = Identifier{ .token = self.curr_t } };
    }

    fn expectPeek(self: *Self, tag: []const u8) !bool {
        if (std.mem.eql(u8, self.peek_t.tag(), tag)) {
            self.nextToken();
            return true;
        } else {
            try self.errors.append(Err.UnexpectedToken);
        }
        return false;
    }

    fn nextToken(self: *Self) void {
        self.curr_t = self.peek_t;
        self.peek_t = self.lex.nextToken();
    }
};

const t = std.testing;
const t_allocator = t.allocator;

fn letStatement(a: std.mem.Allocator, identifier: []const u8) !Statement {
    var statement = try a.create(LetStatement);
    statement.identifier = Identifier{ .token = l.Token{ .ident = identifier } };
    statement.value = Expression.default;

    return Statement{ .let_statement = statement };
}

fn exprStatement(a: std.mem.Allocator, value: []const u8) !Statement {
    var statement = try a.create(ExpressionStatement);

    statement.value = Expression{ .identifier = Identifier{ .token = l.Token{ .ident = value } } };

    return Statement{ .expr_statement = statement };
}

test "let statement" {
    const input =
        \\let x = 5;
        \\let y = 10;
        \\let foobar = 83834;
    ;

    var lexer = l.Lexer.init(input);
    var parser = try Parser.init(t_allocator, lexer);
    defer parser.deinit();

    var program = try parser.parse();
    defer program.destroy();

    const statements_len: u64 = @as(u64, program.statements.items.len);
    const expected_len: u64 = 3;
    try t.expectEqual(expected_len, statements_len);

    const expected_statements = [_]Statement{
        try letStatement(t_allocator, "x"),
        try letStatement(t_allocator, "y"),
        try letStatement(t_allocator, "foobar"),
    };

    defer for (expected_statements) |value| {
        value.deinit(t_allocator);
        switch (value) {
            .let_statement => |stmt| {
                t_allocator.destroy(stmt);
            },
            else => unreachable,
        }
    };

    for (program.statements.items, 0..) |value, idx| {
        try t.expectEqualDeep(expected_statements[idx].let_statement, value.let_statement);
    }
}

test "identifier" {
    const input = "foobar;";

    var lex = l.Lexer.init(input);
    var parser = try Parser.init(t_allocator, lex);
    defer parser.deinit();

    var program = try parser.parse();
    defer program.destroy();

    const statements_len: u64 = @as(u64, program.statements.items.len);
    const errors_len: u64 = @as(u64, parser.errors.items.len);

    try t.expect(errors_len == 0);
    try t.expect(statements_len == 1);

    const expected_statements = [_]Statement{
        try exprStatement(t_allocator, "foobar"),
    };

    defer for (expected_statements) |value| {
        value.deinit(t_allocator);
        switch (value) {
            .expr_statement => |stmt| {
                t_allocator.destroy(stmt);
            },
            else => unreachable,
        }
    };

    for (program.statements.items, 0..) |value, idx| {
        try t.expectEqualDeep(expected_statements[idx].expr_statement, value.expr_statement);
    }
}
