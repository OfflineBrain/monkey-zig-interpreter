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
                a.destroy(value);
            },
            .return_statement => |value| {
                value.deinit(a);
                a.destroy(value);
            },
            .expr_statement => |value| {
                value.deinit(a);
                a.destroy(value);
            },
        }
    }

    pub fn format(
        self: Statement,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = options;
        _ = fmt;
        switch (self) {
            .let_statement => |e| try writer.print("{s}", .{e}),
            .return_statement => |e| try writer.print("{s}", .{e}),
            .expr_statement => |e| try writer.print("{s}", .{e}),
        }
    }
};

const Expression = union(enum) {
    const Self = @This();

    identifier: Identifier,
    integer: Integer,
    prefix: *PrefixExpression,
    infix: *InfixExpression,
    default,
    fn deinit(self: Expression, a: std.mem.Allocator) void {
        switch (self) {
            .prefix => |expr| {
                expr.deinit(a);
                a.destroy(expr);
            },
            .infix => |expr| {
                expr.deinit(a);
                a.destroy(expr);
            },
            else => {},
        }
    }

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = options;
        _ = fmt;
        switch (self) {
            .identifier => |e| try writer.print("{s}", .{e}),
            .integer => |e| try writer.print("{s}", .{e}),
            .prefix => |e| try writer.print("{s}", .{e}),
            .infix => |e| try writer.print("{s}", .{e}),
            else => try writer.print("DEFAULT", .{}),
        }
    }
};

const Program = struct {
    statements: std.ArrayList(Statement),
    allocator: std.mem.Allocator,

    pub fn destroy(program: *Program) void {
        defer program.statements.deinit();
        defer for (program.statements.items) |value| {
            value.deinit(t_allocator);
        };
    }

    pub fn format(
        self: Program,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = options;
        _ = fmt;

        for (self.statements.items) |value| {
            try writer.print("{s}", .{value});
        }
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
    const Self = @This();
    token: l.Token,

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = options;
        _ = fmt;
        try writer.print("{s}", .{self.token});
    }
};

const Integer = struct {
    const Self = @This();
    token: l.Token,

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = options;
        _ = fmt;
        try writer.print("{s}", .{self.token});
    }
};

const PrefixExpression = struct {
    const Self = @This();
    operator: l.Token,
    right: Expression,

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = options;
        _ = fmt;
        try writer.print("({s}{s})", .{ self.operator, self.right });
    }

    fn deinit(self: Self, a: std.mem.Allocator) void {
        self.right.deinit(a);
    }
};

const InfixExpression = struct {
    const Self = @This();
    operator: l.Token,
    left: Expression,
    right: Expression,

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = options;
        _ = fmt;
        try writer.print("({s} {s} {s})", .{ self.left, self.operator, self.right });
    }

    fn deinit(self: Self, a: std.mem.Allocator) void {
        self.left.deinit(a);
        self.right.deinit(a);
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

    fn int(self: Presedence) u8 {
        return @intFromEnum(self);
    }
};

fn mapPresedence(token: l.Token) Presedence {
    return switch (token) {
        .eq, .noteq => .equals,
        .lt, .lte, .gt, .gte => .ltgt,
        .plus, .minus => .sum,
        .asterisk, .slash => .product,
        else => .lowest,
    };
}

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
        try p.prefixFn.put("int", &parseInteger);
        try p.prefixFn.put("bang", &parsePrefixExpression);
        try p.prefixFn.put("minus", &parsePrefixExpression);

        try p.infixFn.put("plus", &parseInfixExpression);
        try p.infixFn.put("minus", &parseInfixExpression);
        try p.infixFn.put("asterisk", &parseInfixExpression);
        try p.infixFn.put("slash", &parseInfixExpression);
        try p.infixFn.put("eq", &parseInfixExpression);
        try p.infixFn.put("noteq", &parseInfixExpression);
        try p.infixFn.put("lt", &parseInfixExpression);
        try p.infixFn.put("lte", &parseInfixExpression);
        try p.infixFn.put("gt", &parseInfixExpression);
        try p.infixFn.put("gte", &parseInfixExpression);

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

        while (self.curr_t != l.Token.semicolon and self.curr_t != l.Token.eof) {
            self.nextToken();
        }

        var stmt = try self.allocator.create(ExpressionStatement);
        stmt.value = expr;
        return Statement{ .expr_statement = stmt };
    }

    fn parseExpression(self: *Self, presedence: Presedence) !Expression {
        const prefix = self.prefixFn.get(self.curr_t.tag());
        var left = if (prefix) |func| blk: {
            break :blk try func(self);
        } else {
            return Expression.default;
        };

        while (self.peek_t != l.Token.semicolon and presedence.int() < self.peekPresedence().int()) {
            const infix = self.infixFn.get(self.peek_t.tag());
            left = if (infix) |func| blk: {
                self.nextToken();
                break :blk try func(self, left);
            } else {
                return Expression.default;
            };
        }

        // std.debug.print("{s}\n", .{left});
        return left;
    }

    fn parseIdentifier(self: *Self) !Expression {
        return Expression{ .identifier = Identifier{ .token = self.curr_t } };
    }

    fn parseInteger(self: *Self) !Expression {
        return Expression{ .integer = Integer{ .token = self.curr_t } };
    }

    fn parsePrefixExpression(self: *Self) !Expression {
        var expr = try self.allocator.create(PrefixExpression);
        expr.operator = self.curr_t;

        self.nextToken();

        expr.right = try self.parseExpression(.prefix);

        return Expression{ .prefix = expr };
    }

    fn parseInfixExpression(self: *Self, left: Expression) !Expression {
        var expr = try self.allocator.create(InfixExpression);
        expr.left = left;
        expr.operator = self.curr_t;

        const p = self.currPresedence();
        self.nextToken();
        expr.right = try self.parseExpression(p);

        return Expression{ .infix = expr };
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

    fn currPresedence(self: Self) Presedence {
        return mapPresedence(self.curr_t);
    }

    fn peekPresedence(self: Self) Presedence {
        return mapPresedence(self.peek_t);
    }
};

const t = std.testing;
const t_allocator = t.allocator;
const expectEqualDeep = @import("testing/eql.zig").expectEqualDeep;

fn letStatement(a: std.mem.Allocator, identifier: []const u8) !Statement {
    var statement = try a.create(LetStatement);
    statement.identifier = Identifier{ .token = l.Token{ .ident = identifier } };
    statement.value = Expression.default;

    return Statement{ .let_statement = statement };
}

fn exprStatement(a: std.mem.Allocator, value: Expression) !Statement {
    var statement = try a.create(ExpressionStatement);

    statement.value = value;

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

    const errors_len: u64 = @as(u64, parser.errors.items.len);
    try t.expect(errors_len == 0);

    const expected_statements = [_]Statement{
        try letStatement(t_allocator, "x"),
        try letStatement(t_allocator, "y"),
        try letStatement(t_allocator, "foobar"),
    };

    defer for (expected_statements) |value| {
        value.deinit(t_allocator);
    };

    for (program.statements.items, 0..) |value, idx| {
        try expectEqualDeep(expected_statements[idx].let_statement, value.let_statement);
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
        try exprStatement(t_allocator, Expression{ .identifier = Identifier{ .token = l.Token{ .ident = "foobar" } } }),
    };

    defer for (expected_statements) |value| {
        value.deinit(t_allocator);
    };

    for (program.statements.items, 0..) |value, idx| {
        try expectEqualDeep(expected_statements[idx].expr_statement, value.expr_statement);
    }
}

test "integers" {
    const input = "5;";

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
        try exprStatement(t_allocator, Expression{ .integer = Integer{ .token = l.Token{ .int = "5" } } }),
    };

    defer for (expected_statements) |value| {
        value.deinit(t_allocator);
    };

    for (program.statements.items, 0..) |value, idx| {
        try expectEqualDeep(expected_statements[idx].expr_statement, value.expr_statement);
    }
}

fn testStatement(
    input: []const u8,
    expected: Statement,
) !void {
    var lex = l.Lexer.init(input);
    var parser = try Parser.init(t_allocator, lex);
    defer parser.deinit();

    var program = try parser.parse();
    defer program.destroy();

    const statements_len: u64 = @as(u64, program.statements.items.len);
    const errors_len: u64 = @as(u64, parser.errors.items.len);

    try t.expect(errors_len == 0);
    try t.expect(statements_len == 1);

    try expectEqualDeep(expected, program.statements.items[0]);
}

test "prefix expression" {
    const input = [_][]const u8{ "!5;", "-15;" };
    const expected = [_]Statement{
        Statement{
            .expr_statement = blk: {
                var stmt = try t_allocator.create(ExpressionStatement);
                var expr = try t_allocator.create(PrefixExpression);
                expr.operator = l.Token.bang;
                expr.right = Expression{ .integer = Integer{ .token = l.Token{ .int = "5" } } };
                stmt.value = Expression{ .prefix = expr };
                break :blk stmt;
            },
        },
        Statement{
            .expr_statement = blk: {
                var stmt = try t_allocator.create(ExpressionStatement);
                var expr = try t_allocator.create(PrefixExpression);
                expr.operator = l.Token.minus;
                expr.right = Expression{ .integer = Integer{ .token = l.Token{ .int = "15" } } };
                stmt.value = Expression{ .prefix = expr };
                break :blk stmt;
            },
        },
    };

    defer for (expected) |stmt| {
        stmt.deinit(t_allocator);
    };

    for (input, 0..) |value, i| {
        try testStatement(value, expected[i]);
    }
}

fn infixExpression(opr: l.Token, left: Expression, right: Expression) !Expression {
    var expr = try t_allocator.create(InfixExpression);
    expr.operator = opr;
    expr.left = left;
    expr.right = right;
    return Expression{ .infix = expr };
}

test "infix expressions" {
    const input = [_][]const u8{
        "5 + 5;",
        "5 - 5;",
        "5 * 5;",
        "5 / 5;",
        "5 > 5;",
        "5 >= 5;",
        "5 < 5;",
        "5 <= 5;",
        "5 == 5;",
        "5 != 5;",
    };

    const expected = [_]Statement{
        Statement{
            .expr_statement = blk: {
                var stmt = try t_allocator.create(ExpressionStatement);
                var expr = Expression{ .integer = Integer{ .token = l.Token{ .int = "5" } } };
                stmt.value = try infixExpression(l.Token.plus, expr, expr);
                break :blk stmt;
            },
        },
        Statement{
            .expr_statement = blk: {
                var stmt = try t_allocator.create(ExpressionStatement);
                var expr = Expression{ .integer = Integer{ .token = l.Token{ .int = "5" } } };
                stmt.value = try infixExpression(l.Token.minus, expr, expr);
                break :blk stmt;
            },
        },
        Statement{
            .expr_statement = blk: {
                var stmt = try t_allocator.create(ExpressionStatement);
                var expr = Expression{ .integer = Integer{ .token = l.Token{ .int = "5" } } };
                stmt.value = try infixExpression(l.Token.asterisk, expr, expr);
                break :blk stmt;
            },
        },
        Statement{
            .expr_statement = blk: {
                var stmt = try t_allocator.create(ExpressionStatement);
                var expr = Expression{ .integer = Integer{ .token = l.Token{ .int = "5" } } };
                stmt.value = try infixExpression(l.Token.slash, expr, expr);
                break :blk stmt;
            },
        },
        Statement{
            .expr_statement = blk: {
                var stmt = try t_allocator.create(ExpressionStatement);
                var expr = Expression{ .integer = Integer{ .token = l.Token{ .int = "5" } } };
                stmt.value = try infixExpression(l.Token.gt, expr, expr);
                break :blk stmt;
            },
        },
        Statement{
            .expr_statement = blk: {
                var stmt = try t_allocator.create(ExpressionStatement);
                var expr = Expression{ .integer = Integer{ .token = l.Token{ .int = "5" } } };
                stmt.value = try infixExpression(l.Token.gte, expr, expr);
                break :blk stmt;
            },
        },
        Statement{
            .expr_statement = blk: {
                var stmt = try t_allocator.create(ExpressionStatement);
                var expr = Expression{ .integer = Integer{ .token = l.Token{ .int = "5" } } };
                stmt.value = try infixExpression(l.Token.lt, expr, expr);
                break :blk stmt;
            },
        },
        Statement{
            .expr_statement = blk: {
                var stmt = try t_allocator.create(ExpressionStatement);
                var expr = Expression{ .integer = Integer{ .token = l.Token{ .int = "5" } } };
                stmt.value = try infixExpression(l.Token.lte, expr, expr);
                break :blk stmt;
            },
        },
        Statement{
            .expr_statement = blk: {
                var stmt = try t_allocator.create(ExpressionStatement);
                var expr = Expression{ .integer = Integer{ .token = l.Token{ .int = "5" } } };
                stmt.value = try infixExpression(l.Token.eq, expr, expr);
                break :blk stmt;
            },
        },
        Statement{
            .expr_statement = blk: {
                var stmt = try t_allocator.create(ExpressionStatement);
                var expr = Expression{ .integer = Integer{ .token = l.Token{ .int = "5" } } };
                stmt.value = try infixExpression(l.Token.noteq, expr, expr);
                break :blk stmt;
            },
        },
    };

    defer for (expected) |stmt| {
        stmt.deinit(t_allocator);
    };

    for (input, 0..) |value, i| {
        try testStatement(value, expected[i]);
    }
}

fn testStatementString(input: []const u8, output: []const u8) !void {
    var lex = l.Lexer.init(input);
    var parser = try Parser.init(t_allocator, lex);
    defer parser.deinit();

    var program = try parser.parse();
    defer program.destroy();

    const errors_len: u64 = @as(u64, parser.errors.items.len);

    try t.expect(errors_len == 0);

    const stmt_str = try std.fmt.allocPrint(t_allocator, "{s}", .{program});
    defer t_allocator.free(stmt_str);

    try t.expectEqualStrings(output, stmt_str);
}

test "complex infix expressions" {
    const input = [_][]const u8{
        "-a * b",
        "!-a",
        "a + b + c",
        "a + b - c",
        "a * b * c",
        "a * b / c",
        "a + b * c + d / e - f",
        "3 + 4; -5 * 5",
        "5 > 4 == 3 < 4",
        "5 >= 4 != 3 <= 4",
        "3 + 4 * 5 == 3 * 1 + 4 * 5;",
    };

    const output = [_][]const u8{
        "((-a) * b)",
        "(!(-a))",
        "((a + b) + c)",
        "((a + b) - c)",
        "((a * b) * c)",
        "((a * b) / c)",
        "(((a + (b * c)) + (d / e)) - f)",
        "(3 + 4)((-5) * 5)",
        "((5 > 4) == (3 < 4))",
        "((5 >= 4) != (3 <= 4))",
        "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
    };

    for (input, 0..) |value, i| {
        try testStatementString(value, output[i]);
    }
}
