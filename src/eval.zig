const p = @import("parser.zig");
const std = @import("std");

pub const Environment = struct {
    const Self = @This();
    allocator: std.mem.Allocator,
    store: std.StringArrayHashMap(Object),
    outer: ?*Self,
    inner: std.ArrayList(*Environment),

    pub fn init(a: std.mem.Allocator) !*Environment {
        var env = try a.create(Environment);
        env.allocator = a;
        env.store = std.StringArrayHashMap(Object).init(a);
        env.outer = null;
        env.inner = std.ArrayList(*Environment).init(a);

        return env;
    }

    pub fn deinit(self: *Self) void {
        for (self.inner.items) |inner_env| {
            inner_env.deinit();
        }
        self.inner.deinit();
        self.store.deinit();
        self.outer = null;
        self.allocator.destroy(self);
    }

    fn get(self: *Self, name: []const u8) ?Object {
        if (self.store.get(name)) |value| {
            return value;
        } else if (self.outer) |env| {
            return env.get(name);
        }
        return null;
    }

    fn set(self: *Self, name: []const u8, value: Object) !void {
        return self.store.put(name, value);
    }

    fn sub(self: *Self) !*Self {
        var env = try init(self.allocator);
        env.outer = self;

        try self.inner.append(env);

        return env;
    }
};

const Object = union(enum) {
    const Self = @This();

    integer: i64,
    boolean: bool,
    null_obj,
    return_value: *const ReturnValue,
    error_value: ErrorValue,
    function: Function,

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = options;
        _ = fmt;

        switch (self) {
            .integer => |i| try writer.print("{d}", .{i}),
            .boolean => |i| try writer.print("{}", .{i}),
            .null_obj => try writer.print("null", .{}),
            .return_value => |v| try writer.print("Return: {s}", .{v.value}),
            .error_value => |e| try writer.print("Error: {s}", .{e.message}),
            .function => |f| try writer.print("{s}", .{f}),
        }
    }

    fn clone(self: Self, a: std.mem.Allocator) !Self {
        switch (self) {
            .function => |f| return Object{ .function = try f.clone(a) },
            else => return self,
        }
    }
};

const ReturnValue = struct { value: Object };
const ErrorValue = struct {
    message: []const u8,
};

const Function = struct {
    const Self = @This();

    func: *p.Function,
    // parameters: std.ArrayList(p.Identifier),
    // body: p.Statement,
    env: *Environment,

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = writer;
        _ = self;
        _ = options;
        _ = fmt;
    }
};

pub fn evalProgram(node: p.Program, env: *Environment) !Object {
    var result: Object = undefined;

    for (node.statements.items) |value| {
        result = try evalStatement(value, env);
        switch (result) {
            .return_value => |obj| return obj.value,
            .error_value => return result,
            else => {},
        }
    }

    return result;
}

fn evalStatement(node: p.Statement, env: *Environment) !Object {
    switch (node) {
        .let_statement => |stmt| {
            const value = try evalExpression(stmt.value, env);
            switch (value) {
                .error_value => return value,
                else => {
                    try env.set(stmt.identifier.token.ident, value);
                    return value;
                },
            }
        },
        .return_statement => |stmt| {
            const obj = try evalExpression(stmt.value, env);
            return Object{ .return_value = &ReturnValue{ .value = obj } };
        },
        .expr_statement => |stmt| {
            return try evalExpression(stmt.value, env);
        },
        .block_statement => |stmt| {
            var result: Object = undefined;

            for (stmt.statements.items) |value| {
                result = try evalStatement(value, env);

                switch (result) {
                    .return_value, .error_value => return result,
                    else => {},
                }
            }

            return result;
        },
    }
}

const true_obj = Object{ .boolean = true };
const false_obj = Object{ .boolean = false };

fn evalExpression(node: p.Expression, env: *Environment) std.mem.Allocator.Error!Object {
    switch (node) {
        .integer => |e| return Object{ .integer = e.value },
        .boolean => |e| return if (e.value) true_obj else false_obj,
        .identifier => |e| {
            if (env.get(e.token.ident)) |value| {
                return value;
            }
            return Object{ .error_value = ErrorValue{ .message = "Unknown identifier" } };
        },
        .prefix => |e| return try evalPrefixExpression(e, env),
        .infix => |e| return try evalInvixExpression(e, env),
        .ifelse => |e| {
            const condition = try evalExpression(e.condition, env);

            switch (condition) {
                .error_value => return condition,
                else => {},
            }

            if (isTrue(condition)) {
                return try evalStatement(e.consequence, env);
            } else if (e.alternative) |alt| {
                return try evalStatement(alt, env);
            } else {
                return .null_obj;
            }
        },
        .function => |e| {
            // const params = e.parameters;
            // const body = e.body;

            return Object{
                .function = Function{
                    // .parameters = params,
                    // .body = body,
                    .func = try e.clone(env.allocator),
                    .env = env,
                },
            };
        },
        .call => |e| {
            const func = try evalExpression(e.function, env);

            switch (func) {
                .error_value => return func,
                else => {},
            }

            const args = try evalExpressions(e.args, env);
            defer args.deinit();

            if (args.items.len == 1) {
                const item = args.getLast();
                switch (item) {
                    .error_value => return item,
                    else => {},
                }
            }

            return applyFunction(func, args, env);
        },
        else => return .null_obj,
    }
}

fn evalExpressions(exprs: std.ArrayList(p.Expression), env: *Environment) std.mem.Allocator.Error!std.ArrayList(Object) {
    var objs = std.ArrayList(Object).init(env.allocator);
    errdefer objs.deinit();

    for (exprs.items) |value| {
        const obj = try evalExpression(value, env);
        switch (obj) {
            .error_value => {
                objs.deinit();

                var err = std.ArrayList(Object).init(env.allocator);
                errdefer err.deinit();

                try err.append(obj);

                return err;
            },
            else => {
                try objs.append(obj);
            },
        }
    }

    return objs;
}

fn evalPrefixExpression(expr: *p.PrefixExpression, env: *Environment) std.mem.Allocator.Error!Object {
    const right = try evalExpression(expr.right, env);

    switch (right) {
        .error_value => return right,
        else => {},
    }

    return switch (expr.operator) {
        .bang => switch (right) {
            .boolean => |e| if (e) false_obj else true_obj,
            .null_obj => true_obj,
            else => false_obj,
        },
        .minus => if (std.mem.eql(u8, @tagName(right), "integer")) .null_obj else Object{ .integer = -right.integer },
        else => .null_obj,
    };
}

fn evalInvixExpression(expr: *p.InfixExpression, env: *Environment) std.mem.Allocator.Error!Object {
    const left = try evalExpression(expr.left, env);

    switch (left) {
        .error_value => return left,
        else => {},
    }

    const right = try evalExpression(expr.right, env);

    switch (right) {
        .error_value => return right,
        else => {},
    }

    if (std.mem.eql(u8, @tagName(left), "integer") and std.mem.eql(u8, @tagName(right), "integer")) {
        return switch (expr.operator) {
            .plus => Object{ .integer = left.integer + right.integer },
            .minus => Object{ .integer = left.integer - right.integer },
            .asterisk => Object{ .integer = left.integer * right.integer },
            .slash => Object{ .integer = @divFloor(left.integer, right.integer) },
            .lt => if (left.integer < right.integer) true_obj else false_obj,
            .lte => if (left.integer <= right.integer) true_obj else false_obj,
            .gt => if (left.integer > right.integer) true_obj else false_obj,
            .gte => if (left.integer >= right.integer) true_obj else false_obj,
            .eq => if (left.integer == right.integer) true_obj else false_obj,
            .noteq => if (left.integer != right.integer) true_obj else false_obj,
            else => Object{ .error_value = ErrorValue{ .message = "Unsupported opreator for type [integer]" } },
        };
    } else if (std.mem.eql(u8, @tagName(left), "boolean") and std.mem.eql(u8, @tagName(right), "boolean")) {
        return switch (expr.operator) {
            .eq => if (left.boolean == right.boolean) true_obj else false_obj,
            .noteq => if (left.boolean != right.boolean) true_obj else false_obj,
            else => Object{ .error_value = ErrorValue{ .message = "Unsupported opreator for type [boolean]" } },
        };
    } else if (!std.mem.eql(u8, @tagName(left), @tagName(right))) {
        return Object{ .error_value = ErrorValue{ .message = "Incompatible types" } };
    } else return Object.null_obj;
}

fn applyFunction(func: Object, args: std.ArrayList(Object), env: *Environment) std.mem.Allocator.Error!Object {
    _ = env;
    var func_obj: Function = undefined;

    switch (func) {
        .function => |f| {
            func_obj = f;
        },
        else => return Object{ .error_value = ErrorValue{ .message = "not a function" } },
    }

    var closure = try func_obj.env.sub();

    for (func_obj.func.parameters.items, 0..) |value, idx| {
        try closure.set(value.token.ident, args.items[idx]);
    }

    std.debug.print("Fn: {s}\n", .{func.function.func.body});
    std.debug.print("Store: {any}\n", .{closure.store.keys()});
    if (closure.outer) |outer| {
        std.debug.print("Outer: {any}\n", .{outer.store.keys()});
        if (outer.outer) |o_outer| {
            std.debug.print("Outer-outer: {any}\n", .{o_outer.store.keys()});
        }
    }
    std.debug.print("=============\n\n", .{});

    const result = try evalStatement(func_obj.func.body, closure);
    switch (result) {
        .return_value => |ret| return ret.value,
        else => return result,
    }
}

fn isTrue(obj: Object) bool {
    switch (obj) {
        .boolean => |b| return b,
        .integer => return true,
        .null_obj => return false,
        else => return true,
    }
}

const l = @import("lexer.zig");
const t = std.testing;

test "let exression" {
    const input =
        \\let x = 10;
        \\
        \\let f = fn(inner, outer) {
        \\  return inner() + outer();
        \\}
        \\
        \\let i = fn() { return 5; }
        \\let j = fn() { return 4;}
        \\
        \\return f(i, j);
        \\
    ;

    var lex = l.Lexer.init(input);
    var parser = try p.Parser.init(t.allocator, lex);
    defer parser.deinit();
    var program = try parser.parse();
    defer program.destroy();

    var env = try Environment.init(t.allocator);
    defer env.deinit();

    var result = try evalProgram(program, env);

    std.debug.print("{any}\n", .{result});
}
