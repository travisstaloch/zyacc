const std = @import("std");
const m = @import("mecha");
const assert = std.debug.assert;
const mem = std.mem;
const Allocator = mem.Allocator;
const print = std.debug.print;

pub const Production = struct {
    name: []const u8 = &.{},
    rule: Rule,
    pub fn deinit(prod: Production, allocator: Allocator) void {
        prod.rule.root.deinit(allocator);
    }
};

pub const Rule = struct { source: []const u8 = &.{}, root: Symbol };
pub const Symbol = union(enum) {
    comment,
    dot,
    // newline,
    group_end,
    char_lit: []const u8,
    str_lit: []const u8,
    sqbkt_lit: []const u8,
    _terminal,
    name: []const u8,

    optional: *const Symbol,
    some: *const Symbol,
    many: *const Symbol,
    not: *const Symbol,
    _operator,
    choice: []const Symbol,
    seq: []const Symbol,
    group: []const Symbol,

    pub const Tag = std.meta.Tag(Symbol);

    pub fn deinit(sym: Symbol, allocator: Allocator) void {
        switch (sym) {
            .name, .char_lit, .str_lit, .sqbkt_lit, .dot, .group_end, .comment, ._operator, ._terminal => {},
            .optional, .some, .many, .not => |child| {
                child.deinit(allocator);
                allocator.destroy(child);
            },
            .group, .seq, .choice => |children| {
                for (children) |child| child.deinit(allocator);
                allocator.free(children);
            },
        }
    }

    pub fn text(sym: Symbol) []const u8 {
        switch (sym) {
            .name, .char_lit, .str_lit, .sqbkt_lit => |s| return s,
            .optional, .some, .many, .not => |child| return text(child.*),
            else => unreachable,
        }
    }

    pub fn format(s: Symbol, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        const seq_separator = " ";
        const bracket_choices = false;
        const bracket_literals = true;
        const show_tag = false;
        if (show_tag) _ = try writer.write(@tagName(s));
        if (show_tag) _ = try writer.write(": ");
        switch (s) {
            .name => |x| _ = try writer.write(x),
            .char_lit => |x| {
                if (bracket_literals) _ = try writer.writeByte('\'');
                _ = try writer.write(x);
                if (bracket_literals) _ = try writer.writeByte('\'');
            },
            .str_lit => |x| {
                if (bracket_literals) _ = try writer.writeByte('"');
                _ = try writer.write(x);
                if (bracket_literals) _ = try writer.writeByte('"');
            },
            .sqbkt_lit => |x| {
                if (bracket_literals) _ = try writer.writeByte('[');
                _ = try writer.write(x);
                if (bracket_literals) _ = try writer.writeByte(']');
            },
            .optional => |x| {
                try writer.print("{}", .{x});
                _ = try writer.write("?");
            },
            .some => |x| {
                try writer.print("{}", .{x});
                _ = try writer.write("+");
            },
            .many => |x| {
                try writer.print("{}", .{x.*});
                _ = try writer.write("*");
            },
            .not => |x| {
                _ = try writer.write("!");
                try writer.print("{}", .{x.*});
            },
            .dot => _ = try writer.write("."),
            .group => |x| {
                _ = try writer.write("(");
                for (x) |sym, i| {
                    const sep: []const u8 = if (i == 0) "" else seq_separator;
                    _ = try writer.write(sep);
                    try writer.print("{}", .{sym});
                }
                _ = try writer.write(")");
            },
            .seq => |x| {
                for (x) |sym, i| {
                    const sep: []const u8 = if (i == 0) "" else seq_separator;
                    _ = try writer.write(sep);
                    try writer.print("{}", .{sym});
                }
            },
            .choice => |x| {
                if (bracket_choices) _ = try writer.write("[");
                for (x) |sym, i| {
                    const sep: []const u8 = if (i == 0) "" else " / ";
                    _ = try writer.write(sep);
                    try writer.print("{}", .{sym});
                }
                if (bracket_choices) _ = try writer.write("]");
            },
            .group_end => unreachable,
            // .newline => _ = try writer.writeByte('\n'),
            .comment => unreachable,
            ._operator => unreachable,
            ._terminal => unreachable,
        }
    }
};

const showtrace = true;
fn trace(comptime fmt: []const u8, args: anytype) void {
    // @compileLog(comptime std.fmt.comptimePrint(fmt, args));
    // @compileError(comptime std.fmt.comptimePrint(fmt, args));
    if (@hasDecl(@This(), "showtrace"))
        std.debug.print(fmt, args);
}

fn todo() noreturn {
    @panic("TODO");
}

pub const p = struct {
    pub fn char(comptime c: u8) m.Parser(u8) {
        const Res = m.Result(u8);
        return struct {
            fn res(_: Allocator, s: []const u8) m.Error!Res {
                return if (s.len > 0 and s[0] == c)
                    Res{ .value = c, .rest = s[1..] }
                else
                    error.ParserFailed;
            }
        }.res;
    }
    pub const alphanum = m.oneOf(.{ m.ascii.alphanum, char('_') });
    pub const manyalphanum = m.many(alphanum, .{ .collect = false });
    pub const ws = m.discard(m.many(m.ascii.space, .{ .collect = false }));
    pub const somenonws = m.discard(m.many(m.ascii.not(m.ascii.space), .{ .collect = false, .min = 1 }));
    // pub const manyspacenotnl = m.many(m.oneOf(.{
    //     char(' '),
    //     char('\r'),
    //     char('\t'),
    //     char(std.ascii.control_code.VT),
    //     char(std.ascii.control_code.FF),
    // }), .{ .collect = false });
    // pub const somenl = m.many(char('\n'), .{ .collect = false, .min = 1 });
    pub const ident = m.asStr(m.combine(.{ m.ascii.alpha, manyalphanum, m.opt(char('\'')) }));
    pub const _larrow = m.string("<-");
    pub const larrow = m.discard(_larrow);
    pub const _many_non_larrow = m.many(m.ascii.not(larrow), .{ .collect = false });
    pub const many_non_larrow = m.combine(.{ ws, _many_non_larrow, larrow, ws });
    pub const many_non_nl = m.many(m.ascii.not(char('\n')), .{ .collect = false });
    pub const comment = m.asStr(m.combine(.{ char('#'), many_non_nl, m.opt(char('\n')) }));
    pub const ident_arrow = m.combine(.{ ws, ident, ws, larrow, ws });
    pub const ws_arrow = m.combine(.{ ws, larrow });
    // pub const many_not_ident_arrow = m.many(m.ascii.not(p.ident_arrow), .{ .collect = false });

    pub const quote = m.ascii.char('\'');
    pub const dquote = m.ascii.char('"');
    pub const choice = m.asStr(char('/'));
    pub const optional = m.asStr(char('?'));
    pub const many = m.asStr(char('*'));
    pub const some = m.asStr(char('+'));
    pub const group = m.asStr(char('('));
    pub const group_end = m.asStr(char(')'));
    pub const not = m.asStr(char('!'));
    pub const dot = m.asStr(char('.'));
    pub const sqbkt_l = m.ascii.char('[');
    pub const sqbkt_r = m.ascii.char(']');

    pub const escaped_char_lit = m.convert(u8, convEscapeCharLit, m.combine(.{ char('\\'), m.oneOf(.{ char('\''), char('n'), char('\\') }) }));
    pub const many_not_quote = m.many(m.oneOf(.{ escaped_char_lit, m.ascii.not(quote) }), .{ .collect = false, .min = 1 });
    pub const char_lit = m.combine(.{ quote, many_not_quote, quote });
    pub const escaped_str = m.convert(u8, convEscapeStrLit, m.combine(.{ char('\\'), m.oneOf(.{ char('"'), char('n'), char('\\') }) }));
    pub const many_not_dquote = m.many(m.oneOf(.{ escaped_str, m.ascii.not(dquote) }), .{ .collect = false, .min = 0 });
    pub const str_lit = m.combine(.{ dquote, many_not_dquote, dquote });
    pub const many_not_sqbkt_r = m.many(m.ascii.not(sqbkt_r), .{ .collect = false, .min = 1 });
    pub const sqbkt_lit = m.combine(.{ sqbkt_l, many_not_sqbkt_r, sqbkt_r });

    fn convEscapeCharLit(_: Allocator, res: [2]u8) !u8 {
        assert(res[0] == '\\');
        return switch (res[1]) {
            'n' => '\n',
            '\'' => '\'',
            '\\' => '\\',
            else => unreachable,
        };
    }
    fn convEscapeStrLit(_: Allocator, res: [2]u8) !u8 {
        assert(res[0] == '\\');
        return switch (res[1]) {
            'n' => '\n',
            '"' => '"',
            '\\' => '\\',
            else => unreachable,
        };
    }
};

fn copy(a: Allocator, sym: Symbol) !*Symbol {
    const cp = try a.create(Symbol);
    cp.* = sym;
    return cp;
}

// fn precedence(sym: Symbol.Tag) u8 {
//     // return @enumToInt(sym);
//     return switch (sym) {
//         .group => 100,
//         .choice => 80,
//         .seq => 60,
//         .many, .some, .optional, .not => 40,
//         .name, .char_lit, .str_lit, .sqbkt_lit, .dot => 20,
//         .comment, .newline => 10,
//         .group_end => 0,
//         ._terminal, ._operator => unreachable,
//         // else => ,
//     };
// }

fn isTerminal(sym: Symbol) bool {
    return switch (sym) {
        .many, .some, .optional, .not => |x| isTerminal(x.*),
        else => @enumToInt(sym) < @enumToInt(@as(Symbol, ._terminal)),
    };
}
// fn isOperator(sym: Symbol.Tag) bool {
//     return @enumToInt(sym) > @enumToInt(@as(Symbol, ._operator));
// }

pub const Context = struct {
    allr: Allocator,
    fallr: Allocator,
    rest: []const u8,
    pending_token: ?Token = null,

    pub fn eof(self: Context) bool {
        return self.rest.len == 0;
    }

    pub fn init(allr: Allocator, fallr: Allocator, rest: []const u8) Context {
        return .{
            .allr = allr,
            .fallr = fallr,
            .rest = rest,
        };
    }

    pub fn nextToken(ctx: *Context) ?Token {
        if (ctx.pending_token) |sym| {
            ctx.pending_token = null;
            return sym;
        }
        // trace("parseSym\n", .{});
        if (p.ws(ctx.fallr, ctx.rest)) |r| { // skip ws
            ctx.rest = r.rest;
        } else |_| {}

        const parsers = [_]std.meta.Tuple(&.{ m.Parser([]const u8), Symbol.Tag }){
            .{ p.ident, .name },
            .{ p.char_lit, .char_lit },
            .{ p.str_lit, .str_lit },
            .{ p.sqbkt_lit, .sqbkt_lit },
            .{ p.optional, .optional },
            .{ p.many, .many },
            .{ p.some, .some },
            .{ p.comment, .comment },
            .{ p.group_end, .group_end },
            .{ p.group, .group },
            .{ p.choice, .choice },
            .{ p.not, .not },
            .{ p.dot, .dot },
        };
        inline for (parsers) |pt| {
            const parser = pt[0];
            const tag = pt[1];
            if (parser(ctx.fallr, ctx.rest)) |r| {
                ctx.rest = r.rest;
                return Token.init(tag, r.value);
            } else |_| {}
        }

        return null;
    }
};

pub fn peekRest(s: []const u8, n: usize) []const u8 {
    const len = std.math.min(n, s.len);
    return s[0..len];
}

pub const Error = error{EmptySequence} || Allocator.Error;

pub const Token = struct {
    tag: Symbol.Tag,
    str: []const u8,
    pub fn init(tag: Symbol.Tag, str: []const u8) Token {
        return .{
            .tag = tag,
            .str = str,
        };
    }
};

pub fn nextSym(ctx: *Context) ?Token {
    if (ctx.pending_token) |sym| {
        ctx.pending_token = null;
        return sym;
    }
    if (p.ws(ctx.fallr, ctx.rest)) |r| { // skip ws
        ctx.rest = r.rest;
    } else |_| {}

    const parsers = [_]std.meta.Tuple(&.{ m.Parser([]const u8), Symbol.Tag }){
        .{ p.ident, .name },
        .{ p.char_lit, .char_lit },
        .{ p.str_lit, .str_lit },
        .{ p.sqbkt_lit, .sqbkt_lit },
        .{ p.optional, .optional },
        .{ p.many, .many },
        .{ p.some, .some },
        .{ p.comment, .comment },
        .{ p.group_end, .group_end },
        .{ p.group, .group },
        .{ p.choice, .choice },
        .{ p.not, .not },
        .{ p.dot, .dot },
    };
    // trace("parseSym '{s}'\n", .{ctx.peekRest(10)});
    inline for (parsers) |pt| {
        const parser = pt[0];
        const tag = pt[1];
        if (parser(ctx.fallr, ctx.rest)) |r| {
            if (tag == .name) {
                // TODO: remove this once the parser supports feeding tokens
                if (p.ws_arrow(ctx.fallr, r.rest)) |_| return null else |_| {}
            }
            ctx.rest = r.rest;
            return Token.init(tag, r.value);
        } else |_| {}
    }

    return null;
}
fn peekTop(items: anytype) ?std.meta.Child(@TypeOf(items)) {
    return if (items.len > 0) items[items.len - 1] else null;
}
pub fn symToSymbol(sym: Token) Symbol {
    return switch (sym.tag) {
        .name => .{ .name = sym.str },
        .char_lit => .{ .char_lit = sym.str },
        .str_lit => .{ .str_lit = sym.str },
        .sqbkt_lit => .{ .sqbkt_lit = sym.str },
        .group => .{ .group = &.{} },
        .choice => .{ .choice = &.{} },
        .group_end => .group_end,
        .many => .{ .many = undefined },
        .some => .{ .some = undefined },
        .optional => .{ .optional = undefined },
        .not => .{ .not = undefined },
        .comment => .comment,
        .dot => .dot,
        // .newline => .newline,
        else => {
            std.debug.print("tag {}\n", .{sym.tag});
            unreachable;
        },
    };
}

fn oneSym(ctx: *Context, sym: Token, seq: *std.ArrayListUnmanaged(Symbol)) !void {
    var symbol = symToSymbol(sym);
    // trace("symbol {s}:{}\n", .{ @tagName(symbol), symbol });
    switch (symbol) {
        .group => {
            var group = (try seqUntil(ctx, &.{.group_end})) orelse unreachable;

            const s = nextSym(ctx);
            // const ss: [2][]const u8 = if (s != null) .{ @tagName(s.?.tag), s.?.str } else .{ "null", "" };
            // trace("s {s}:{s}\n", .{ ss[0], ss[1] });
            assert(s == null or s.?.tag == .group_end);

            symbol.group = group.toOwnedSlice(ctx.allr);
            try seq.append(ctx.allr, symbol);
        },
        .many => {
            const last = seq.popOrNull() orelse unreachable;
            symbol.many = try copy(ctx.allr, last);
            try seq.append(ctx.allr, symbol);
        },
        .some => {
            const last = seq.popOrNull() orelse unreachable;
            symbol.some = try copy(ctx.allr, last);
            try seq.append(ctx.allr, symbol);
        },
        .optional => {
            const last = seq.popOrNull() orelse unreachable;
            symbol.optional = try copy(ctx.allr, last);
            try seq.append(ctx.allr, symbol);
        },
        .not => {
            const s = nextSym(ctx) orelse unreachable;
            symbol.not = try copy(ctx.allr, symToSymbol(s));
            try seq.append(ctx.allr, symbol);
        },
        .choice => {
            assert(seq.items.len > 0);
            const last = seq.pop();
            // trace("choice last {}\n", .{last});

            const terminators = &.{ .choice, .group_end };
            var next = (try seqUntil(ctx, terminators)) orelse unreachable;
            // trace("choice pending {}\n", .{ctx.pending_token});
            var choices = std.ArrayListUnmanaged(Symbol){};
            if (last == .choice) {
                try choices.appendSlice(ctx.allr, last.choice);
                ctx.allr.free(last.choice);
            } else try choices.append(ctx.allr, last);
            if (next.items.len > 1) {
                try choices.append(ctx.allr, .{ .seq = next.toOwnedSlice(ctx.allr) });
            } else {
                defer next.deinit(ctx.allr);
                try choices.append(ctx.allr, next.items[0]);
            }
            try seq.append(ctx.allr, .{ .choice = choices.toOwnedSlice(ctx.allr) });
        },
        .comment => {},
        else => {
            trace("appending {s}\n", .{@tagName(symbol)});
            try seq.append(ctx.allr, symbol);
        },
    }
}

fn seqUntil(ctx: *Context, terminators: []const Symbol.Tag) Error!?std.ArrayListUnmanaged(Symbol) {
    var seq = std.ArrayListUnmanaged(Symbol){};
    while (nextSym(ctx)) |sym| {
        trace("seqUntil {s}:{s}\n", .{ @tagName(sym.tag), sym.str });
        if (mem.indexOfScalar(Symbol.Tag, terminators, sym.tag) != null) {
            ctx.pending_token = sym;
            break;
        }
        try oneSym(ctx, sym, &seq);
    }
    return seq;
}

pub fn parseChoice(ctx: *Context) Error!Symbol {
    trace("parseChoice\n", .{});
    var seq = std.ArrayListUnmanaged(Symbol){};
    while (!ctx.eof()) {
        const sym = nextSym(ctx) orelse break;
        // trace("sym {}\n", .{sym});
        if (sym.tag == .choice) break;
        try oneSym(ctx, sym, &seq);
    }

    trace("parseChoice seq {any}\n", .{seq.items});
    return switch (seq.items.len) {
        0 => error.EmptySequence,
        1 => blk: {
            const result = seq.items[0];
            seq.deinit(ctx.allr);
            break :blk result;
        },
        else => Symbol{ .seq = seq.toOwnedSlice(ctx.allr) },
    };
}

pub fn parseRule(ctx: *Context) Error!Rule {
    trace("parseRule\n", .{});
    var rule: Rule = .{ .source = ctx.rest, .root = undefined };
    var choices = std.ArrayListUnmanaged(Symbol){};
    errdefer {
        for (choices.items) |*c| c.deinit(ctx.allr);
        choices.deinit(ctx.allr);
    }
    while (!ctx.eof()) {
        const choice = parseChoice(ctx) catch |e| switch (e) {
            error.EmptySequence => break,
            else => return e,
        };
        try choices.append(ctx.allr, choice);
    }
    rule.root = .{ .choice = choices.toOwnedSlice(ctx.allr) };
    return rule;
}

pub fn parseFree(allocator: Allocator, prods: []const Production) void {
    for (prods) |prod| prod.deinit(allocator);
    allocator.free(prods);
}

pub fn parseGrammar(allr: Allocator, fallr: Allocator, src: []const u8) ![]const Production {
    var ctx = Context.init(allr, fallr, src);
    var prods = std.ArrayList(Production).init(ctx.allr);
    errdefer for (prods.items) |*prod| prod.deinit(ctx.allr);
    trace("\nparseGrammar()\n", .{});
    while (ctx.rest.len > 0) {
        if (ctx.rest[0] == '#') {
            if (mem.indexOfScalar(u8, ctx.rest, '\n')) |nlidx|
                ctx.rest = ctx.rest[nlidx + 1 ..];
        }
        if (p.ident_arrow(ctx.fallr, ctx.rest)) |r| {
            const name = r.value;
            ctx.rest = r.rest;
            trace("name '{s}'\n", .{name});
            const rule = try parseRule(&ctx);
            try prods.append(.{ .name = name, .rule = rule });
        } else |_| {
            trace("done. ctx.rest '{s}'\n", .{ctx.rest});
            break;
        }
    }
    return prods.toOwnedSlice();
}

const MatchError = error{ Todo, UnescapedDash };
pub fn matchLit(lit: Symbol, token: []const u8) MatchError!usize {
    return switch (lit) {
        .sqbkt_lit => blk: {
            var rest = token;
            var restlit = lit.sqbkt_lit;
            // var matchlen: usize = 0;
            var match = true;
            while (rest.len > 0 and restlit.len > 0 and match) {
                match = false;
                const c = rest[0];
                trace("sqbkt_lit c '{c}' restlit[0] '{c}'\n", .{ c, restlit[0] });
                if (restlit.len > 1) {
                    const next = restlit[1];
                    if (next == '-') {
                        if (restlit.len > 2) {
                            const start = restlit[0];
                            const end = restlit[2];
                            assert(end >= start);
                            match = c -% start <= (end - start);
                            restlit = restlit[3 * @as(u2, @boolToInt(match)) ..];
                        } else {
                            // unescaped '-'
                            return error.UnescapedDash;
                        }
                    } else {
                        match = c == restlit[0];
                        restlit = restlit[@boolToInt(match)..];
                    }
                }

                trace("  match {}\n", .{match});
                rest = rest[@boolToInt(match)..];
            }
            break :blk token.len - rest.len;
        },
        .char_lit => @boolToInt(token.len == 1 and token[0] == lit.char_lit[0]) * token.len,
        .str_lit => @boolToInt(mem.eql(u8, lit.str_lit, token)) * token.len,
        .some => {
            const len = try matchLit(lit.some.*, token);
            return len * @boolToInt(len > 0);
        },
        else => error.Todo,
    };
}

pub fn parseInput(allocator: Allocator, fallr: Allocator, prods: []const Production, input: []const u8) !void {
    var terminals = std.StringHashMap(Rule).init(allocator);
    var nonterminals = std.StringHashMap(Rule).init(allocator);
    defer {
        terminals.deinit();
        nonterminals.deinit();
    }
    for (prods) |prod| {
        if (prod.rule.root.choice.len == 1) {
            const firstchoice = prod.rule.root.choice[0];
            trace("firstchoice {}\n", .{firstchoice});
            if (isTerminal(firstchoice)) {
                try terminals.put(prod.name, prod.rule);
                continue;
            }
        }
        try nonterminals.put(prod.name, prod.rule);
    }
    var rest = input;
    var stack = std.ArrayListUnmanaged([]const u8){};
    if (p.ws(fallr, rest)) |r| rest = r.rest else |_| {}
    while (rest.len > 0) {
        if (m.combine(.{ comptime m.asStr(p.somenonws), p.ws })(fallr, rest)) |r| {
            rest = r.rest;
            trace("token '{s}'\n", .{r.value});
            var termsit = terminals.iterator();
            const matchedname = while (termsit.next()) |ent| {
                const matchlen = try matchLit(ent.value_ptr.*.root.choice[0], r.value);
                trace("checking {s} matchlen {}\n", .{ ent.key_ptr.*, matchlen });
                if (matchlen == r.value.len) {
                    trace("matched {} chars with {s} \n", .{ matchlen, ent.key_ptr.* });
                    break ent.key_ptr.*;
                }
            } else null;
            if (matchedname) |name| {
                // trace("matchedname {s}\n", .{k});
                stack.append(name);
            } else break;
        } else |_| {}
    }
}

fn shift(fallr: Allocator, rest: *[]const u8) []const u8 {
    if (p.ws(fallr, rest.*)) |r| rest.* = r.rest else |_| {}
    if (m.combine(.{ comptime m.asStr(p.somenonws), p.ws })(fallr, rest.*)) |r| {
        rest.* = r.rest;
        return r.value;
    } else |_| return "";
}

pub fn parseLR(allocator: Allocator, fallr: Allocator, ginfo: GrammarInfo, input: []const u8) !void {
    var tables = try createTables(allocator, ginfo, "");
    defer tables.deinit(allocator);
    // var stack = std.ArrayList(ShiftReduce(SymbolId)).init(allocator);
    // TODO: not sure if the stack should be states or symbols?
    // var stack = std.ArrayList(SymbolId).init(allocator);
    var stack = std.ArrayList(StateId).init(allocator);
    defer stack.deinit();
    try stack.append(0);

    var rest = input;
    var token = shift(fallr, &rest);
    var symbolid: SymbolId = 0;
    while (rest.len > 0 and stack.items.len > 0) {
        const stateid = stack.items[stack.items.len - 1];
        const action = tables.actions.get(.{ symbolid, stateid }) orelse unreachable;
        switch (action) {
            // shift next state onto stack
            .shift => {
                token = shift(fallr, &rest);
                symbolid = 0;
                // try stack.append(tables.goto.get(.{ symbolid, stateid }) orelse unreachable);
            },
            .reduce => |nextstateid| {
                const len = 1;
                stack.items.len -= len;
                try stack.append(tables.goto.get(.{ stack.items[stack.items.len - 1], nextstateid }) orelse unreachable);
                if (true) @panic("TODO: output the production nextstateid");
            },
        }

        // ColRow: (symbolid, stateid)
        const nextstateid = tables.goto.get(.{ symbolid, stateid }) orelse unreachable;
        trace("nextstateid {} action {}", .{ nextstateid, action });
        // tables.action.get()
    }
}

pub const SymbolId = u16;
pub const TypedSymbolId = struct {
    id: SymbolId,
    ty: Type,
    pub const Type = enum { term, nonterm };

    pub fn init(id: SymbolId, ty: Type) TypedSymbolId {
        return .{
            .id = id,
            .ty = ty,
        };
    }

    pub fn eql(a: TypedSymbolId, b: TypedSymbolId) bool {
        return @bitCast(u32, a) == @bitCast(u32, b);
    }

    // pub fn adjustedId(self: TypedSymbolId, ginfo: GrammarInfo) SymbolId {
    //     return self.id + ((ginfo.terminalscount + 1) * @boolToInt(self.ty == .nonterm));
    // }
};

pub const GrammarInfo = struct {
    grammar: std.ArrayListUnmanaged(Production) = .{},
    name_sym: NameSymbolMap = .{},
    sym_name: SymbolNameMap = .{},
    /// map of (prodid, pos) => [id]
    /// lookup where which symbols are found at (prodid, pos)
    item_syms: ItemSymbolsMap = .{},
    /// map of tid => [(id, pos)]
    /// lookup all places where a tid is found
    sym_items: SymbolItemsMap = .{},
    production_ids: std.AutoArrayHashMapUnmanaged(SymbolId, void) = .{},
    follow_sets: std.AutoArrayHashMapUnmanaged(SymbolId, []const SymbolId) = .{},
    /// aka total_symbol_count, same as symbolCount(null)
    end: SymbolId = 0,

    pub const NameSymbolMap = std.StringArrayHashMapUnmanaged(TypedSymbolId);
    pub const SymbolNameMap = std.AutoArrayHashMapUnmanaged(TypedSymbolId, []const u8);
    const SymbolList = std.ArrayListUnmanaged(TypedSymbolId);
    const ItemSymbolsMap = std.AutoArrayHashMapUnmanaged(Item, SymbolList);
    const SymbolItemsMap = std.AutoArrayHashMapUnmanaged(TypedSymbolId, ItemSet);

    pub fn init(allocator: Allocator, grammar: []const Production) !GrammarInfo {
        var result: GrammarInfo = .{};
        try result.addProductions(allocator, grammar);
        return result;
    }

    pub fn deinit(self: *GrammarInfo, allocator: Allocator) void {
        self.name_sym.deinit(allocator);
        self.sym_name.deinit(allocator);
        self.grammar.deinit(allocator);
        for (self.item_syms.values()) |*list| list.deinit(allocator);
        self.item_syms.deinit(allocator);
        for (self.sym_items.values()) |*list| list.deinit(allocator);
        self.sym_items.deinit(allocator);
        self.production_ids.deinit(allocator);
        for (self.follow_sets.values()) |slice| allocator.free(slice);
        self.follow_sets.deinit(allocator);
    }

    fn populateIdTables(self: *GrammarInfo, allocator: Allocator, sym: Symbol, pos: u16, prodid: u16) Error!void {
        trace("populateIdTables() {} pos {} prodid {} \n", .{ sym, pos, prodid });
        switch (sym) {
            .char_lit, .str_lit, .sqbkt_lit => |s| {
                trace("lit {s}\n", .{s});
                var id = TypedSymbolId.init(self.symbolCount(null), .term);
                const gop = try self.name_sym.getOrPut(allocator, s);
                if (gop.found_existing) id = gop.value_ptr.* else gop.value_ptr.* = id;
                try self.addByPosition(allocator, id, prodid, pos);
            },
            .name => |s| {
                trace("name {s}\n", .{s});
                var id = TypedSymbolId.init(self.symbolCount(null), .nonterm);
                const gop = try self.name_sym.getOrPut(allocator, s);
                if (gop.found_existing) id = gop.value_ptr.* else gop.value_ptr.* = id;
                try self.addByPosition(allocator, id, prodid, pos);
            },
            .optional, .some, .many, .not => |s| {
                try self.populateIdTables(allocator, s.*, pos, prodid);
            },
            .seq, .group => |ss| for (ss) |s, i| {
                try self.populateIdTables(allocator, s, pos + @intCast(u16, i), prodid);
            },
            .choice => |ss| for (ss) |s| {
                try self.populateIdTables(allocator, s, pos, prodid);
            },
            else => unreachable,
        }
    }

    fn addByPosition(self: *GrammarInfo, allocator: Allocator, id: TypedSymbolId, prodid: u16, pos: u16) !void {
        trace("addByPosition() id {} prodid {} pos {}\n", .{ id.id, prodid, pos });
        {
            const gop = try self.item_syms.getOrPut(allocator, Item.init(prodid, pos));
            if (!gop.found_existing) gop.value_ptr.* = .{};
            try gop.value_ptr.*.append(allocator, id);
        }
        {
            const gop = try self.sym_items.getOrPut(allocator, id);
            if (!gop.found_existing) gop.value_ptr.* = .{};
            try gop.value_ptr.*.append(allocator, Item.init(prodid, pos));
        }
    }

    pub fn symbolCount(self: GrammarInfo, comptime mtype: ?TypedSymbolId.Type) SymbolId {
        return if (mtype) |ty| blk: {
            var result: SymbolId = 0;
            for (self.name_sym.values()) |tid|
                result += @boolToInt(tid.ty == ty);
            break :blk result;
        } else @intCast(SymbolId, self.name_sym.count());
    }

    pub fn addProductions(self: *GrammarInfo, allocator: Allocator, grammar: []const Production) !void {
        for (grammar) |prod| {
            var id = TypedSymbolId.init(self.symbolCount(null), .nonterm);
            {
                const gop = try self.name_sym.getOrPut(allocator, prod.name);
                if (gop.found_existing) {
                    id = gop.value_ptr.*;
                } else {
                    gop.value_ptr.* = id;
                }
                try self.production_ids.put(allocator, id.id, {});
            }
            try self.populateIdTables(allocator, prod.rule.root, 0, id.id);
            trace("-- '{s}' prodid {}\n", .{ prod.name, id.id });
        }

        //
        for (self.name_sym.values()) |*tid| {
            if (tid.ty == .nonterm and !self.production_ids.contains(tid.id)) {
                self.sym_items.getKeyPtr(tid.*).?.ty = .term;
                for (self.item_syms.values()) |syms| {
                    for (syms.items) |*sym| {
                        if (sym.eql(tid.*)) sym.ty = .term;
                    }
                }
                tid.ty = .term;
            }
        }

        var it = self.name_sym.iterator();
        while (it.next()) |e| {
            const gop = try self.sym_name.getOrPut(allocator, e.value_ptr.*);
            if (!gop.found_existing) gop.value_ptr.* = e.key_ptr.*;
        }
        try self.grammar.appendSlice(allocator, grammar);
        const termcount = self.symbolCount(.term);
        const nontermcount = self.symbolCount(.nonterm);
        self.end = self.symbolCount(null);
        try self.calcFollowSets(allocator);

        trace("glen {} ppslen {} nonterms/terms {}/{} total names/syms {}/{}\n", .{
            self.grammar.items.len,
            self.item_syms.count(),
            termcount,
            nontermcount,
            self.name_sym.count(),
            self.sym_name.count(),
        });
        assert(self.name_sym.count() == self.sym_name.count());
    }

    pub fn first(ginfo: GrammarInfo, allocator: Allocator, item: Item) !ItemSet {
        trace("first({})\n", .{Item.Fmt.init(item, ginfo)});
        var result: ItemSet = .{};
        var i = item;
        while (ginfo.item_syms.get(i)) |fs| {
            trace("fs {}\n", .{TidsFmt.init(fs.items, ginfo)});
            for (fs.items) |f| {
                if (ginfo.idToTid(f.id).?.ty == .term)
                    try result.append(allocator, .{ .id = f.id, .pos = 0 })
                else if (f.id != i.id) {
                    i = .{ .id = f.id, .pos = 0 };
                    break;
                }
            }
            if (result.items.len > 0) break;
        }
        return result;
    }

    pub fn start(ginfo: GrammarInfo, allocator: Allocator) !SymbolId {
        var set = std.AutoArrayHashMap(SymbolId, void).init(allocator);
        defer set.deinit();
        for (ginfo.production_ids.keys()) |prodid| try set.put(prodid, {});

        // assert(ginfo.production_ids.items.len > 0);
        // const targetlen = ginfo.production_ids.items.len - 1;
        for (ginfo.production_ids.keys()) |prodid| {
            var pos: u16 = 0;
            while (ginfo.item_syms.get(.{ .id = prodid, .pos = pos })) |tids| : (pos += 1) {
                for (tids.items) |tid|
                    _ = set.swapRemove(tid.id);
                if (set.count() == 1) return set.keys()[0];
            }
        }
        unreachable;
    }

    pub fn calcFollowSets(ginfo: *GrammarInfo, allocator: Allocator) !void {
        const startid = try ginfo.start(allocator);
        trace("calcFollowSets start {s}\n", .{ginfo.idToName(ginfo.idToTid(startid).?)});
        var followset = std.AutoArrayHashMap(SymbolId, void).init(allocator);
        defer followset.deinit();
        try followset.put(ginfo.end, {});
        for (ginfo.follow_sets.values()) |slice| allocator.free(slice);
        ginfo.follow_sets.clearRetainingCapacity();
        try ginfo.follow_sets.putNoClobber(allocator, startid, try allocator.dupe(SymbolId, followset.keys()));
        for (ginfo.production_ids.keys()) |prodid| {
            if (ginfo.follow_sets.contains(prodid)) continue;
            var pos: u16 = 0;
            followset.clearRetainingCapacity();
            while (ginfo.item_syms.get(.{ .id = prodid, .pos = pos })) |tids| : (pos += 1) {
                const count = followset.count();
                for (tids.items) |tid| {
                    trace("tid {}\n", .{TidFmt.init(tid, ginfo.*)});
                    var firsts = try ginfo.first(allocator, .{ .id = tid.id, .pos = 0 });
                    defer firsts.deinit(allocator);
                    try followset.put(ginfo.end, {});
                    for (firsts.items) |fitem|
                        try followset.put(fitem.id, {});
                }
                if (followset.count() == count) break;
            }
            try ginfo.follow_sets.putNoClobber(allocator, prodid, try allocator.dupe(SymbolId, followset.keys()));
        }
    }

    pub fn follow(ginfo: GrammarInfo, prodid: SymbolId) []const SymbolId {
        return if (ginfo.follow_sets.get(prodid)) |list| list else &[0]SymbolId{};
    }

    pub fn idToName(ginfo: GrammarInfo, tid: TypedSymbolId) ?[]const u8 {
        return ginfo.sym_name.get(tid);
    }
    pub fn nameToTid(ginfo: GrammarInfo, name: []const u8) ?TypedSymbolId {
        return ginfo.name_sym.get(name);
    }
    // pub fn nameToSymbolId(ginfo: GrammarInfo, name: []const u8) ?SymbolId {
    //     return if (ginfo.name_sym.get(name)) |tid| tid.adjustedId(ginfo) else null;
    // }
    pub fn nameToProdId(ginfo: GrammarInfo, name: []const u8) ?SymbolId {
        for (ginfo.grammar.items) |prod, i| {
            if (mem.eql(u8, name, prod.name)) return @intCast(SymbolId, i);
        }
        return null;
    }
    // pub fn nameToItem(ginfo: GrammarInfo, name: []const u8, dotpos: u16) ?ItemSet.Item {
    //     return if (ginfo.name_to_tyid.get(name)) |tyid|
    //         ItemSet.Item{ .symid = tyid.id, .dotpos = dotpos }
    //     else
    //         null;
    // }
    pub fn idToTid(ginfo: GrammarInfo, id: SymbolId) ?TypedSymbolId {
        for (ginfo.name_sym.values()) |tid| {
            if (id == tid.id) return tid;
        }
        return null;
    }
    pub fn itemSet(ginfo: GrammarInfo, allocator: Allocator, dotpos: u16) !ItemSet {
        var result: ItemSet = .{};
        for (ginfo.grammar.items) |prod| {
            const tid = ginfo.nameToTid(prod.name) orelse unreachable;
            assert(tid.ty == .nonterm);
            try result.append(allocator, .{ .id = tid.id, .pos = dotpos });
        }
        return result;
    }
    pub fn itemSetUsing(ginfo: GrammarInfo, allocator: Allocator, lookupginfo: GrammarInfo, dotpos: u16) !ItemSet {
        var result: ItemSet = .{};
        for (ginfo.grammar.items) |prod| {
            const tid = lookupginfo.nameToTid(prod.name) orelse unreachable;
            assert(tid.ty == .nonterm);
            try result.append(allocator, .{ .id = tid.id, .pos = dotpos });
        }
        return result;
    }
    pub fn itemSetFromNames(ginfo: GrammarInfo, allocator: Allocator, names: []const []const u8, dotpos: u16) !ItemSet {
        var result: ItemSet = .{};
        for (names) |name| {
            const tid = ginfo.nameToTid(name) orelse return error.MissingName;
            assert(tid.ty == .nonterm);
            try result.append(allocator, .{ .id = tid.id, .pos = dotpos });
        }
        return result;
    }
};

pub const ItemSet = std.ArrayListUnmanaged(Item);
pub const ItemSetList = std.ArrayListUnmanaged(ItemSet);
pub const Item = struct {
    id: SymbolId,
    pos: u16,
    pub fn init(id: SymbolId, pos: u16) Item {
        return .{ .id = id, .pos = pos };
    }
    pub fn fromArr(arr: [2]u16) Item {
        return @bitCast(Item, arr);
    }
    pub fn eql(a: Item, b: Item) bool {
        return @bitCast(u32, a) == @bitCast(u32, b);
    }

    pub const Fmt = struct {
        item: Item,
        ginfo: GrammarInfo,
        pub fn init(item: Item, ginfo: GrammarInfo) Fmt {
            return .{
                .item = item,
                .ginfo = ginfo,
            };
        }

        pub fn format(self: Fmt, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            const id = self.item.id;
            if (id == self.ginfo.end) {
                _ = try writer.write(". $ ");
            } else if (self.ginfo.idToName(.{ .id = id, .ty = .nonterm })) |_| {
                const idx = mem.indexOfScalar(SymbolId, self.ginfo.production_ids.keys(), id) orelse unreachable;
                try writer.print("{s} <- ", .{self.ginfo.grammar.items[idx].name});
                var dotpos: u16 = 0;
                var wrotedot = false;
                // const dot: []const u8 = &[_]u8{ 250, ' ' };
                const dot: []const u8 = ". ";
                while (self.ginfo.item_syms.get(.{ .id = id, .pos = dotpos })) |tids| : (dotpos += 1) {
                    if (self.item.pos == dotpos) {
                        _ = try writer.write(dot);
                        wrotedot = true;
                    }

                    if (tids.items.len > 1) {
                        _ = try writer.write("(");
                        for (tids.items) |tid, i| {
                            if (i != 0) _ = try writer.write(" / ");
                            try writer.print("{s}", .{TidFmt.init(tid, self.ginfo)});
                        }
                        _ = try writer.write(") ");
                    } else {
                        try writer.print("{s} ", .{TidFmt.init(tids.items[0], self.ginfo)});
                    }
                }
                if (!wrotedot) _ = try writer.write(dot);
            } else try writer.print(". {s} ", .{self.ginfo.idToName(.{ .id = id, .ty = .term })});
        }
    };
};

pub fn itemSetEql(a: ItemSet, b: ItemSet) bool {
    return a.items.len == b.items.len and for (a.items) |aitem| {
        const b_contains_aitem = for (b.items) |bitem| {
            if (bitem.eql(aitem)) break true;
        } else false;

        if (!b_contains_aitem) break false;
    } else true;
}

pub fn itemSetsContain(sets: []const ItemSet, set: ItemSet) bool {
    for (sets) |seta| {
        if (itemSetEql(seta, set)) return true;
    }
    return false;
}

pub fn itemSetContains(set: ItemSet, item: Item) bool {
    for (set.items) |setitem| {
        if (setitem.eql(item)) return true;
    }
    return false;
}

pub const ItemSetFmt = struct {
    itemset: ItemSet,
    ginfo: GrammarInfo,
    pub fn init(itemset: ItemSet, ginfo: GrammarInfo) ItemSetFmt {
        return .{
            .itemset = itemset,
            .ginfo = ginfo,
        };
    }

    pub fn format(self: ItemSetFmt, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        _ = try writer.write("{");
        for (self.itemset.items) |item, i| {
            if (i != 0) _ = try writer.write(", ");
            try writer.print("{}", .{Item.Fmt.init(item, self.ginfo)});
        }
        _ = try writer.write("}");
    }
};

pub const TidsFmt = struct {
    tids: []const TypedSymbolId,
    ginfo: GrammarInfo,
    pub fn init(tids: []const TypedSymbolId, ginfo: GrammarInfo) TidsFmt {
        return .{
            .tids = tids,
            .ginfo = ginfo,
        };
    }

    pub fn format(self: TidsFmt, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        for (self.tids) |item| {
            try writer.print("{}, ", .{TidFmt.init(item, self.ginfo)});
        }
    }
};

pub const TidFmt = struct {
    tid: TypedSymbolId,
    ginfo: GrammarInfo,
    pub fn init(tid: TypedSymbolId, ginfo: GrammarInfo) TidFmt {
        return .{
            .tid = tid,
            .ginfo = ginfo,
        };
    }
    pub fn format(self: TidFmt, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        // try writer.print("{s}:{}, ", .{ self.ginfo.idToName(self.tid), self.tid.id });
        if (self.tid.id == self.ginfo.end)
            _ = try writer.write("$")
        else if (self.tid.ty == .term)
            try writer.print("'{s}'", .{self.ginfo.idToName(self.tid)})
        else if (self.ginfo.idToName(self.tid)) |name|
            try writer.print("{s}", .{name})
        else
            _ = try writer.write("unknowntid");
    }
};

pub const SymIdFmt = struct {
    id: SymbolId,
    ginfo: GrammarInfo,
    pub fn init(id: SymbolId, ginfo: GrammarInfo) SymIdFmt {
        return .{
            .id = id,
            .ginfo = ginfo,
        };
    }
    pub fn format(self: SymIdFmt, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        // try writer.print("{s}:{}, ", .{ self.ginfo.idToName(self.tid), self.tid.id });
        if (self.ginfo.idToTid(self.id)) |tid|
            try writer.print("{}", .{TidFmt.init(tid, self.ginfo)})
        else if (self.id == self.ginfo.end)
            _ = try writer.write("$")
        else
            try writer.print("{}", .{self.id});
    }
};

// fn closure(I) f
// J = I;
// while(true) {
//   for ( each '.B' in J ) {
//      for ( each 'B -> .y' in G ) {
//          if ( 'B -> .y' is not in J )
//              add 'B -> .y' to J ;
//      }
//   }
//   until no more items are added to J on one round;
//}
// return J;
pub fn closure(allocator: Allocator, I: ItemSet, ginfo: GrammarInfo) !ItemSet {
    trace("closure({})\n", .{ItemSetFmt.init(I, ginfo)});

    // NOTE: the final argument to std.ArrayHashMapUnmanaged() is store_hash.
    // it is set to false here to prevent a segfault.  not sure why its needed.
    // var J: std.AutoArrayHashMapUnmanaged(Item, void) = .{}; // <--- causes segfault
    var J: std.ArrayHashMapUnmanaged(Item, void, std.array_hash_map.AutoContext(Item), false) = .{};
    defer J.deinit(allocator);
    for (I.items) |i| try J.put(allocator, i, {});

    while (true) {
        const count = J.count();
        // std.debug.print("J.count() {}\n", .{J.count()});
        for (J.keys()) |item| { // NOTE: segfault here if AutoArrayHashMapUnmanaged is used
            // trace("  item {}\n", .{Item.Fmt.init(item, ginfo)});
            const Bs = ginfo.item_syms.get(item) orelse continue;
            // trace("Bs {s}\n", .{TidsFmt.init(Bs.items, ginfo)});
            for (Bs.items) |b| {
                if (b.ty == .term) continue;
                // trace("b {}\n", .{TidFmt.init(b, ginfo)});
                var newitem: Item = .{ .id = b.id, .pos = 0 };
                try J.put(allocator, newitem, {});
            }
        }
        if (count == J.count()) break;
    }

    const keys = J.keys();
    return ItemSet{ .items = try allocator.dupe(Item, keys), .capacity = keys.len };
}

// Compilers-DragonBook p. 261

// SetOfItems goto(I, X ) {
//  initialize J to b e the empty set;
//  for ( each item ['.X'] in I )
//      add item ['X.'] to set J ;
//  return closure(J);
//}

// goto([S' <- .S], S) => [S' <- S.]
pub fn goto(allocator: Allocator, I: ItemSet, X: TypedSymbolId, ginfo: GrammarInfo) !ItemSet {
    var result = ItemSet{};
    defer result.deinit(allocator);

    trace("goto(I: {}, X: {})\n", .{ ItemSetFmt.init(I, ginfo), TidFmt.init(X, ginfo) });
    // lookup X by id to get [(prod, pos)]
    // var X_ = X;
    // X_.ty.remove(.final);
    const mprodposlist = ginfo.sym_items.get(X);
    const prodposlist = if (mprodposlist) |l| l else ItemSet{};
    // trace("prodposlist {any}\n", .{ItemSetFmt.init(prodposlist, ginfo)});
    for (prodposlist.items) |prodpos| {
        try result.append(allocator, .{ .id = prodpos.id, .pos = prodpos.pos + 1 });
    }
    return closure(allocator, result, ginfo);
}

// void items(G') {
//   C = closure({[S' -> S]}) ;
//   repeat
//     for ( each set of items I in C )
//       for ( each grammar symbol X )
//         if ( GOTO(I ; X ) is not empty and not in C )
//           add GOTO(I ; X ) to C ;
//   until no new sets of items are added to C on a round;
// }
pub fn lr0items(allr: Allocator, set0: ItemSet, ginfo: GrammarInfo) !ItemSetList {
    trace("lr0items({})\n", .{ItemSetFmt.init(set0, ginfo)});
    var itemsets = ItemSetList{};
    try itemsets.append(allr, try closure(allr, set0, ginfo));
    var n: usize = 0;
    while (true) : (n += 1) {
        const count = itemsets.items.len;
        if (n == 2) @panic("SDF");
        trace("lr0itemset-{}\n", .{n});
        for (itemsets.items) |I| {
            var next = ItemSet{};
            defer next.deinit(allr);
            // trace("  I: {}\n", .{ItemSetFmt.init(I, ginfo)});
            // trace("  I: {any}\n", .{I.items});
            // for (ginfo.sym_name.keys()) |X| {
            for (I.items) |i| {
                const Xs = ginfo.item_syms.get(i) orelse continue;
                for (Xs.items) |X| {
                    var G = try goto(allr, I, X, ginfo);
                    // trace("    G: {}\n", .{ItemSetFmt.init(G, ginfo)});
                    // trace("    G: {any}\n", .{G.items});
                    if (G.items.len > 0 and !itemSetsContain(itemsets.items, G))
                        try itemsets.append(allr, G)
                    else
                        G.deinit(allr);
                }
            }
        }
        if (itemsets.items.len == count) break;
    }

    for (itemsets.items) |itemset, i| {
        // trace("itemset {}\n{any}\n", .{ i, itemset.items });
        trace("itemset {}: {}\n", .{ i, ItemSetFmt.init(itemset, ginfo) });
    }
    return itemsets;
}

pub const StateId = u16;
pub const ColRow = struct {
    col: SymbolId = 0,
    row: SymbolId = 0,
    pub fn max(a: ColRow, b: ColRow) ColRow {
        // const V = std.meta.Vector(2, SymbolId);
        // const V2 = std.meta.Vector(2, u1);
        // const va = @bitCast(V, a);
        // const vb = @bitCast(V, b);
        // const cmp: [2]SymbolId = @as([2]SymbolId, @bitCast(V2, va > vb));
        // return @bitCast(ColRow, cmp);
        const row = std.math.max(a.row, b.row);
        const col = std.math.max(a.col, b.col);
        return .{ .row = row, .col = col };
    }
};

pub const SLRTables = struct {
    actions: Actions = .{},
    gotos: Gotos = .{},
    actionrows: u16 = 0,
    gotorows: u16 = 0,
    ginfo: GrammarInfo,

    pub const Entry = struct {
        col: TypedSymbolId,
        row: StateId,

        pub const Fmt = struct {
            entry: Entry,
            ginfo: GrammarInfo,
            pub fn init(entry: Entry, ginfo: GrammarInfo) Fmt {
                return .{
                    .entry = entry,
                    .ginfo = ginfo,
                };
            }
            pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
                try writer.print("{}:({}-{s},{})", .{ TidFmt.init(self.entry.col, self.ginfo), self.entry.col.id, @tagName(self.entry.col.ty), self.entry.row });
            }
        };
    };

    pub const Action = union(enum) {
        shift: SymbolId,
        reduce: Reduce,
        accept,

        pub const Reduce = struct {
            id: SymbolId,
            choice: SymbolId,
            pub fn format(self: Reduce, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
                try writer.print("{}:{}", .{ self.id, self.choice });
            }
        };

        pub const ProductionChoice = struct {
            prodid: SymbolId,
            choiceid: SymbolId,
        };
        pub fn format(self: @This(), comptime fmt: []const u8, opts: std.fmt.FormatOptions, writer: anytype) !void {
            // std.debug.print("---fmt {s} opts {}", .{ fmt, opts });
            _ = fmt;
            _ = opts;
            switch (self) {
                .shift => |s| _ = try writer.print("s{}" ++ fmt, .{s}),
                .reduce => |r| _ = try writer.print("r{}" ++ fmt, .{r}),
                .accept => _ = try writer.write("$"),
            }
        }
    };

    pub const Actions = std.AutoArrayHashMapUnmanaged(Entry, Action);
    pub const Gotos = std.AutoArrayHashMapUnmanaged(Entry, SymbolId);
    pub const Type = enum { action, goto };
    pub fn deinit(self: *SLRTables, allocator: Allocator) void {
        self.actions.deinit(allocator);
        self.gotos.deinit(allocator);
        self.ginfo.deinit(allocator);
    }

    pub fn putNoClobber(self: *SLRTables, allocator: Allocator, comptime ty: Type, entry: Entry, value: anytype) !void {
        switch (ty) {
            .action => {
                const gop = try self.actions.getOrPut(allocator, entry);
                // TODO: handle existing
                if (gop.found_existing)
                    std.debug.print("  found existing\n", .{});
                // if (gop.found_existing) return error.FoundExisting;
                gop.value_ptr.* = value;
                self.actionrows = std.math.max(self.actionrows, entry.row);
            },
            .goto => {
                const gop = try self.gotos.getOrPut(allocator, entry);
                // TODO: handle existing
                if (gop.found_existing)
                    std.debug.print("  found existing\n", .{});
                // if (gop.found_existing) return error.FoundExisting;
                gop.value_ptr.* = value;
                self.gotorows = std.math.max(self.gotorows, entry.row);
            },
        }
    }

    fn writeByteNTimesNl(writer: anytype, byte: u8, n: usize) !void {
        try writer.writeByteNTimes(byte, n);
        _ = try writer.write("\n");
    }

    pub fn display(self: SLRTables, writer: anytype, comptime colwidth: u8) !void {
        var col: SymbolId = 0;
        const itemfmt = comptime std.fmt.comptimePrint("{{: <{}}}", .{colwidth});
        const stritemfmt = comptime std.fmt.comptimePrint("{{s: <{}}}", .{colwidth});
        const termcount = self.ginfo.symbolCount(.term);
        const nontermcount = self.ginfo.symbolCount(.nonterm);
        const maxcol = (termcount + nontermcount) + 3;

        // -- start header
        try writeByteNTimesNl(writer, '-', maxcol * colwidth);
        try writer.print(stritemfmt ++ "action", .{""});
        try writer.writeByteNTimes(' ', (termcount -| 1) * colwidth);
        try writer.print(stritemfmt ++ "goto\n", .{""});
        try writeByteNTimesNl(writer, '-', maxcol * colwidth);
        _ = try writer.print(stritemfmt, .{"state"});
        col = 0;
        while (col <= termcount) : (col += 1) {
            const name = self.ginfo.idToName(.{ .ty = .term, .id = col }) orelse continue;
            try writer.print(stritemfmt, .{peekRest(name, colwidth - 1)});
        }
        _ = try writer.print(stritemfmt, .{"$"});

        for (self.ginfo.production_ids.keys()) |id| {
            const name = self.ginfo.sym_name.get(.{ .id = id, .ty = .nonterm }) orelse unreachable;
            try writer.print(stritemfmt, .{peekRest(name, colwidth - 1)});
        }
        _ = try writer.print(stritemfmt ++ "\n", .{"$"});

        try writeByteNTimesNl(writer, '-', maxcol * colwidth);
        // -- end header

        const maxrow = std.math.max(self.actionrows, self.gotorows);
        var row: SymbolId = 0;
        while (row < maxrow) : (row += 1) {
            try writer.print(itemfmt, .{row});
            col = 0;
            while (col <= termcount) : (col += 1) {
                if (self.actions.get(.{ .row = row, .col = .{ .ty = .term, .id = col } })) |value| {
                    try writer.print(itemfmt, .{value});
                } else try writer.print(stritemfmt, .{" "});
            }
            col = 0;
            while (col <= nontermcount) : (col += 1) {
                if (self.gotos.get(.{ .row = row, .col = .{ .ty = .nonterm, .id = col } })) |value| {
                    try writer.print(itemfmt, .{value});
                } else try writer.print(stritemfmt, .{" "});
            }
            _ = try writer.write("\n");
        }
    }

    pub fn parseTables(allocator: Allocator, fallr: Allocator, rows: []const u8, ginfo: GrammarInfo) !SLRTables {
        var tables: SLRTables = .{ .ginfo = ginfo };
        var linesit = std.mem.split(u8, rows, "\n");
        var row: u8 = 0;
        while (linesit.next()) |line| : (row += 1) {
            var it = std.mem.split(u8, line, ", ");
            while (it.next()) |ent| {
                // trace("ent {s}\n", .{ent});
                const parser = comptime m.combine(.{
                    m.oneOf(.{ p.ident, p.str_lit, p.char_lit, p.sqbkt_lit, m.asStr(p.char('$')) }), p.char('-'),
                    m.rest,
                });
                var colname: []const u8 = undefined;
                var val: []const u8 = undefined;
                if (ent.len == 0) continue;
                if (parser(fallr, ent)) |r| {
                    colname = r.value[0];
                    val = r.value[2];
                } else |_| {}

                var valit = mem.split(u8, val, ":");
                const symname = valit.next().?;
                const mchoice = valit.next();
                if (mchoice) |choicestr| {
                    // this is a reduce
                    assert(symname.len > 0 and symname[0] == 'r');
                    const choiceid = try std.fmt.parseInt(SymbolId, choicestr, 10);
                    const tid = ginfo.name_sym.get(symname[1..]).?;
                    // const colid: TypedSymbolId = .{ .id = tid, .ty = .nonterm };
                    const col = .{ .col = tid, .row = row };
                    const action = Action{ .reduce = .{ .id = tid.id, .choice = choiceid } };
                    try tables.actions.put(allocator, col, action);
                } else {
                    const colid = if (std.mem.eql(u8, colname, "$"))
                        TypedSymbolId{ .id = ginfo.end, .ty = .term }
                    else
                        ginfo.nameToTid(colname).?;
                    const col = .{ .col = colid, .row = row };
                    if (colid.ty == .term) {
                        // action
                        trace("ent '{s}' val '{s}'\n", .{ ent, val });
                        const action: Action = if (val[0] == 's')
                            Action{ .shift = try std.fmt.parseInt(StateId, val[1..], 10) }
                        else if (val[0] == 'r') // r0
                            Action{ .reduce = .{ .id = try std.fmt.parseInt(StateId, val[1..], 10), .choice = BAD_CHOICE } }
                        else if (std.mem.eql(u8, val, "$a"))
                            Action{ .accept = {} }
                        else
                            unreachable;

                        try tables.actions.put(allocator, col, action);
                    } else {
                        // goto
                        const state = try std.fmt.parseInt(StateId, val, 10);
                        try tables.gotos.put(allocator, col, state);
                    }
                }
            }
        }
        tables.actionrows = row;
        return tables;
    }
};

const ItemStateMap = std.AutoArrayHashMapUnmanaged(SLRTables.Entry, SymbolId);
// FIXME: this needs to be able to store a productionchoice pair
// or else productions need to be duplicated for each choice so that
// A <- B / C
// becomes
// A <- B
// A <- C
fn nextStateId(allocator: Allocator, seen: *ItemStateMap, entry: SLRTables.Entry) !SymbolId {
    const seencount = @intCast(StateId, seen.count()) + 1;
    const gop = try seen.getOrPut(allocator, entry);
    const stateid = if (gop.found_existing) gop.value_ptr.* else blk: {
        gop.value_ptr.* = seencount;
        break :blk seencount;
    };
    return stateid;
}

const BAD_CHOICE = std.math.maxInt(SymbolId);

pub fn createTables(allocator: Allocator, _ginfo: GrammarInfo, augmented_start: []const u8) !SLRTables {
    // based on https://pages.github-dev.cs.illinois.edu/cs421-sp20/web/handouts/lr-parsing-tables.pdf

    var tables: SLRTables = .{ .ginfo = try GrammarInfo.init(allocator, _ginfo.grammar.items) };

    var augmentedstart = try parseGrammar(allocator, std.testing.failing_allocator, augmented_start);
    var auginfo = try GrammarInfo.init(allocator, augmentedstart);
    defer {
        parseFree(allocator, augmentedstart);
        auginfo.deinit(allocator);
    }
    try tables.ginfo.addProductions(allocator, auginfo.grammar.items);
    var set0 = try auginfo.itemSetUsing(allocator, tables.ginfo, 0);
    var itemsets = try lr0items(allocator, set0, tables.ginfo);
    defer {
        set0.deinit(allocator);
        for (itemsets.items) |*itemset| itemset.deinit(allocator);
        itemsets.deinit(allocator);
    }
    if (itemsets.items.len == 0) return error.EmptyItemsets;

    var seen: ItemStateMap = .{};
    defer seen.deinit(allocator);

    for (itemsets.items) |itemset, i| {
        std.debug.print("itemset-{}\n", .{i});
        for (itemset.items) |item, j| {
            std.debug.print("item-{} {}\n", .{ j, Item.Fmt.init(item, tables.ginfo) });
            const msyms = tables.ginfo.item_syms.get(item);
            const isfinal = msyms == null;
            if (isfinal) {
                trace("isfinal \n", .{});
                // Enter a reduce action in the follow set columns, .reduce=sym.id
                // TODO: follow set
                // for now assume follow set = {$}
                var follows = tables.ginfo.follow(item.id);
                for (follows) |fitem| {
                    trace("follow {}\n", .{SymIdFmt.init(fitem, tables.ginfo)});

                    const entry: SLRTables.Entry = .{
                        // FIXME: how to determine .ty ?
                        .col = .{ .id = fitem, .ty = .term },
                        .row = @intCast(SymbolId, i),
                    };
                    try tables.putNoClobber(allocator, .action, entry, SLRTables.Action{
                        .reduce = .{ .id = item.id, .choice = 0 },
                    });
                }
            } else for (msyms.?.items) |sym| {
                const symentry: SLRTables.Entry = .{ .col = sym, .row = item.pos };
                const stateid = try nextStateId(allocator, &seen, symentry);
                std.debug.print("stateid {} entry {}\n", .{ stateid, SLRTables.Entry.Fmt.init(symentry, tables.ginfo) });
                if (sym.ty == .term) {
                    const entry: SLRTables.Entry = .{ .col = sym, .row = @intCast(SymbolId, i) };
                    std.debug.print(
                        "sym {} entry {} stateid {}\n",
                        .{ TidFmt.init(sym, tables.ginfo), SLRTables.Entry.Fmt.init(entry, tables.ginfo), stateid },
                    );
                    // Enter a shift action in row n and column α,
                    try tables.putNoClobber(allocator, .action, entry, .{ .shift = stateid });
                    // and enter n′ in the corresponding entry of the goto ts[i].
                    // try tables.putNoClobber(allocator, .goto, entry, nextstateid);
                } else {
                    const entry: SLRTables.Entry = .{ .col = sym, .row = @intCast(SymbolId, i) };
                    std.debug.print(
                        "sym {} entry {} stateid {}\n",
                        .{ TidFmt.init(sym, tables.ginfo), SLRTables.Entry.Fmt.init(entry, tables.ginfo), stateid },
                    );
                    // Enter n′ in the goto table for row n and nonterminal A.
                    try tables.putNoClobber(allocator, .goto, entry, stateid);
                }
            }
        }
    }
    return tables;
}
