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
    pub const alphanum = m.oneOf(.{ char('_'), m.ascii.alphanum });
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
    pub const ident = m.asStr(m.combine(.{ m.ascii.alpha, manyalphanum }));
    pub const _larrow = m.string("<-");
    pub const larrow = m.discard(_larrow);
    pub const _many_non_larrow = m.many(m.ascii.not(larrow), .{ .collect = false });
    pub const many_non_larrow = m.combine(.{ ws, _many_non_larrow, larrow, ws });
    pub const many_non_nl = m.many(m.ascii.not(char('\n')), .{ .collect = false });
    pub const comment = m.asStr(m.combine(.{ char('#'), many_non_nl, m.opt(char('\n')) }));

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
    pending_sym: ?Sym = null,

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
};

pub const Error = error{EmptySequence} || Allocator.Error;

pub const Sym = struct {
    tag: Symbol.Tag,
    str: []const u8,
    pub fn init(tag: Symbol.Tag, str: []const u8) Sym {
        return .{
            .tag = tag,
            .str = str,
        };
    }
};

pub fn nextSym(ctx: *Context) ?Sym {
    if (ctx.pending_sym) |sym| {
        ctx.pending_sym = null;
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
            return Sym.init(tag, r.value);
        } else |_| {}
    }

    return null;
}
fn peekTop(items: anytype) ?std.meta.Child(@TypeOf(items)) {
    return if (items.len > 0) items[items.len - 1] else null;
}
pub fn symToSymbol(sym: Sym) Symbol {
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

fn oneSym(ctx: *Context, sym: Sym, seq: *std.ArrayListUnmanaged(Symbol)) !void {
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
            // trace("choice pending {}\n", .{ctx.pending_sym});
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
            ctx.pending_sym = sym;
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
        if (sym.tag == .choice) break;
        try oneSym(ctx, sym, &seq);
        // trace("symbol {}\n", .{symbol});
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
        const choice = try parseChoice(ctx);
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
    trace("\nparse()\n", .{});
    while (ctx.rest.len > 0) {
        if (ctx.rest[0] == '#') {
            if (mem.indexOfScalar(u8, ctx.rest, '\n')) |nlidx|
                ctx.rest = ctx.rest[nlidx + 1 ..];
        }
        // trace("rest '{s}'\n", .{rest[0..20]});
        const nameres = p.many_non_larrow(ctx.fallr, ctx.rest) catch {
            trace("no arrow. ctx.rest '{s}'\n", .{ctx.rest});
            @panic("Parse failure");
        };
        ctx.rest = ctx.rest[ctx.rest.len - nameres.rest.len ..];
        const name = mem.trim(u8, nameres.value, &std.ascii.spaces);
        const nextarrowidx = mem.indexOf(u8, ctx.rest, "<-") orelse {
            const rest = ctx.rest;
            defer ctx.rest = rest;
            ctx.rest = mem.trim(u8, ctx.rest, &std.ascii.spaces);
            trace("rule '{s}'\n", .{name});
            try prods.append(.{ .name = name, .rule = try parseRule(&ctx) });
            break;
        };
        // trace("name '{s}' nextarrowidx {}\n", .{ name, nextarrowidx });
        var endruleidx = nextarrowidx;
        while (true) {
            endruleidx = mem.lastIndexOfScalar(u8, ctx.rest[0..endruleidx], '\n') orelse break;
            const identres = p.ident(ctx.fallr, ctx.rest[endruleidx + 1 ..]) catch {
                // trace("ident() fail '{s}'\n", .{rest[endruleidx..][0..10]});
                // const s = ctx.rest[endruleidx + 1 ..];
                // const len = std.math.min(10, s.len);
                // trace("ident() fail '{s}':'{s}' endruleidx {}\n", .{ name, s[0..len], endruleidx });
                continue;
            };
            // trace("identres {s}\n", .{identres.value});
            _ = identres;
            const rest = ctx.rest;
            defer ctx.rest = rest;
            ctx.rest = mem.trim(u8, ctx.rest[0..endruleidx], &std.ascii.spaces);
            trace("rule '{s}'\n", .{name});
            try prods.append(.{ .name = name, .rule = try parseRule(&ctx) });
            break;
        }
        ctx.rest = mem.trimLeft(u8, ctx.rest[endruleidx..], &std.ascii.spaces);
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
        const action = tables.action.get(.{ symbolid, stateid }) orelse unreachable;
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

    // pub fn adjustedId(self: TypedSymbolId, ginfo: GrammarInfo) SymbolId {
    //     return self.id + ((ginfo.terminalscount + 1) * @boolToInt(self.ty == .nonterm));
    // }

};

pub const GrammarInfo = struct {
    grammar: std.ArrayListUnmanaged(Production) = .{},
    names_to_syms: NameTypedSymbolsMap = .{},
    syms_to_names: TypedSymbolNamesMap = .{},
    prod_pos_symbols: ProdPosTypedSymbolsMap = .{},
    symbol_prod_pos: TypedSymbolProdPosMap = .{},
    terminalscount: SymbolId = 0,
    nonterminalscount: SymbolId = 0,

    pub const NameTypedSymbolsMap = std.StringArrayHashMapUnmanaged(TypedSymbolId);
    pub const TypedSymbolNamesMap = std.AutoArrayHashMapUnmanaged(TypedSymbolId, []const u8);
    const TypedSymbolsList = std.ArrayListUnmanaged(TypedSymbolId);
    const ProdPosTypedSymbolsMap = std.AutoArrayHashMapUnmanaged(Item, TypedSymbolsList);
    const TypedSymbolProdPosMap = std.AutoArrayHashMapUnmanaged(TypedSymbolId, ItemSet);

    pub fn init(allocator: Allocator, grammar: []const Production) !GrammarInfo {
        var result: GrammarInfo = .{};
        try result.addToGrammar(allocator, grammar);
        return result;
    }

    pub fn deinit(self: *GrammarInfo, allocator: Allocator) void {
        self.names_to_syms.deinit(allocator);
        self.syms_to_names.deinit(allocator);
        self.grammar.deinit(allocator);
        for (self.prod_pos_symbols.values()) |*list| list.deinit(allocator);
        self.prod_pos_symbols.deinit(allocator);
        for (self.symbol_prod_pos.values()) |*list| list.deinit(allocator);
        self.symbol_prod_pos.deinit(allocator);
    }

    fn nonterminalscountCheck(self: GrammarInfo) SymbolId {
        var result: SymbolId = 0;
        for (self.names_to_syms.values()) |s| result += @boolToInt(s.ty == .nonterm);
        return result;
    }
    fn terminalscountCheck(self: GrammarInfo) SymbolId {
        var result: SymbolId = 0;
        for (self.names_to_syms.values()) |s| result += @boolToInt(s.ty == .term);
        return result;
    }

    fn populateIdTables(self: *GrammarInfo, allocator: Allocator, sym: Symbol, pos: u16, maxpos: ?*u16, prodid: u16) Error!void {
        trace("populateIdTables() {} pos {} prodid {} \n", .{ sym, pos, prodid });
        if (maxpos) |mp| mp.* = std.math.max(mp.*, pos);
        switch (sym) {
            .char_lit, .str_lit, .sqbkt_lit => |s| {
                trace("lit {s}\n", .{s});
                var id = TypedSymbolId.init(self.terminalscount, .term);
                const gop = try self.names_to_syms.getOrPut(allocator, s);
                if (gop.found_existing) id = gop.value_ptr.* else gop.value_ptr.* = id;
                try self.addProdPosSym(allocator, id, prodid, pos);
                self.terminalscount += @boolToInt(!gop.found_existing);
            },
            .name => |s| {
                trace("name {s}\n", .{s});
                var id = TypedSymbolId.init(self.nonterminalscount, .nonterm);
                const gop = try self.names_to_syms.getOrPut(allocator, s);
                if (gop.found_existing) id = gop.value_ptr.* else gop.value_ptr.* = id;
                try self.addProdPosSym(allocator, id, prodid, pos);
                self.nonterminalscount += @boolToInt(!gop.found_existing);
            },
            .optional, .some, .many, .not => |s| {
                try self.populateIdTables(allocator, s.*, pos, maxpos, prodid);
            },
            .seq, .group => |ss| for (ss) |s, i| {
                try self.populateIdTables(allocator, s, pos + @intCast(u16, i), maxpos, prodid);
            },
            .choice => |ss| for (ss) |s| {
                try self.populateIdTables(allocator, s, pos, maxpos, prodid);
            },
            else => unreachable,
        }
    }

    fn addProdPosSym(self: *GrammarInfo, allocator: Allocator, id: TypedSymbolId, prodid: u16, pos: u16) !void {
        trace("addProdPosSym() id {} prodid {} pos {}\n", .{ id.id, prodid, pos });
        {
            const gop = try self.prod_pos_symbols.getOrPut(allocator, Item.init(prodid, pos));
            if (!gop.found_existing) gop.value_ptr.* = .{};
            try gop.value_ptr.*.append(allocator, id);
        }
        {
            const gop = try self.symbol_prod_pos.getOrPut(allocator, id);
            if (!gop.found_existing) gop.value_ptr.* = .{};
            try gop.value_ptr.*.append(allocator, Item.init(prodid, pos));
        }
    }

    pub fn addToGrammar(self: *GrammarInfo, allocator: Allocator, grammar: []const Production) !void {
        for (grammar) |prod, _prodid| {
            const id = TypedSymbolId.init(self.nonterminalscount, .nonterm);
            // TODO: check for duplicate production names somewhere
            {
                const gop = try self.names_to_syms.getOrPut(allocator, prod.name);
                if (!gop.found_existing) gop.value_ptr.* = id;
                self.nonterminalscount += @boolToInt(!gop.found_existing);
            }
            const prodid = @intCast(u16, _prodid + self.grammar.items.len);

            var maxpos: u16 = 0;
            try self.populateIdTables(allocator, prod.rule.root, 0, &maxpos, prodid);
            trace("-- '{s}' prodid {} terms {} nonterms {} maxpos {}\n", .{ prod.name, prodid, self.terminalscount, self.nonterminalscount, maxpos });
        }

        var it = self.names_to_syms.iterator();
        while (it.next()) |e| {
            const gop = try self.syms_to_names.getOrPut(allocator, e.value_ptr.*);
            if (!gop.found_existing) gop.value_ptr.* = e.key_ptr.*;
        }
        try self.grammar.appendSlice(allocator, grammar);

        trace("glen {} ppslen {} nonterms/terms {}/{} total names/syms {}/{}\n", .{
            self.grammar.items.len,
            self.prod_pos_symbols.count(),
            self.nonterminalscount,
            self.terminalscount,
            self.names_to_syms.count(),
            self.syms_to_names.count(),
        });
        assert(self.nonterminalscount + self.terminalscount == self.names_to_syms.count());
        assert(self.names_to_syms.count() == self.syms_to_names.count());

        assert(self.terminalscount == self.terminalscountCheck());
        assert(self.nonterminalscount == self.nonterminalscountCheck());
    }

    pub fn idToName(ginfo: GrammarInfo, tid: TypedSymbolId) ?[]const u8 {
        return ginfo.syms_to_names.get(tid);
    }
    pub fn nameToTid(ginfo: GrammarInfo, name: []const u8) ?TypedSymbolId {
        return ginfo.names_to_syms.get(name);
    }
    // pub fn nameToSymbolId(ginfo: GrammarInfo, name: []const u8) ?SymbolId {
    //     return if (ginfo.names_to_syms.get(name)) |tid| tid.adjustedId(ginfo) else null;
    // }
    pub fn nameToProdId(ginfo: GrammarInfo, name: []const u8) ?SymbolId {
        for (ginfo.grammar.items) |prod, i| {
            if (mem.eql(u8, name, prod.name)) return @intCast(SymbolId, i);
        }
        return null;
    }
    pub fn nameToItem(ginfo: GrammarInfo, name: []const u8, dotpos: u16) ?ItemSet.Item {
        return if (ginfo.name_to_tyid.get(name)) |tyid|
            ItemSet.Item{ .symid = tyid.id, .dotpos = dotpos }
        else
            null;
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
            if (self.ginfo.idToName(.{ .id = id, .ty = .nonterm })) |_| {
                try writer.print("{s} <- ", .{self.ginfo.grammar.items[id].name});
                var dotpos: u16 = 0;
                var wrotedot = false;
                // const dot: []const u8 = &[_]u8{ 250, ' ' };
                const dot: []const u8 = &[_]u8{ '.', ' ' };
                while (self.ginfo.prod_pos_symbols.get(.{ .id = id, .pos = dotpos })) |tids| : (dotpos += 1) {
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
        try writer.print("{s}", .{self.ginfo.idToName(self.tid)});
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
            const Bs = ginfo.prod_pos_symbols.get(item) orelse continue;
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
    const mprodposlist = ginfo.symbol_prod_pos.get(X);
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
    while (true) {
        const count = itemsets.items.len;
        // trace("itemsets.len {}\n", .{count});
        for (itemsets.items) |I| {
            var next = ItemSet{};
            defer next.deinit(allr);
            // trace("  I: {}\n", .{ItemSetFmt.init(I, ginfo)});
            // trace("  I: {any}\n", .{I.items});
            for (ginfo.syms_to_names.keys()) |X| {
                var G = try goto(allr, I, X, ginfo);
                // trace("    G: {}\n", .{ItemSetFmt.init(G, ginfo)});
                // trace("    G: {any}\n", .{G.items});
                if (G.items.len > 0 and !itemSetsContain(itemsets.items, G))
                    try itemsets.append(allr, G)
                else
                    G.deinit(allr);
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

pub const TypedColRow = struct {
    col: TypedSymbolId,
    row: SymbolId = 0,

    pub const Fmt = struct {
        tcr: TypedColRow,
        ginfo: GrammarInfo,
        pub fn init(tcr: TypedColRow, ginfo: GrammarInfo) Fmt {
            return .{
                .tcr = tcr,
                .ginfo = ginfo,
            };
        }
        pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            try writer.print("{}:{}", .{ TidFmt.init(self.tcr.col, self.ginfo), self.tcr.row });
        }
    };
};

pub fn Action(comptime I: type) type {
    return union(enum) {
        shift,
        reduce: I,
        accept,
        pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            switch (self) {
                .shift => _ = try writer.write("s"),
                .reduce => |r| _ = try writer.print("r{}", .{r}),
                .accept => _ = try writer.write("$"),
            }
        }
    };
}
pub const ActionTable = std.AutoArrayHashMapUnmanaged(TypedColRow, Action(StateId));
pub const GotoTable = std.AutoArrayHashMapUnmanaged(TypedColRow, StateId);
pub const Tables = struct {
    action: ActionTable = .{},
    goto: GotoTable = .{},
    actionrows: u16 = 0,
    gotorows: u16 = 0,
    ginfo: GrammarInfo,
    pub const Type = enum { action, goto };
    pub fn deinit(self: *Tables, allocator: Allocator) void {
        self.action.deinit(allocator);
        self.goto.deinit(allocator);
        self.ginfo.deinit(allocator);
    }

    pub fn putNoClobber(self: *Tables, allocator: Allocator, comptime ty: Type, colrow: TypedColRow, value: anytype) !void {
        switch (ty) {
            .action => {
                const gop = try self.action.getOrPut(allocator, colrow);
                // TODO: handle existing
                if (gop.found_existing)
                    std.debug.print("  found existing\n", .{});
                // if (gop.found_existing) return error.FoundExisting;
                gop.value_ptr.* = value;
                self.actionrows = std.math.max(self.actionrows, colrow.row);
            },
            .goto => {
                const gop = try self.goto.getOrPut(allocator, colrow);
                // TODO: handle existing
                if (gop.found_existing)
                    std.debug.print("  found existing\n", .{});
                // if (gop.found_existing) return error.FoundExisting;
                gop.value_ptr.* = value;
                self.gotorows = std.math.max(self.gotorows, colrow.row);
            },
        }
    }

    fn writeByteNTimesNl(writer: anytype, byte: u8, n: usize) !void {
        try writer.writeByteNTimes(byte, n);
        _ = try writer.write("\n");
    }

    pub fn display(self: Tables, writer: anytype, comptime colwidth: u8) !void {
        var col: SymbolId = 0;
        const itemfmt = comptime std.fmt.comptimePrint("{{: <{}}}", .{colwidth});
        const stritemfmt = comptime std.fmt.comptimePrint("{{s: <{}}}", .{colwidth});
        const maxcol = (self.ginfo.terminalscount + self.ginfo.nonterminalscount) + 3;

        // -- start header
        try writeByteNTimesNl(writer, '-', maxcol * colwidth);
        try writer.print(stritemfmt ++ "action                  goto\n", .{""});
        try writeByteNTimesNl(writer, '-', maxcol * colwidth);
        _ = try writer.print(stritemfmt, .{"state"});
        col = 0;
        while (col <= self.ginfo.terminalscount) : (col += 1) {
            if (col == self.ginfo.terminalscount)
                _ = try writer.print(stritemfmt, .{"$"})
            else
                try writer.print(stritemfmt, .{self.ginfo.idToName(.{ .ty = .term, .id = col })});
        }
        col = 0;
        while (col <= self.ginfo.nonterminalscount) : (col += 1) {
            if (col == self.ginfo.nonterminalscount)
                _ = try writer.print(stritemfmt, .{"$"})
            else
                try writer.print(stritemfmt, .{self.ginfo.idToName(.{ .ty = .nonterm, .id = col })});
        }
        _ = try writer.write("\n");
        try writeByteNTimesNl(writer, '-', maxcol * colwidth);
        // -- end header

        const maxrow = std.math.max(self.actionrows, self.gotorows);
        var row: SymbolId = 0;
        while (row <= maxrow) : (row += 1) {
            try writer.print(itemfmt, .{row});
            col = 0;
            while (col <= self.ginfo.terminalscount) : (col += 1) {
                if (self.goto.get(.{ .row = row, .col = .{ .ty = .term, .id = col } })) |value| {
                    try writer.print(itemfmt, .{value});
                } else try writer.print(stritemfmt, .{" "});
            }
            col = 0;
            while (col <= self.ginfo.nonterminalscount) : (col += 1) {
                if (self.goto.get(.{ .row = row, .col = .{ .ty = .nonterm, .id = col } })) |value| {
                    try writer.print(itemfmt, .{value});
                } else try writer.print(stritemfmt, .{" "});
            }
            try writer.print("\n", .{});
        }
    }
};

pub fn createTables(allocator: Allocator, _ginfo: GrammarInfo, augmented_start: []const u8) !Tables {
    // based on https://pages.github-dev.cs.illinois.edu/cs421-sp20/web/handouts/lr-parsing-tables.pdf

    var tables: Tables = .{ .ginfo = try GrammarInfo.init(allocator, _ginfo.grammar.items) };

    var augmentedstart = try parseGrammar(allocator, std.testing.failing_allocator, augmented_start);
    var auginfo = try GrammarInfo.init(allocator, augmentedstart);
    defer {
        parseFree(allocator, augmentedstart);
        auginfo.deinit(allocator);
    }
    try tables.ginfo.addToGrammar(allocator, auginfo.grammar.items);
    var set0 = try auginfo.itemSetUsing(allocator, tables.ginfo, 0);
    var itemsets = try lr0items(allocator, set0, tables.ginfo);
    defer {
        set0.deinit(allocator);
        for (itemsets.items) |*itemset| itemset.deinit(allocator);
        itemsets.deinit(allocator);
    }
    if (itemsets.items.len == 0) return error.EmptyItemsets;

    var seen: std.AutoArrayHashMapUnmanaged(Item, StateId) = .{};
    defer seen.deinit(allocator);

    for (itemsets.items) |itemset, i| {
        std.debug.print("itemset-{}\n", .{i});
        for (itemset.items) |item, j| {
            const seencount = @intCast(StateId, seen.count());
            const gop = try seen.getOrPut(allocator, item);
            const stateid = if (gop.found_existing) gop.value_ptr.* else blk: {
                gop.value_ptr.* = seencount;
                break :blk seencount;
            };
            std.debug.print("item-{} {}\n", .{ j, Item.Fmt.init(item, tables.ginfo) });
            const msyms = tables.ginfo.prod_pos_symbols.get(item);
            const isfinal = msyms == null;
            if (isfinal) {
                std.debug.print("isfinal \n", .{});
                // Enter a reduce action in the follow set columns, .reduce=sym.id
                // TODO: follow set
                // for now assume follow set = {$}
                const colrow: TypedColRow = .{ .col = .{ .id = tables.ginfo.terminalscount, .ty = .term }, .row = @intCast(SymbolId, i) };
                try tables.putNoClobber(allocator, .action, colrow, .{ .reduce = item.id });
            } else for (msyms.?.items) |sym| {
                const nextstateid = stateid + 1;
                // TODO: final
                if (sym.ty == .term) {
                    const colrow: TypedColRow = .{ .col = sym, .row = @intCast(SymbolId, i) };
                    std.debug.print(
                        "sym {} colrow {} state {} \n",
                        .{ TidFmt.init(sym, tables.ginfo), TypedColRow.Fmt.init(colrow, tables.ginfo), nextstateid },
                    );
                    // Enter a shift action in row n and column α,
                    try tables.putNoClobber(allocator, .action, colrow, .shift);
                    // and enter n′ in the corresponding entry of the goto ts[i].
                    try tables.putNoClobber(allocator, .goto, colrow, nextstateid);
                } else {
                    const colrow: TypedColRow = .{ .col = sym, .row = @intCast(SymbolId, i) };
                    std.debug.print(
                        "sym {} colrow {} state {}\n",
                        .{ TidFmt.init(sym, tables.ginfo), TypedColRow.Fmt.init(colrow, tables.ginfo), nextstateid },
                    );
                    // Enter n′ in the goto table for row n and nonterminal A.
                    try tables.putNoClobber(allocator, .goto, colrow, nextstateid);
                }
            }
        }
    }
    return tables;
}
