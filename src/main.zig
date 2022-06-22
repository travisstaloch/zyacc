const std = @import("std");
const m = @import("mecha");
const assert = std.debug.assert;
const mem = std.mem;
const Allocator = mem.Allocator;

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
    if (@hasDecl(@This(), "showtrace")) std.debug.print(fmt, args);
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
    pub const many_not_dquote = m.many(m.oneOf(.{ escaped_str, m.ascii.not(dquote) }), .{ .collect = false, .min = 1 });
    pub const str_lit = m.combine(.{ dquote, many_not_dquote, dquote });
    pub const many_not_sqbkt_r = m.many(m.ascii.not(sqbkt_r), .{ .collect = false, .min = 1 });
    pub const sqbkt_lit = m.combine(.{ sqbkt_l, many_not_sqbkt_r, sqbkt_r });

    fn convEscapeCharLit(_: Allocator, res: [2]u8) !u8 {
        return switch (res[1]) {
            'n' => '\n',
            '\'' => '\'',
            '\\' => '\\',
            else => unreachable,
        };
    }
    fn convEscapeStrLit(_: Allocator, res: [2]u8) !u8 {
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
            // std.debug.print("s {s}:{s}\n", .{ ss[0], ss[1] });
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
            // std.debug.print("choice last {}\n", .{last});

            const terminators = &.{ .choice, .group_end };
            var next = (try seqUntil(ctx, terminators)) orelse unreachable;
            // std.debug.print("choice pending {}\n", .{ctx.pending_sym});
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

pub fn parse(allr: Allocator, fallr: Allocator, src: []const u8) ![]const Production {
    var ctx = Context.init(allr, fallr, src);
    var prods = std.ArrayList(Production).init(ctx.allr);
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
                std.debug.print("sqbkt_lit c '{c}' restlit[0] '{c}'\n", .{ c, restlit[0] });
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

                std.debug.print("  match {}\n", .{match});
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

pub fn parseWith(allocator: Allocator, fallr: Allocator, prods: []const Production, input: []const u8) !void {
    var terminals = std.StringHashMap(Rule).init(allocator);
    var nonterminals = std.StringHashMap(Rule).init(allocator);
    defer {
        terminals.deinit();
        nonterminals.deinit();
    }
    for (prods) |prod| {
        if (prod.rule.root.choice.len == 1) {
            const firstchoice = prod.rule.root.choice[0];
            std.debug.print("firstchoice {}\n", .{firstchoice});
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
            std.debug.print("token '{s}'\n", .{r.value});
            var termsit = terminals.iterator();
            const matchedname = while (termsit.next()) |ent| {
                const matchlen = try matchLit(ent.value_ptr.*.root.choice[0], r.value);
                std.debug.print("checking {s} matchlen {}\n", .{ ent.key_ptr.*, matchlen });
                if (matchlen == r.value.len) {
                    std.debug.print("matched {} chars with {s} \n", .{ matchlen, ent.key_ptr.* });
                    break ent.key_ptr.*;
                }
            } else null;
            if (matchedname) |name| {
                // std.debug.print("matchedname {s}\n", .{k});
                stack.append(name);
            } else break;
        } else |_| {}
    }

    // {
    //     var it = terminals.iterator();
    //     std.debug.print("terminals \n", .{});
    //     while (it.next()) |ent| {
    //         std.debug.print("  {s}\n", .{ent.key_ptr.*});
    //     }
    // }
    // {
    //     var it = nonterminals.iterator();
    //     std.debug.print("nonterminals \n", .{});
    //     while (it.next()) |ent| {
    //         std.debug.print("  {s}\n", .{ent.key_ptr.*});
    //     }
    // }
}

// pub fn Tables(comptime NTerminals: u16, comptime NSymbols: u16, comptime NStates: u16) type {
//     return struct {
//         action: [NTerminals + 1][NStates]ShiftReduce(u8) = undefined,
//         goto: [NSymbols][NStates]I = undefined,
//         pub const I = std.meta.Int(.unsigned, std.math.log2_int_ceil(NStates));
//     };
// }

pub fn ShiftReduce(comptime I: type) type {
    return union(enum) {
        shift,
        reduce: I,
    };
}

fn populateIdTables(termids: *SymbolIdMap, nontermids: *SymbolIdMap, sym: Symbol) Error!void {
    trace("populateIdTables {}\n", .{sym});
    switch (sym) {
        .char_lit, .str_lit, .sqbkt_lit => |s| try termids.put(s, @intCast(u16, termids.count())),
        .name => |s| try nontermids.put(s, @intCast(u16, nontermids.count())),
        .optional, .some, .many, .not => |s| {
            try populateIdTables(termids, nontermids, s.*);
        },
        .choice, .seq, .group => |ss| for (ss) |s| {
            try populateIdTables(termids, nontermids, s);
        },
        else => unreachable,
    }
}

fn nameId(name: []const u8, terminal_ids: SymbolIdMap, nonterminal_ids: SymbolIdMap, terminalscount: SymbolId) SymbolId {
    return terminal_ids.get(name) orelse
        ((nonterminal_ids.get(name) orelse unreachable) + terminalscount);
}

fn idName(id: SymbolId, terminal_ids: SymbolIdMap, nonterminal_ids: SymbolIdMap, terminalscount: SymbolId) ?SymbolId {
    {
        var it = terminal_ids.iterator();
        while (it.next()) |ent| : (id += 1) {
            if (id == ent.value_ptr.*) return ent.key_ptr.*;
        }
    }
    {
        var it = nonterminal_ids.iterator();
        while (it.next()) |ent| : (id += 1) {
            if (id == ent.value_ptr.* + terminalscount) return ent.key_ptr.*;
        }
    }
    return null;
}

const SymbolId = u16;
const SymbolIdMap = std.StringArrayHashMap(SymbolId);
const StateId = u8;
const ColRow = std.meta.Tuple(&.{ SymbolId, StateId });

const ProdMap = std.StringHashMap(Production);
const ActionTable = std.AutoArrayHashMapUnmanaged(ColRow, ShiftReduce(StateId));
const GotoTable = std.AutoArrayHashMapUnmanaged(ColRow, StateId);
const Tables = struct { action: ActionTable, goto: GotoTable };
const start_symname = "S'";
pub fn createTables(allocator: Allocator, prods: []const Production) !Tables {
    // based on https://pages.github-dev.cs.illinois.edu/cs421-sp20/web/handouts/lr-parsing-tables.pdf
    var prodmap = ProdMap.init(allocator);
    var terminal_ids = SymbolIdMap.init(allocator);
    var nonterminal_ids = SymbolIdMap.init(allocator);
    defer {
        prodmap.deinit();
        terminal_ids.deinit();
        nonterminal_ids.deinit();
    }
    // populate terminal/nonterminal_ids tables
    for (prods) |prod| {
        try prodmap.put(prod.name, prod);
        if (prod.rule.root.choice.len == 1) {
            const firstchoice = prod.rule.root.choice[0];
            trace("firstchoice {}\n", .{firstchoice});
            if (isTerminal(firstchoice)) {
                try terminal_ids.put(prod.name, @intCast(u16, terminal_ids.count()));
                continue;
            }
        }
        if (!mem.eql(u8, prod.name, start_symname))
            try nonterminal_ids.put(prod.name, @intCast(u16, nonterminal_ids.count()));
        try populateIdTables(&terminal_ids, &nonterminal_ids, prod.rule.root);
    }
    const terminalscount = @intCast(SymbolId, terminal_ids.count());
    const nonterminalscount = @intCast(SymbolId, nonterminal_ids.count());

    trace("terminal_ids.count() {}\n", .{terminalscount});
    trace("nonterminal_ids.count() {}\n", .{nonterminalscount});

    var actiontable = ActionTable{};
    var gototable = GotoTable{};
    defer {
        actiontable.deinit(allocator);
        gototable.deinit(allocator);
    }
    var stateid: StateId = 1;

    // state 0: insert initial states (row 0) into goto table
    const start_firstname = try getFirstNameOf(allocator, prodmap.get(start_symname).?);
    const start_firstid = nameId(start_firstname, terminal_ids, nonterminal_ids, terminalscount);
    trace("start_firstname '{s}':{}\n", .{ start_firstname, start_firstid });
    try gototable.put(allocator, .{ start_firstid, 0 }, stateid);
    displayTable("goto", gototable);
    stateid += 1;
    const closure = try getClosure(allocator, start_symname, prodmap);
    defer allocator.free(closure);
    trace("closure.len {}\n", .{closure.len});

    for (closure[1..]) |cl| {
        var pos: u8 = 0;
        var names = std.StringHashMap(void).init(allocator);
        defer names.deinit();
        try getNamesAtPosition(0, cl.rule.root, &names, &pos, null);
        trace("cl {s} names.count {} at pos 0\n", .{ cl.name, names.count() });
        var it = names.iterator();
        while (it.next()) |ent| {
            const id = nameId(ent.key_ptr.*, terminal_ids, nonterminal_ids, terminalscount);
            const gop = try gototable.getOrPut(allocator, .{ id, 0 });
            if (!gop.found_existing) {
                trace("  adding state {} name {s}:{}\n", .{ stateid, ent.key_ptr.*, id });
                gop.value_ptr.* = stateid;
                stateid += 1;
            }
        }
    }
    displayTable("goto", gototable);

    // move the dotpos cursor through each position for each production
    // and check for names at that position.
    // if found, insert ((col, row), stateid) entries into the tables
    var dotpos: StateId = 0;
    stateid = 1;
    outer: while (true) : (dotpos += 1) {
        for (prods) |prod| {
            var names = std.StringHashMap(void).init(allocator);
            defer names.deinit();
            var pos: StateId = 0;
            var maxpos: StateId = 0;
            try getNamesAtPosition(dotpos, prod.rule.root, &names, &pos, &maxpos);
            if (dotpos == maxpos) {
                trace("{s}: atmaxpos {}\n", .{ prod.name, maxpos });
            }
            var it = names.iterator();
            while (it.next()) |ent| {
                const name = ent.key_ptr.*;
                const id = nameId(name, terminal_ids, nonterminal_ids, terminalscount);
                trace("{s}:{} dotpos/maxpos {}/{} names {s}\n", .{ prod.name, id, dotpos, maxpos, name });
            }
        }
        if (dotpos == 4) break :outer;
    }

    return Tables{ .action = actiontable, .goto = gototable };
}

fn displayTable(tablename: []const u8, table: anytype) void {
    var it = table.iterator();
    while (it.next()) |ent|
        trace("{s} table entry: {}={}\n", .{ tablename, ent.key_ptr.*, ent.value_ptr.* });
}

fn getClosure(allocator: Allocator, _name: []const u8, prodmap: ProdMap) Error![]const Production {
    var stack = std.ArrayList([]const u8).init(allocator);
    defer stack.deinit();
    try stack.append(_name);

    var result = std.ArrayList(Production).init(allocator);
    while (stack.popOrNull()) |name| {
        if (prodmap.get(name)) |prod| {
            const found = for (result.items) |it| {
                if (mem.eql(u8, it.name, prod.name)) break true;
            } else false;
            if (found) continue;
            try result.append(prod);
            var names = std.StringHashMap(void).init(allocator);
            defer names.deinit();
            var pos: u8 = 0;
            try getNamesAtPosition(0, prod.rule.root, &names, &pos, null);
            var it = names.iterator();
            while (it.next()) |ent| try stack.append(ent.key_ptr.*);
        }
    }
    return result.toOwnedSlice();
}

fn getNamesAtPosition(dotpos: u8, sym: Symbol, names: *std.StringHashMap(void), pos: *u8, mmaxpos: ?*u8) Error!void {
    if (mmaxpos) |maxpos| maxpos.* = std.math.max(maxpos.*, pos.*);
    switch (sym) {
        .char_lit, .str_lit, .sqbkt_lit, .name => |s| if (pos.* == dotpos) try names.put(s, {}),
        .optional, .some, .many, .not => |s| {
            try getNamesAtPosition(dotpos, s.*, names, pos, mmaxpos);
        },
        .seq, .group => |ss| for (ss) |s, i| {
            var pos2 = pos.* + @intCast(u8, i);
            try getNamesAtPosition(dotpos, s, names, &pos2, mmaxpos);
        },
        .choice => |ss| for (ss) |s| {
            try getNamesAtPosition(dotpos, s, names, pos, mmaxpos);
        },
        else => unreachable,
    }
}

fn getFirstNameOf(allocator: Allocator, prod: Production) ![]const u8 {
    var names = std.StringHashMap(void).init(allocator);
    defer names.deinit();
    var pos: u8 = 0;
    try getNamesAtPosition(0, prod.rule.root, &names, &pos, null);
    trace("getFirstNameOf {s} names.count() {}\n", .{ prod.name, names.count() });
    var it = names.iterator();
    return if (names.count() == 1) it.next().?.key_ptr.* else error.NamesCountNotEqOne;
}
