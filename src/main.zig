//! June 2022 - Travis Staloch
//! Much of the work here has been ported from
//!   https://mdaines.github.io/grammophone/assets/application.js
//! In many places i have left the original javascript code in comments above the ported code.
//! This implementation differs from the original work in that each production has a 'name' field
//! separate from the other symbols which make up a rule.  In grammophone.js the name is included
//! as the first element in the 'productions[i]' array.
const std = @import("std");
const m = @import("mecha");
const assert = std.debug.assert;
const mem = std.mem;
const Allocator = mem.Allocator;

pub const Token = struct {
    tag: Tag,
    id: SymbolId,

    pub const Tag = enum {
        _name_arrow,
        comment,
        dot,
        group_end,
        char_lit,
        str_lit,
        sqbkt_lit,
        name,
        optional,
        some,
        many,
        not,
        choice,
        group,
    };

    pub fn init(tag: Tag, id: SymbolId) Token {
        return .{
            .tag = tag,
            .id = id,
        };
    }

    pub fn eql(a: Token, b: Token) bool {
        return a.id == b.id;
    }

    pub const Fmt = struct {
        token: Token,
        g: Grammar,
        pub fn init(token: Token, g: Grammar) Fmt {
            return .{
                .token = token,
                .g = g,
            };
        }

        pub fn format(self: Fmt, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            // try writer.print("{s}-{}", .{ self.g.name(self.token.id), self.token.id });
            try writer.print("{s}", .{self.g.name(self.token.id)});
        }
    };
};

pub const SymbolList = std.ArrayListUnmanaged(Symbol);
pub const SymbolListList = std.ArrayListUnmanaged(SymbolList);
pub const Symbol = struct {
    flags: Flags = .{},
    sym: Sym,
    pub fn deinit(symbol: *Symbol, allocator: Allocator) void {
        switch (symbol.sym) {
            .group => |*list| {
                for (list.items) |*it| it.deinit(allocator);
                list.deinit(allocator);
            },
            .choice => |*listlist| {
                for (listlist.items) |*list| {
                    for (list.items) |*it| it.deinit(allocator);
                    list.deinit(allocator);
                }
                listlist.deinit(allocator);
            },
            else => {},
        }
    }
    pub const Sym = union(enum) {
        token: Token,
        group: SymbolList,
        choice: SymbolListList,
        pub const Tag = std.meta.Tag(Sym);
    };
    pub const Flag = enum { optional, some, many, not };
    pub const Flags = std.EnumSet(Flag);

    pub fn eqlId(a: Symbol, b: Symbol) bool {
        return a.sym == .token and b.sym == .token and a.sym.token.id == b.sym.token.id;
    }
    pub const Fmt = struct {
        symbol: Symbol,
        g: Grammar,
        pub fn init(symbol: Symbol, g: Grammar) Fmt {
            return .{
                .symbol = symbol,
                .g = g,
            };
        }

        pub fn format(self: Fmt, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            switch (self.symbol.sym) {
                .token => |token| try writer.print("{}", .{Token.Fmt.init(token, self.g)}),
                .group => |g| {
                    _ = try writer.write("(");
                    for (g.items) |sym| try writer.print("{} ", .{Symbol.Fmt.init(sym, self.g)});
                    _ = try writer.write(")");
                },
                .choice => |cs| {
                    for (cs.items) |choice, i| {
                        if (i != 0) _ = try writer.write(" /");
                        for (choice.items) |sym|
                            try writer.print("{} ", .{Symbol.Fmt.init(sym, self.g)});
                    }
                },
            }
        }
    };
};

pub const State = struct {
    kernel: ItemSet,
    items: ItemSet,
    transitions: std.AutoArrayHashMapUnmanaged(StateId, StateId),

    fn deinit(s: *State, allocator: Allocator) void {
        s.kernel.deinit(allocator);
        s.items.deinit(allocator);
        s.transitions.deinit(allocator);
    }
};

pub const ItemSet = std.AutoArrayHashMapUnmanaged(Item, void);
pub const ItemSetSet = std.AutoArrayHashMapUnmanaged(SymbolId, ItemSet);
pub fn itemSetFromItems(allocator: Allocator, items: []const Item) !ItemSet {
    var result: ItemSet = .{};
    for (items) |item|
        try result.put(allocator, item, {});
    return result;
}
pub const ItemSetFmt = struct {
    itemset: ItemSet,
    g: Grammar,
    pub fn init(itemset: ItemSet, g: Grammar) ItemSetFmt {
        return .{
            .itemset = itemset,
            .g = g,
        };
    }

    pub fn format(self: ItemSetFmt, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        _ = try writer.write("{");
        for (self.itemset.keys()) |item, i| {
            if (i != 0) _ = try writer.write(", ");
            const prod = if (item.id == Automaton.augmented_id)
                self.g.augprod
            else
                self.g.productions.items[item.id];
            try writer.print("{s}:{}", .{ self.g.name(prod.name.id), item });
        }
        _ = try writer.write("}");
    }
};

pub const Item = struct {
    id: SymbolId,
    pos: u16,
    pub fn init(id: SymbolId, pos: u16) Item {
        return .{
            .id = id,
            .pos = pos,
        };
    }
    pub fn format(self: Item, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        _ = try writer.print("{}-{}", .{ self.id, self.pos });
    }
};

pub const SymbolId = u16;
pub const SymbolIdList = std.ArrayListUnmanaged(SymbolId);
pub const StateId = u16;

pub const LR1Table = struct {
    pub const Entry = struct {
        id: SymbolId,
        row: StateId,
        shift: SymbolId,
        reduce: []const SymbolId,
    };
};

pub const Grammar = struct {
    productions: std.ArrayListUnmanaged(Production) = .{},
    buf: []u8 = &.{},
    augprod: Production = undefined,
    name_ids: SymbolNamesMap = .{},
    id_names: NameSymbolsMap = .{},
    nonterminals: SymbolIdSet = .{},
    src: []const u8,

    pub fn init(allr: Allocator, fallr: Allocator, src: []const u8) !Grammar {
        var result: Grammar = .{ .src = src };
        var ctx = Context.init(allr, fallr, src);
        var i: SymbolId = 0;
        while (try ctx.nextProduction(&result)) |prods| : (i += 1) {
            for (prods.items) |prod|
                try result.productions.append(allr, prod);
            if (prods.items.len > 0)
                try result.nonterminals.putNoClobber(allr, prods.items[0].name.id, {});
        }
        // TODO: honor paren groupings
        // add augmented production at grammar.augprod and fixup its associated data using Automaton.augmented_id
        if (result.productions.items.len > 0) {
            const startname = result.id_names.get(result.start().name.id).?;
            result.buf = try std.fmt.allocPrint(allr, "{s}' <- {s}", .{ startname, startname });
            // trace("augsrc: {s}\n", .{augsrc});
            ctx.rest = result.buf;
            assert(ctx.mprodname == null);
            var prods = (try ctx.nextProduction(&result)).?;
            defer prods.deinit(allr);
            assert(prods.items.len == 1);
            result.augprod = prods.items[0];
            // trace("name(augprod.name): {s}\n", .{result.name(result.augprod.name)});
            const aname = result.name(result.augprod.name.id).?;
            // replace given id with Automaton.augmented_id in 'name_ids' and 'id_names'
            const idptr = result.name_ids.getPtr(aname).?;
            _ = result.id_names.swapRemove(result.augprod.name.id);
            idptr.* = Automaton.augmented_id;
            try result.id_names.put(allr, Automaton.augmented_id, aname);
            result.augprod.name.id = Automaton.augmented_id;
            // note: we don't include S' in nonterminals
        }
        try result.name_ids.putNoClobber(allr, "$", Grammar.END);
        try result.id_names.putNoClobber(allr, Grammar.END, "$");
        try result.name_ids.putNoClobber(allr, "$accept", Automaton.accept);
        try result.id_names.putNoClobber(allr, Automaton.accept, "$accept");
        return result;
    }
    pub fn deinit(grammar: *Grammar, allr: Allocator) void {
        for (grammar.productions.items) |*prod| prod.deinit(allr);
        grammar.productions.deinit(allr);
        grammar.augprod.deinit(allr);
        grammar.name_ids.deinit(allr);
        grammar.id_names.deinit(allr);
        grammar.nonterminals.deinit(allr);
        allr.free(grammar.buf);
    }
    pub fn start(grammar: Grammar) Production {
        return grammar.productions.items[0];
    }
    pub fn name(grammar: Grammar, sid: SymbolId) ?[]const u8 {
        return grammar.id_names.get(sid);
    }

    pub fn id(g: *Grammar, allocator: Allocator, sym: []const u8) !SymbolId {
        const nextid = @intCast(SymbolId, g.name_ids.count());
        const gop = try g.name_ids.getOrPut(allocator, sym);
        return if (gop.found_existing) gop.value_ptr.* else blk: {
            gop.value_ptr.* = nextid;
            try g.id_names.put(allocator, nextid, sym);
            break :blk nextid;
        };
    }
    const SymbolIdSet = std.AutoArrayHashMapUnmanaged(SymbolId, void);
    fn nullable(grammar: Grammar, allocator: Allocator) !SymbolIdSet {
        var result: SymbolIdSet = .{};
        var added: SymbolIdSet = .{}; // TODO: could be an array list
        defer added.deinit(allocator);
        while (true) {
            // added = [];
            added.clearRetainingCapacity();

            // for (i = 0; i < grammar.productions.length; i++) {
            //     for (j = 1; j < grammar.productions[i].length; j++) {
            //       if (!nullable[grammar.productions[i][j]])
            //         break;
            for (grammar.productions.items) |prod| {
                const j = for (prod.rule.items) |item, j| {
                    if (!result.contains(item.sym.token.id))
                        break j;
                } else prod.rule.items.len;

                // if (j == grammar.productions[i].length && !nullable[head]) {
                //   nullable[head] = true;
                //   added.push(head);
                const head = prod.name.id;
                if (j == prod.rule.items.len and !result.contains(head)) {
                    try result.putNoClobber(allocator, head, {});
                    try added.put(allocator, head, {});
                }
            }
            if (added.count() == 0) break;
        }

        return result;
    }

    pub fn Relation(comptime T: type) type {
        return struct {
            map: std.AutoArrayHashMapUnmanaged(T, TSet) = .{},
            pub const TSet = std.AutoArrayHashMapUnmanaged(T, void);
            const Self = @This();
            pub fn deinit(self: *Self, allocator: Allocator) void {
                self.map.deinit(allocator);
            }
            pub fn add(relation: *Self, allocator: Allocator, s: T, t: T) !void {
                // relation[s] = relation[s] || {};
                const gop = try relation.map.getOrPut(allocator, s);
                if (!gop.found_existing) gop.value_ptr.* = .{};
                try gop.value_ptr.put(allocator, t, {});
                // relation[s][t] = true;
            }

            pub fn has(relation: Self, i: T, j: T) bool {
                if (relation.map.get(i)) |ls| return ls.get(j) != null;
                return false;
            }

            /// Given a relation, return its transitive closure as a new object.
            /// (floyd-warshall)
            pub fn closure(allocator: Allocator, relation: Self) !Self {
                var result: Self = .{};
                var keys: std.AutoArrayHashMapUnmanaged(T, void) = .{};
                defer keys.deinit(allocator);

                // Copy the relation and build the set of keys

                // for (i in relation) {
                //   keys[i] = true;
                //   for (j in relation[i]) {
                //     keys[j] = true;
                //     result[i] = result[i] || {};
                //     result[i][j] = relation[i][j];
                for (relation.map.keys()) |i| {
                    try keys.put(allocator, i, {});
                    const js = relation.map.get(i) orelse continue;
                    for (js.keys()) |j| {
                        try keys.put(allocator, j, {});
                        const gop = try result.map.getOrPut(allocator, i);
                        if (!gop.found_existing) gop.value_ptr.* = .{};
                        try gop.value_ptr.put(allocator, j, {});
                    }
                }

                // for (i in keys) {
                //   result[i] = result[i] || {};
                for (keys.keys()) |i| {
                    const gop = try result.map.getOrPut(allocator, i);
                    if (!gop.found_existing) gop.value_ptr.* = .{};
                }

                // Perform transitive closure
                // for (k in keys) {
                //   for (i in keys) {
                //     for (j in keys) {
                //       if (result[i][j] || (result[i][k] && result[k][j]))
                //         result[i][j] = true;
                for (keys.keys()) |k| {
                    for (keys.keys()) |i| {
                        for (keys.keys()) |j| {
                            if (result.has(i, j) or (result.has(i, k) and result.has(k, j))) {
                                var l = result.map.get(i) orelse unreachable;
                                try l.put(allocator, j, {});
                            }
                        }
                    }
                }

                return result;
            }
            fn dump(r: Self) void {
                for (r.map.keys()) |k|
                    trace("{}: {any}\n", .{ k, r.map.get(k).?.keys() });
            }
            pub fn propagate(immediate: *Self, allocator: Allocator, propagation: Self) !Self {
                var result: Self = .{};
                var closed = try Self.closure(allocator, propagation);
                defer closed.map.deinit(allocator);
                // trace("propagate closed\n", .{});
                // closed.dump();
                // for (k in immediate) {
                //   for (l in immediate[k]) {
                //     result[k] = result[k] || {};
                //     result[k][l] = immediate[k][l];
                {
                    var it = immediate.map.iterator();
                    while (it.next()) |ent| {
                        const k = ent.key_ptr.*;
                        const gop = try result.map.getOrPut(allocator, k);
                        if (!gop.found_existing) gop.value_ptr.* = .{};
                        const ls = ent.value_ptr.*;
                        const immks = immediate.map.get(k) orelse unreachable;
                        for (ls.keys()) |l| {
                            try gop.value_ptr.put(allocator, l, immks.get(l) orelse continue);
                        }
                    }
                }
                // trace("propagate result1\n", .{});
                // result.dump();
                // for (s in closed) {
                //   for (t in closed[s]) {
                //     for (u in immediate[t]) {
                //       result[s] = result[s] || {};
                //       result[s][u] = immediate[t][u];
                {
                    var it = closed.map.iterator();
                    while (it.next()) |ent| {
                        const s = ent.key_ptr.*;
                        const gop = try result.map.getOrPut(allocator, s);
                        if (!gop.found_existing) gop.value_ptr.* = .{};
                        const ts = ent.value_ptr.*;
                        for (ts.keys()) |t| {
                            const us = immediate.map.get(t) orelse unreachable;
                            for (us.keys()) |u| {
                                try gop.value_ptr.put(allocator, u, {});
                            }
                        }
                    }
                }
                // trace("propagate result2\n", .{});
                // result.dump();
                return result;
            }

            pub fn fmt(self: Self, g: Grammar) Fmt {
                return Fmt.init(self, g);
            }

            pub const Fmt = struct {
                r: Self,
                g: Grammar,
                pub fn init(r: Self, g: Grammar) Fmt {
                    return .{
                        .r = r,
                        .g = g,
                    };
                }

                pub fn format(self: Fmt, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
                    var it = self.r.map.iterator();
                    while (it.next()) |ent| {
                        const k = ent.key_ptr.*;
                        try writer.print("{s}-{}: ", .{ self.g.name(k), k });
                        for (ent.value_ptr.*.keys()) |v| {
                            try writer.print("{s}-{}, ", .{ self.g.name(v), v });
                        }
                    }
                }
            };
        };
    }

    pub fn isNonTerminal(grammar: Grammar, symbol: Symbol) bool {
        return symbol.sym == .token and grammar.nonterminals.contains(symbol.sym.token.id);
    }
    pub const Error = error{} || Allocator.Error;

    pub fn first(grammar: Grammar, allocator: Allocator) Error!Relation(SymbolId) {
        var _nullable = try grammar.nullable(allocator);
        trace("first nullable {any}\n", .{_nullable.keys()});
        var immediate: Relation(SymbolId) = .{};
        var propagation: Relation(SymbolId) = .{};
        defer {
            _nullable.deinit(allocator);
            immediate.map.deinit(allocator);
            propagation.map.deinit(allocator);
        }

        // For each production, add the first terminal symbol after a sequence of nullable symbols.

        for (grammar.productions.items) |prod| {

            // Skip nullable symbols...

            const j = for (prod.rule.items) |item, j| {
                if (!_nullable.contains(item.sym.token.id))
                    break j;
            } else prod.rule.items.len;

            // If the first non-nullable symbol is a terminal, add it to the immediate first set
            // of this nonterminal.

            // if (j < grammar.productions[i].length && !nonterminals[grammar.productions[i][j]])
            //   Relation.add(immediate, grammar.productions[i][0], grammar.productions[i][j]);
            if (j < prod.rule.items.len) {
                const item = prod.rule.items[j];
                if (!grammar.isNonTerminal(item)) {
                    try immediate.add(allocator, prod.name.id, item.sym.token.id);
                }
            }
        }

        trace("first1 immediate {any} propagation {any}\n", .{ immediate.fmt(grammar), propagation.fmt(grammar) });

        // For each production, add the prefix of nullable nonterminals, and then the next symbol
        // if it is also a nonterminal.

        for (grammar.productions.items) |prod| {
            for (prod.rule.items) |item| {

                // Is it a nonterminal? Add it.
                // if (nonterminals[grammar.productions[i][j]])
                //     Relation.add(propagation, grammar.productions[i][0], grammar.productions[i][j]);
                if (grammar.isNonTerminal(item))
                    try propagation.add(allocator, prod.name.id, item.sym.token.id);

                // Is it not nullable? Stop.

                // if (!nullable[grammar.productions[i][j]])
                //   break;
                if (!_nullable.contains(item.sym.token.id))
                    break;
            }
        }

        trace("first2 immediate {any} propagation {any}\n", .{ immediate.fmt(grammar), propagation.fmt(grammar) });
        // Propagate the relation.

        var result = try immediate.propagate(allocator, propagation);
        trace("first3 immediate {any} propagation {any} result {}\n", .{ immediate.fmt(grammar), propagation.fmt(grammar), result.fmt(grammar) });
        // Ensure that all nonterminals are present as keys, even if that particular follow set is empty.

        // for (k in nonterminals) {
        //   if (typeof result[k] === "undefined")
        //     result[k] = {};
        // }
        for (grammar.nonterminals.keys()) |ntid| {
            const gop = try result.map.getOrPut(allocator, ntid);
            if (!gop.found_existing) gop.value_ptr.* = .{};
        }

        return result;
    }

    pub const END = Automaton.end;

    pub fn follow(grammar: Grammar, allocator: Allocator) !Relation(SymbolId) {
        var firsts = try grammar.first(allocator);
        defer firsts.map.deinit(allocator);
        var _nullable = try grammar.nullable(allocator);
        defer _nullable.deinit(allocator);
        const _start = grammar.start();

        var immediate: Relation(SymbolId) = .{};
        var propagation: Relation(SymbolId) = .{};
        defer {
            immediate.map.deinit(allocator);
            propagation.map.deinit(allocator);
        }

        // Add the end of input symbol to the immediate follow set of the grammar's start symbol.

        // Relation.add(immediate, start, Grammar.END);
        try immediate.add(allocator, _start.name.id, END);

        // Given a production X -> ... A β, follow(A) includes first(β), except for the empty string.

        for (grammar.productions.items) |prod| {
            for (prod.rule.items) |item, j| {

                // If the symbol is a nonterminal...

                // if (nonterminals[grammar.productions[i][j]]) {
                if (grammar.isNonTerminal(item)) {

                    // Add the first set of the remaining symbols to the follow set of the symbol

                    for (prod.rule.items[j + 1 ..]) |item2| {

                        // If this symbol is a terminal, add it, and then stop adding.

                        // if (!nonterminals[grammar.productions[i][k]]) {
                        //   Relation.add(immediate, grammar.productions[i][j], grammar.productions[i][k]);
                        //   break;
                        // }
                        if (!grammar.isNonTerminal(item2)) {
                            try immediate.add(allocator, item.sym.token.id, item2.sym.token.id);
                            break;
                        }

                        // If it is a nonterminal, add the first set of that nonterminal.

                        // for (s in first[grammar.productions[i][k]])
                        //   Relation.add(immediate, grammar.productions[i][j], s);
                        if (firsts.map.get(item2.sym.token.id)) |kfirst| {
                            for (kfirst.keys()) |s|
                                try immediate.add(allocator, item.sym.token.id, s);
                        }

                        // Stop if it isn't nullable.

                        // if (!nullable[grammar.productions[i][k]])
                        //   break;
                        if (!_nullable.contains(item2.sym.token.id))
                            break;
                    }
                }
            }
        }
        // Given a production B -> ... A β where β is nullable or is the empty string, follow(A) includes follow(B)

        for (grammar.productions.items) |prod| {
            // Scan from the end of the right side of the production to the beginning...
            var j: usize = prod.rule.items.len - 1;
            while (true) : (j -= 1) {
                const item = prod.rule.items[j];

                // If the symbol is a nonterminal, add the left side.

                // if (nonterminals[grammar.productions[i][j]])
                //   Relation.add(propagation, grammar.productions[i][j], grammar.productions[i][0]);
                if (grammar.isNonTerminal(item))
                    try propagation.add(allocator, item.sym.token.id, prod.name.id);
                // If it isn't nullable, stop.

                // if (!nullable[grammar.productions[i][j]])
                //   break;
                if (!_nullable.contains(item.sym.token.id) or j == 0)
                    break;
            }
        }

        // Propagate the relation

        // var result = Relation.propagate(immediate, propagation);
        var result = try immediate.propagate(allocator, propagation);

        // Ensure that all nonterminals are present as keys, even if that particular follow set is empty.

        // for (k in nonterminals) {
        //   if (typeof result[k] === "undefined")
        //     result[k] = {};
        // }
        for (grammar.nonterminals.keys()) |ntid| {
            const gop = try result.map.getOrPut(allocator, ntid);
            if (!gop.found_existing) gop.value_ptr.* = .{};
        }

        return result;
    }

    // account for item.id == augmented_id by using the start symbol name
    pub fn itemSymbolId(grammar: Grammar, item: Item, _start: Production) ?SymbolId {
        // If the production is the augmented start production, we're looking
        // for the original start symbol. Otherwise, use the grammar's productions
        // to find the symbol, but add one to account for the left-hand side of
        // the production.

        // if (item.production === -1)
        //   symbol = [start][item.index];
        // else
        //   symbol = grammar.productions[item.production][item.index + 1];
        const isaug = item.id == Automaton.augmented_id;
        const prod = if (isaug)
            _start
        else
            grammar.productions.items[item.id];

        return if (isaug) blk: {
            break :blk if (item.pos == 0)
                prod.name.id
            else if (item.pos < prod.rule.items.len)
                prod.rule.items[item.pos].sym.token.id
            else
                null;
        } else if (item.pos < prod.rule.items.len)
            prod.rule.items[item.pos].sym.token.id
        else
            null;
    }
};

pub const TokenList = std.ArrayListUnmanaged(Token);
pub const TokenListList = std.ArrayListUnmanaged(TokenList);

pub const Production = struct {
    name: Token,
    rule: SymbolList,

    pub fn deinit(prod: *Production, allocator: Allocator) void {
        for (prod.rule.items) |*it| {
            it.deinit(allocator);
        }
        prod.rule.deinit(allocator);
    }

    pub const Fmt = struct {
        p: Production,
        g: Grammar,
        pub fn init(pr: Production, g: Grammar) Fmt {
            return .{ .p = pr, .g = g };
        }
        pub fn format(self: Fmt, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            try writer.print("{s} <- ", .{self.g.name(self.p.name.id)});
            for (self.p.rule.items) |sym| {
                try writer.print("{} ", .{Symbol.Fmt.init(sym, self.g)});
            }
        }
    };
};

// const showtrace = true;
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
    pub const somedigits = m.many(m.ascii.digit(10), .{ .min = 1, .collect = false });
    pub const manyalphanum = m.many(alphanum, .{ .collect = false });
    pub const ws = m.discard(m.many(m.ascii.space, .{ .collect = false }));
    pub const somenonws = m.discard(m.many(m.ascii.not(m.ascii.space), .{ .collect = false, .min = 1 }));

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
pub const SymbolNamesMap = std.StringArrayHashMapUnmanaged(SymbolId);
pub const NameSymbolsMap = std.AutoArrayHashMapUnmanaged(SymbolId, []const u8);
pub const Context = struct {
    allr: Allocator,
    /// dummy failing allocator for mecha parsers which don't do any allocations
    fallr: Allocator,
    rest: []const u8,
    /// prodname is be stored so that when a new 'name <-' is found,
    /// this previous one gets applied to all tokens between
    mprodname: ?Token = null,

    pub fn eof(self: Context) bool {
        return self.rest.len == 0;
    }

    pub fn init(allr: Allocator, fallr: Allocator, src: []const u8) Context {
        return .{
            .allr = allr,
            .fallr = fallr,
            .rest = src,
        };
    }

    pub fn nextToken(ctx: *Context, g: *Grammar) !?Token {
        if (p.ws(ctx.fallr, ctx.rest)) |r| { // skip ws
            ctx.rest = r.rest;
        } else |_| {}

        const parsers = [_]std.meta.Tuple(&.{ m.Parser([]const u8), Token.Tag }){
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
                const id = if (std.mem.indexOfScalar(Token.Tag, &.{ .name, .char_lit, .str_lit, .sqbkt_lit }, tag) != null)
                    try g.id(ctx.allr, r.value)
                else
                    Automaton.accept;
                trace("nextToken '{s}':{}\n", .{ r.value, id });
                if (tag == .name) {
                    if (p.ws_arrow(ctx.fallr, ctx.rest)) |r2| {
                        ctx.rest = r2.rest;
                        return Token.init(._name_arrow, id);
                    } else |_| {}
                }

                return Token.init(tag, id);
            } else |_| {}
        }

        return null;
    }

    pub fn indexOfScalarPosFn(comptime T: type, slice: []const T, start_index: usize, func: fn (T) bool) ?usize {
        var i: usize = start_index;
        while (i < slice.len) : (i += 1) {
            if (func(slice[i])) return i;
        }
        return null;
    }

    fn splitRule(allocator: Allocator, tokens: []Symbol, comptime tag: Token.Tag) !SymbolListList {
        var result = SymbolListList{};
        var i: usize = 0;
        const istag = struct {
            fn func(tok: Symbol) bool {
                return tok.sym == .token and tok.sym.token.tag == tag;
            }
        }.func;
        while (indexOfScalarPosFn(Symbol, tokens, i, istag)) |idx| {
            trace("splitRule() {}/{}\n", .{ idx, tokens.len });
            if (i < idx) try result.append(allocator, arrayListFrom(Symbol, tokens[i..idx]));
            i = idx + 1;
        }
        if (i < tokens.len)
            try result.append(allocator, arrayListFrom(Symbol, tokens[i..]));
        return result;
    }

    fn peekTop(comptime T: type, list: std.ArrayListUnmanaged(T)) ?*T {
        return if (list.items.len == 0) null else &list.items[list.items.len - 1];
    }

    pub const Error = error{GrammarError} || mem.Allocator.Error;
    fn tokensToSymbols(allocator: Allocator, tokens: []Token, i: *usize, depth: usize) Error!SymbolList {
        // trace("tokensToSymbols tokens.len {}\n", .{tokens.len});
        var result: SymbolList = .{};

        while (i.* < tokens.len) : (i.* += 1) {
            const token = tokens[i.*];
            // trace("  tokensToSymbols token {}\n", .{token});
            const mprevsym = peekTop(Symbol, result);
            switch (token.tag) {
                .optional => if (mprevsym) |sym|
                    sym.flags.insert(.optional)
                else
                    return error.GrammarError,
                .some => if (mprevsym) |sym|
                    sym.flags.insert(.some)
                else
                    return error.GrammarError,
                .many => if (mprevsym) |sym|
                    sym.flags.insert(.many)
                else
                    return error.GrammarError,
                .not => todo(),
                .group_end => return result,
                .group => {
                    i.* += 1;
                    const group = try tokensToSymbols(allocator, tokens, i, depth + 1);
                    try result.append(allocator, .{ .sym = .{ .group = group } });
                    assert(tokens[i.*].tag == .group_end);
                },
                .choice => if (depth == 0)
                    try result.append(allocator, .{ .sym = .{ .token = token } })
                else {
                    var choice = if (mprevsym != null and mprevsym.?.sym == .choice)
                        mprevsym.?.sym.choice
                    else
                        SymbolListList{};
                    try choice.append(allocator, result);
                    result = .{};
                    try result.append(allocator, .{ .sym = .{ .choice = choice } });
                },
                else => {
                    var sink = if (mprevsym != null and mprevsym.?.sym == .choice)
                        peekTop(SymbolList, mprevsym.?.sym.choice) orelse unreachable
                    else
                        &result;
                    try sink.append(allocator, .{ .sym = .{ .token = token } });
                },
            }
        }
        return result;
    }

    fn arrayListFrom(comptime T: type, items: []T) std.ArrayListUnmanaged(T) {
        return .{
            .items = items,
            .capacity = items.len,
        };
    }
    pub const ProductionList = std.ArrayListUnmanaged(Production);
    pub fn parseChoice(allr: Allocator, tokens: []Token, prodname: Token) !ProductionList {
        var i: usize = 0;
        var symbols = try tokensToSymbols(allr, tokens, &i, 0);
        const rules = try splitRule(allr, symbols.toOwnedSlice(allr), .choice);
        var result: ProductionList = .{};
        for (rules.items) |rule| try result.append(allr, .{ .name = prodname, .rule = rule });
        return result;
    }

    pub fn nextProduction(ctx: *Context, g: *Grammar) !?ProductionList {
        var tokens = TokenList{};
        // defer tokens.deinit(ctx.allr);
        while (try ctx.nextToken(g)) |tok| {
            // trace("tok {s}-{s}\n", .{ g.name(tok.id), @tagName(tok.tag) });
            if (tok.tag == ._name_arrow) {
                defer ctx.mprodname = tok;
                // trace("_name_arrow {s}-{s}\n", .{ g.name(tok), @tagName(tok.tag) });
                if (ctx.mprodname) |prodname|
                    return try parseChoice(ctx.allr, tokens.toOwnedSlice(ctx.allr), prodname);
            } else try tokens.append(ctx.allr, tok);

            // trace("{}-{s}\n", .{ g.Token.Fmt.init(tok, src), @tagName(tok.tag) });
        }
        if (ctx.mprodname) |prodname| {
            defer ctx.mprodname = null;
            return try parseChoice(ctx.allr, tokens.toOwnedSlice(ctx.allr), prodname);
        }
        return null;
    }
};
pub const Automaton = struct {
    kernel: ItemSet,
    closure: ItemSet,
    transitions: Transitions,

    pub fn deinit(a: *Automaton, allocator: Allocator) void {
        for (a.transitions.items) |*it| it.deinit(allocator);
        a.transitions.deinit(allocator);
        a.kernel.deinit(allocator);
        a.closure.deinit(allocator);
    }
    // eql: EqlFn,

    pub const Transitions = std.ArrayListUnmanaged(State);
    pub const TransitionsFmt = struct {
        ts: Transitions,
        g: Grammar,

        pub fn init(ts: Transitions, g: Grammar) TransitionsFmt {
            return .{ .ts = ts, .g = g };
        }

        pub fn format(self: TransitionsFmt, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            for (self.ts.items) |state| {
                try writer.print("{s}-{}-{any}", .{
                    ItemSetFmt.init(state.kernel, self.g),
                    ItemSetFmt.init(state.items, self.g),
                    state.transitions.keys(),
                });
            }
        }
    };
    pub fn init(allocator: Allocator, grammar: Grammar, build: Build) Error!Automaton {
        var result: Automaton = .{ .kernel = .{}, .closure = .{}, .transitions = .{} };

        var initkernel = try itemSetFromItems(allocator, build.initial);
        trace("init() kernel.count {}\n", .{initkernel.count()});
        try result.transitions.append(allocator, .{ .kernel = initkernel, .items = .{}, .transitions = .{} });

        var s: StateId = 0;
        while (s < result.transitions.items.len) {
            var l = result.transitions.items.len;
            while (s < l) : (s += 1) {
                // NOTE: '&' here is very important so that the changes in this block are saved.
                // without it, we are just modifying a copy :^)
                var state = &result.transitions.items[s];

                // Find the closure of the state's kernel
                state.items = try build.closure(result, allocator, grammar, state.kernel);
                // defer state.items.deinit(allocator);

                // Find the transitions out of the state (a map from symbol to kernel)

                var transitions = try build.transitions(result, allocator, grammar, state.items);
                defer {
                    // for (transitions.values()) |*v| v.deinit(allocator);
                    transitions.deinit(allocator);
                }

                trace("init() transitions.count {}\n", .{transitions.count()});
                state.transitions.deinit(allocator);
                state.transitions = .{};
                // defer state.transitions.deinit(allocator);

                for (transitions.keys()) |symbol| {

                    // Given a symbol and kernel in the transition map, find out if we've
                    // already added the kernel as a state. If we have, assign that state's
                    // index to the transition table for the symbol. If not, create a
                    // new state.

                    var kernel = transitions.get(symbol).?;
                    var i: usize = 0;
                    while (i < result.transitions.items.len) : (i += 1) {
                        // for (result.transitions.items) |transition| {
                        if (build.eql(result.transitions.items[i].kernel, kernel)) {
                            try state.transitions.put(allocator, symbol, @intCast(StateId, i));
                            break;
                        }
                    }

                    if (i == result.transitions.items.len) {
                        try state.transitions.put(allocator, symbol, @intCast(StateId, result.transitions.items.len));
                        try result.transitions.append(allocator, .{ .kernel = kernel, .items = .{}, .transitions = .{} });
                    }
                }
            }
        }
        return result;
    }

    pub const Build = struct {
        initial: []const Item,
        closure: ClosureFn,
        transitions: TransitionsFn,
        eql: EqlFn,
    };

    pub const Error = error{} || mem.Allocator.Error;
    pub const GrammarFn = fn (Allocator, Grammar) Error!Automaton;
    pub const ClosureFn = fn (Automaton, Allocator, Grammar, ItemSet) Error!ItemSet;
    pub const TransitionsFn = fn (Automaton, Allocator, Grammar, ItemSet) Error!ItemSetSet;
    pub const EqlFn = fn (ItemSet, ItemSet) bool;

    pub fn lr0Closure(_: Automaton, allocator: Allocator, grammar: Grammar, kernel: ItemSet) Error!ItemSet {
        trace("lr0Closure kernel.count {}\n", .{kernel.count()});

        const start = grammar.start();

        // Which productions have been used?

        var used: std.AutoHashMapUnmanaged(SymbolId, void) = .{};
        defer used.deinit(allocator);

        // Copy the kernel as the initial list of items

        var result: ItemSet = .{};

        for (kernel.keys()) |it| {
            trace("{}\n", .{it});
            try result.put(allocator, it, {});
        }

        // While we cannot add more items...

        var added: ItemSet = .{};
        defer added.deinit(allocator);
        while (true) {
            added.clearRetainingCapacity();
            // For each item we have...

            for (result.keys()) |item| {
                // trace("item {}\n", .{item});
                // Find the nonterminal symbol...

                trace("item {}\n", .{item});
                const msymbolid = grammar.itemSymbolId(item, start);

                // Find unused matching productions and add them.

                // for (j = 0; j < grammar.productions.length; j++) {
                //   if (!used[j] && grammar.productions[j][0] == symbol) {
                //     added.push({ production: j, index: 0 });
                //     used[j] = true;
                if (msymbolid) |symbolid| {
                    for (grammar.productions.items) |prod, _j| {
                        const j = @intCast(SymbolId, _j);
                        if (!used.contains(j) and prod.name.id == symbolid) {
                            try added.put(allocator, .{ .id = j, .pos = 0 }, {});
                            try used.put(allocator, j, {});
                        }
                    }
                }
            }

            for (added.keys()) |add| try result.put(allocator, add, {});
            trace("added count {} result count {}\n", .{ added.count(), result.count() });
            if (added.count() == 0) break;
        }

        return result;
    }

    pub fn lr0Transitions(a: Automaton, allocator: Allocator, grammar: Grammar, closure: ItemSet) Error!ItemSetSet {
        trace("lr0Transitions closure.count() {}\n", .{closure.count()});
        _ = a;
        const start = grammar.start();
        var result: ItemSetSet = .{};

        // For each item...

        for (closure.keys()) |item| {
            // Calculate the leaving symbol by looking in the grammar's productions,
            // handling the augmented grammar production as above.

            // TODO: account for non-token syms and flags

            const msymbolid = grammar.itemSymbolId(item, start);
            trace("msymbolid {}\n", .{msymbolid});

            if (msymbolid) |symbolid| {
                const gop = try result.getOrPut(allocator, symbolid);
                if (!gop.found_existing) gop.value_ptr.* = .{};
                try gop.value_ptr.put(allocator, .{ .id = item.id, .pos = item.pos + 1 }, {});
            }
        }

        return result;
    }
    fn lr0Same(a: ItemSet, b: ItemSet) bool {
        return a.count() == b.count() and for (a.keys()) |it| {
            if (!b.contains(it)) break false;
        } else true;
    }
    pub const augmented_id = std.math.maxInt(SymbolId);
    pub const accept = augmented_id - 1;
    pub const end = augmented_id - 2;

    pub var lr0init = [1]Item{.{ .id = augmented_id, .pos = 0 }};
    pub const lr0build: Automaton.Build = .{
        .initial = &lr0init,
        .closure = lr0Closure,
        .transitions = lr0Transitions,
        .eql = lr0Same,
    };
};

pub const lr0_automaton = automaton(Automaton.lr0build);

pub fn automaton(comptime build: Automaton.Build) Automaton.GrammarFn {
    return struct {
        fn func(allocator: Allocator, g: Grammar) Automaton.Error!Automaton {
            return try Automaton.init(allocator, g, build);
        }
    }.func;
}

pub const Table = std.ArrayListUnmanaged(Actions);
pub const Actions = std.AutoArrayHashMapUnmanaged(SymbolId, Action);
pub const Action = struct {
    shift: ?SymbolId = null,
    reduce: SymbolIdList = .{},

    pub fn eql(a: Action, b: Action) bool {
        const shiftsnulleql = (a.shift == null) == (b.shift == null);
        const shiftseql = (shiftsnulleql and
            (a.shift == null or a.shift.? == b.shift.?));
        trace("shifts eql {} {} {}\n", .{ shiftseql, a.shift, b.shift });
        return shiftseql and
            a.reduce.items.len == b.reduce.items.len and
            blk: {
            for (a.reduce.items) |ait| {
                for (b.reduce.items) |bit| {
                    if (bit == ait) break;
                } else break :blk false;
            }
            break :blk true;
        };
    }
};
pub const TableFmt = struct {
    t: Table,
    g: Grammar,

    pub fn init(t: Table, g: Grammar) TableFmt {
        return .{
            .t = t,
            .g = g,
        };
    }

    pub fn format(self: TableFmt, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("table.len {}\n", .{self.t.items.len});
        for (self.t.items) |acts, i| {
            var it = acts.iterator();
            while (it.next()) |ent| {
                const id = ent.key_ptr.*;
                const act = ent.value_ptr.*;
                try writer.print("{}-{s}: ", .{ i, self.g.name(id) });
                if (act.shift) |s|
                    try writer.print(" .shift={} ", .{s});
                if (act.reduce.items.len > 0) {
                    try writer.print(" .reduce=", .{});
                    for (act.reduce.items) |r|
                        try writer.print("{}, ", .{r});
                }
                _ = try writer.write("\n");
            }
        }
    }
};
pub fn slr1Table(allocator: Allocator, grammar: Grammar) !Table {
    var table: Table = .{};

    var a = try lr0_automaton(allocator, grammar);
    defer a.deinit(allocator);
    trace("slr1Table a.transitions.len {}\n", .{a.transitions.items.len});
    var follows = try grammar.follow(allocator);
    defer follows.deinit(allocator);

    for (a.transitions.items) |state, i| {
        trace("  {}\n", .{i});
        var actions: Actions = .{};

        // for (s in state.transitions)
        //   actions[s] = { shift: state.transitions[s] };
        var it = state.transitions.iterator();
        while (it.next()) |ent| {
            const s = ent.key_ptr.*;
            const shift = ent.value_ptr.*;
            trace("  state.transition {},{}\n", .{ s, shift });
            try actions.putNoClobber(allocator, s, .{ .shift = shift });
        }

        for (state.items.keys()) |item| {
            trace("  state.item {}\n", .{item});

            // if (item.production === -1) {
            if (item.id == Automaton.augmented_id) {

                // if (item.index === 1)
                //   addReduceAction(actions, Grammar.END, item.production);
                trace("  augmented_id pos {}\n", .{item.pos});

                if (item.pos == 1) // NOTE: i have no idea why this is necessary
                    try addReduceAction(allocator, &actions, Grammar.END, item.id);
            } else {

                // if (item.index == grammar.productions[item.production].length - 1) {
                //     for (s in follow[grammar.productions[item.production][0]])
                //       addReduceAction(actions, s, item.production);
                // }

                const prod = grammar.productions.items[item.id];
                if (item.pos == prod.rule.items.len) {
                    if (follows.map.get(prod.name.id)) |ss| {
                        for (ss.keys()) |s| {
                            try addReduceAction(allocator, &actions, s, item.id);
                        }
                    }
                }
            }
        }

        try table.append(allocator, actions);
    }

    return table;
}

fn addReduceAction(allocator: Allocator, actions: *Actions, symbol: SymbolId, id: SymbolId) !void {

    // if (typeof actions[symbol] === "undefined")
    //   actions[symbol] = { reduce: [] };

    // if (typeof actions[symbol].reduce === "undefined")
    //   actions[symbol].reduce = [];

    // actions[symbol].reduce.push(production);

    const gop = try actions.getOrPut(allocator, symbol);
    if (!gop.found_existing) gop.value_ptr.* = .{};
    try actions.getPtr(symbol).?.reduce.append(allocator, id);
}

pub fn tableFree(allocator: Allocator, table: *Table) void {
    for (table.items) |*i| i.deinit(allocator);
    table.deinit(allocator);
}
pub fn parseSlr1Table(allocator: Allocator, fallr: Allocator, rows: []const u8, grammar: Grammar) !Table {
    _ = grammar;
    _ = fallr;
    _ = allocator;
    var table: Table = .{};
    var linesit = std.mem.split(u8, rows, "\n");
    // var row: u8 = 0;
    while (linesit.next()) |line| {
        var it = std.mem.split(u8, line, ",");
        while (it.next()) |_ent| {
            const ent = mem.trim(u8, _ent, &std.ascii.spaces);
            trace("ent '{s}'\n", .{ent});
            if (ent.len == 0) continue;
            const parser = comptime m.combine(.{
                p.somedigits,
                p.char('-'),
                m.oneOf(.{ p.ident, p.str_lit, p.char_lit, p.sqbkt_lit, m.asStr(p.char('$')) }),
                p.char('-'),
                m.rest,
            });
            var rowname: []const u8 = undefined;
            var colname: []const u8 = undefined;
            var val: []const u8 = undefined;
            if (ent.len == 0) continue;
            if (parser(fallr, ent)) |r| {
                rowname = r.value[0];
                colname = r.value[2];
                val = r.value[4];
            } else |_| {}
            trace("rowname '{s}' colname '{s}' val '{s}'\n", .{ rowname, colname, val });
            const row = try std.fmt.parseInt(StateId, rowname, 10);
            const colid = grammar.name_ids.get(colname).?;
            const isterm = !grammar.nonterminals.contains(colid);
            if (row >= table.items.len) {
                const nrows = row - table.items.len + 1;
                // trace("appending {} rows to table\n", .{nrows});
                try table.appendNTimes(allocator, .{}, nrows);
            }
            assert(row + 1 <= table.items.len);
            var action: Action = .{};
            if (isterm) {
                trace("term '{s}' val '{s}'\n", .{ ent, val });
                if (val[0] == 's')
                    action.shift = try std.fmt.parseInt(StateId, val[1..], 10)
                else if (val[0] == 'r') // r0
                    try action.reduce.append(allocator, try std.fmt.parseInt(StateId, val[1..], 10))
                else if (mem.eql(u8, val, "$accept"))
                    try action.reduce.append(allocator, Automaton.augmented_id)
                else
                    unreachable;
            } else {
                assert(val[0] == 's');
                action.shift = try std.fmt.parseInt(StateId, val[1..], 10);
            }
            assert(action.shift != null or action.reduce.items.len > 0);
            try table.items[row].put(allocator, colid, action);
        }
    }
    return table;
}
pub fn tablesEql(expected: Table, actual: Table) bool {
    for (expected.items) |exrow, i| {
        const missingidx = for (exrow.keys()) |exk, j| {
            if (i >= actual.items.len or !actual.items[i].contains(exk) or
                !containsval: {
                break :containsval for (actual.items[i].values()) |bv| {
                    if (bv.eql(exrow.values()[j])) break true;
                } else false;
            }) break j;
        } else null;
        if (missingidx) |idx|
            trace(
                "FAILURE: action missing in row {} \n  expected keys {} actual keys {} \n  expected values {} \n  actual values   {}\n",
                .{ i, exrow.keys()[idx], actual.items[i].keys()[idx], exrow.values()[idx], actual.items[i].values()[idx] },
            );
        if (missingidx != null) return false;
    }
    return true;
}
