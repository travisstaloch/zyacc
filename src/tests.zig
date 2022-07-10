const std = @import("std");
const g = @import("main.zig");
const assert = std.debug.assert;
const t = std.testing;
// const allr = t.allocator;
const fallr = t.failing_allocator;
var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
const allr = arena.allocator();
// const allr = std.heap.c_allocator;

test "tokenize" {
    // const src = @embedFile("../samples/zig.grammar");
    const src =
        \\Root <- skip container_doc_comment? ContainerMembers eof
        \\
        \\# *** Top level ***
        \\ContainerMembers <- ContainerDeclarations (ContainerField COMMA)* (ContainerField / ContainerDeclarations)
        \\
        \\keyword <- KEYWORD_align / KEYWORD_allowzero / KEYWORD_and / KEYWORD_anyframe
    ;
    var grammar = try g.Grammar.init(allr, fallr, src);
    defer grammar.deinit(allr);
    // for (grammar.productions.items) |p, i| {
    //     std.debug.print("{}: {s}-{}\n", .{ i, grammar.name(p.name.id), p.name.id });
    // }

    const productions = grammar.productions;
    try t.expectEqual(@as(usize, 6), productions.items.len);
    {
        // skip container_doc_comment? ContainerMembers eof
        const prod = productions.items[0];
        try t.expectEqualStrings("Root", grammar.name(prod.name.id).?);
        try t.expectEqual(@as(usize, 5), prod.rule.len);
        try t.expectEqualStrings("skip", grammar.name(prod.rule[0].sym.token.id).?);
        try t.expectEqualStrings("eof", grammar.name(prod.rule[3].sym.token.id).?);
        const expected_tags: []const g.Token.Tag = &.{ .name, .name, .name, .name, .comment };
        try t.expectEqual(expected_tags.len, prod.rule.len);
        for (prod.rule) |tok, i| {
            if (i < expected_tags.len)
                try t.expectEqual(expected_tags[i], tok.sym.token.tag);
        }
        try t.expect(prod.rule[1].flags.contains(.optional));
    }
    {
        // ContainerDeclarations (ContainerField COMMA)* (ContainerField / ContainerDeclarations)
        const prod = productions.items[1];
        try t.expectEqual(@as(usize, 3), prod.rule.len);
        try t.expectEqual(g.Symbol.Sym.Tag.token, prod.rule[0].sym);
        try t.expectEqual(g.Token.Tag.name, prod.rule[0].sym.token.tag);

        try t.expectEqual(g.Symbol.Sym.Tag.group, prod.rule[1].sym);
        try t.expectEqual(@as(usize, 2), prod.rule[1].sym.group.items.len);
        try t.expect(prod.rule[1].flags.contains(.many));

        try t.expectEqual(g.Symbol.Sym.Tag.group, prod.rule[2].sym);
        // std.debug.print("{}\n", .{prod.rule[2].sym.group.items[0].sym});
        try t.expectEqual(@as(usize, 1), prod.rule[2].sym.group.items.len);
    }
    {
        // KEYWORD_align / KEYWORD_allowzero / KEYWORD_and / KEYWORD_anyframe
        var i: usize = 2;
        while (i < 6) : (i += 1) {
            const prod = productions.items[i];
            try t.expectEqual(@as(usize, 1), prod.rule.len);
        }
    }
}

test "tokenize zig" {
    const src = @embedFile("../samples/zig.bnf");
    var grammar = try g.Grammar.init(allr, fallr, src);
    defer grammar.deinit(allr);

    // for (grammar.productions.items) |prod| std.debug.print("{}\n", .{g.Production.Fmt.init(prod, grammar)});
}

test "tokenize nested groups 1" {
    const src =
        \\X <- (B / C)
    ;
    var grammar = try g.Grammar.init(allr, fallr, src);
    defer grammar.deinit(allr);

    const productions = grammar.productions;
    try t.expectEqual(@as(usize, 1), productions.items.len);
    try t.expectEqual(@as(usize, 1), productions.items[0].rule.len);
    try t.expectEqual(g.Symbol.Sym.Tag.group, productions.items[0].rule[0].sym);
    try t.expectEqual(@as(usize, 1), productions.items[0].rule[0].sym.group.items.len);
    try t.expectEqual(g.Symbol.Sym.Tag.choice, productions.items[0].rule[0].sym.group.items[0].sym);
    try t.expectEqual(@as(usize, 2), productions.items[0].rule[0].sym.group.items[0].sym.choice.items.len);
}

test "tokenize nested groups 2" {
    const src =
        \\X <- (A / (B / C))? D (E / F)
    ;
    var grammar = try g.Grammar.init(allr, fallr, src);
    defer grammar.deinit(allr);

    const productions = grammar.productions;
    try t.expectEqual(@as(usize, 1), productions.items.len);
    try t.expectEqual(@as(usize, 3), productions.items[0].rule.len);
    try t.expectEqual(g.Symbol.Sym.Tag.group, productions.items[0].rule[0].sym);
    try t.expectEqual(g.Symbol.Sym.Tag.token, productions.items[0].rule[1].sym);
    try t.expectEqual(g.Symbol.Sym.Tag.group, productions.items[0].rule[2].sym);
    try t.expectEqual(@as(usize, 1), productions.items[0].rule[0].sym.group.items.len);
    try t.expectEqual(g.Symbol.Sym.Tag.choice, productions.items[0].rule[0].sym.group.items[0].sym);
    try t.expectEqual(@as(usize, 2), productions.items[0].rule[0].sym.group.items[0].sym.choice.items.len);
    try t.expectEqual(@as(usize, 1), productions.items[0].rule[0].sym.group.items[0].sym.choice.items[0].items.len);
    try t.expectEqual(@as(usize, 1), productions.items[0].rule[0].sym.group.items[0].sym.choice.items[1].items.len);
    try t.expectEqual(g.Symbol.Sym.Tag.group, productions.items[0].rule[0].sym.group.items[0].sym.choice.items[1].items[0].sym);
}

fn expectItemSymId(expected_id: ?g.SymbolId, id: g.SymbolId, pos: g.SymbolId, start: g.Production, grammar: g.Grammar) !void {
    const msym = grammar.itemSymbolId(g.Item.init(id, pos), start);
    try t.expectEqual(expected_id, msym);
}
test "itemSymbolId" {
    const src = @embedFile("../samples/xz.bnf");
    var grammar = try g.Grammar.init(allr, fallr, src);
    defer grammar.deinit(allr);
    const start = grammar.start();
    const Sid = grammar.name_ids.get("S").?;
    const Eid = grammar.name_ids.get("E").?;
    const zid = grammar.name_ids.get("z").?;
    const xid = grammar.name_ids.get("x").?;
    try expectItemSymId(Sid, g.Automaton.augmented_id, 0, start, grammar);
    try expectItemSymId(null, g.Automaton.augmented_id, 1, start, grammar);
    try expectItemSymId(Eid, 0, 0, start, grammar);
    try expectItemSymId(null, 0, 1, start, grammar);
    try expectItemSymId(Eid, 1, 0, start, grammar);
    try expectItemSymId(xid, 1, 1, start, grammar);
    try expectItemSymId(Eid, 1, 2, start, grammar);
    try expectItemSymId(null, 1, 3, start, grammar);
    try expectItemSymId(zid, 2, 0, start, grammar);
    try expectItemSymId(null, 2, 1, start, grammar);
}

test "itemSymbolId2" {
    const src = @embedFile("../samples/factor.bnf");
    var grammar = try g.Grammar.init(allr, fallr, src);
    defer grammar.deinit(allr);
    const start = grammar.start();
    // this table was created from item_pos_sym in grammophone/application.js
    // console.log(JSON.stringify(item_pos_sym))
    // the rows are input/output {Item, ?SymbolId}

    const input_output_pairs = [_]std.meta.Tuple(&.{ g.Item, ?g.SymbolId }){
        .{ g.Item.init(g.Automaton.augmented_id, 0), grammar.name_ids.get("E").? },
        .{ g.Item.init(0, 0), grammar.name_ids.get("E").? },
        .{ g.Item.init(1, 0), grammar.name_ids.get("T").? },
        .{ g.Item.init(2, 0), grammar.name_ids.get("T").? },
        .{ g.Item.init(3, 0), grammar.name_ids.get("F").? },
        .{ g.Item.init(4, 0), grammar.name_ids.get("(").? },
        .{ g.Item.init(5, 0), grammar.name_ids.get("id").? },
        .{ g.Item.init(g.Automaton.augmented_id, 1), null },
        .{ g.Item.init(0, 1), grammar.name_ids.get("+").? },
        .{ g.Item.init(1, 1), null },
        .{ g.Item.init(2, 1), grammar.name_ids.get("*").? },
        .{ g.Item.init(3, 1), null },
        .{ g.Item.init(4, 1), grammar.name_ids.get("E").? },
        .{ g.Item.init(5, 1), null },
        .{ g.Item.init(0, 2), grammar.name_ids.get("T").? },
        .{ g.Item.init(2, 2), grammar.name_ids.get("F").? },
        .{ g.Item.init(4, 2), grammar.name_ids.get(")").? },
        .{ g.Item.init(0, 3), null },
        .{ g.Item.init(2, 3), null },
        .{ g.Item.init(4, 3), null },
    };
    for (input_output_pairs) |ex| {
        const item = ex[0];
        const expected_sym = ex[1];
        try expectItemSymId(expected_sym, item.id, item.pos, start, grammar);
    }
}

test "lr0 items" {
    const src = @embedFile("../samples/xz.bnf");
    var grammar = try g.Grammar.init(allr, fallr, src);
    defer grammar.deinit(allr);

    try t.expectEqual(@as(usize, 3), grammar.productions.items.len);

    var a = try g.lr0_automaton(allr, grammar);
    defer a.deinit(allr);

    const states = a.states.items;
    try t.expectEqual(@as(usize, 6), states.len);
    try t.expectEqualStrings("S'", grammar.name(grammar.augprod.name.id).?);
    const Spid = grammar.name_ids.get("S'").?;
    const Sid = grammar.name_ids.get("S").?;
    const Eid = grammar.name_ids.get("E").?;

    // 0 :: S':.S
    try t.expectEqual(@as(usize, 1), states[0].kernel.count());
    try t.expectEqual(g.Item.init(Spid, 0), states[0].kernel.keys()[0]);
    // 1 :: S':S.
    try t.expectEqual(@as(usize, 1), states[1].kernel.count());
    try t.expectEqual(g.Item.init(Spid, 1), states[1].kernel.keys()[0]);
    // 2 :: S:E. E:E.xE
    try t.expectEqual(@as(usize, 2), states[2].kernel.count());
    try t.expectEqual(g.Item.init(Sid, 1), states[2].kernel.keys()[0]);
    try t.expectEqual(g.Item.init(Eid, 1), states[2].kernel.keys()[1]);
    // 3 :: E:z.
    try t.expectEqual(@as(usize, 1), states[3].kernel.count());
    try t.expectEqual(g.Item.init(2, 1), states[3].kernel.keys()[0]);
    // 4 :: E:Ex.E
    try t.expectEqual(@as(usize, 1), states[4].kernel.count());
    try t.expectEqual(g.Item.init(Eid, 2), states[4].kernel.keys()[0]);
    // 5 :: E:ExE. E:E.xE
    try t.expectEqual(@as(usize, 2), states[5].kernel.count());
    try t.expectEqual(g.Item.init(Eid, 3), states[5].kernel.keys()[0]);
    try t.expectEqual(g.Item.init(Eid, 1), states[5].kernel.keys()[1]);
}

test "firsts" {
    const src = @embedFile("../samples/xz.bnf");
    var grammar = try g.Grammar.init(allr, fallr, src);
    defer grammar.deinit(allr);
    var firsts = try grammar.first(allr);
    defer firsts.map.deinit(allr);

    const Sid = grammar.name_ids.get("S").?;
    const Eid = grammar.name_ids.get("E").?;
    const zid = grammar.name_ids.get("z").?;

    try t.expectEqual(@as(usize, 2), firsts.map.count());
    try t.expect(firsts.map.contains(Sid));
    const firstS = firsts.map.get(Sid) orelse unreachable;
    // std.debug.print("{any}\n", .{firstS.keys()});
    try t.expectEqual(@as(usize, 1), firstS.count());
    try t.expect(firstS.contains(zid));

    try t.expect(firsts.map.contains(Eid));
    const firstE = firsts.map.get(Eid) orelse unreachable;
    try t.expectEqual(@as(usize, 1), firstE.count());
    try t.expect(firstE.contains(zid));
}

test "follows" {
    const src = @embedFile("../samples/xz.bnf");
    var grammar = try g.Grammar.init(allr, fallr, src);
    defer grammar.deinit(allr);
    var follows = try grammar.follow(allr);
    defer follows.map.deinit(allr);

    const Sid = grammar.name_ids.get("S").?;
    const Eid = grammar.name_ids.get("E").?;
    const xid = grammar.name_ids.get("x").?;
    // std.debug.print("follows {any}\n", .{follows.map.keys()});
    try t.expectEqual(@as(usize, 2), follows.map.count());
    {
        try t.expect(follows.map.contains(Sid));
        const followS = follows.map.get(Sid) orelse unreachable;
        try t.expectEqual(@as(usize, 1), followS.count());
        try t.expect(followS.contains(g.Grammar.END));
    }
    {
        try t.expect(follows.map.contains(Eid));
        const followE = follows.map.get(Eid) orelse unreachable;
        // for (followE.keys()) |k| std.debug.print("{s}\n", .{grammar.name(k)});
        try t.expectEqual(@as(usize, 2), followE.count());
        try t.expect(followE.contains(xid));
        try t.expect(followE.contains(g.Grammar.END));
    }
}

test "display productions" {
    const src = @embedFile("../samples/xz.bnf");
    var grammar = try g.Grammar.init(allr, fallr, src);
    defer grammar.deinit(allr);
    var l = std.ArrayList(u8).init(allr);
    defer l.deinit();
    const writer = l.writer();
    // for (grammar.productions.items) |p|
    try writer.print("{}", .{g.Production.Fmt.init(grammar.productions.items[0], grammar)});
    try t.expectEqualStrings("S <- E", l.items);
    l.items.len = 0;
    try writer.print("{}", .{g.Production.Fmt.init(grammar.productions.items[1], grammar)});
    try t.expectEqualStrings("E <- E x E", l.items);
    l.items.len = 0;
    try writer.print("{}", .{g.Production.Fmt.init(grammar.productions.items[2], grammar)});
    try t.expectEqualStrings("E <- z", l.items);
    l.items.len = 0;
}

test "slr1Table" {
    const src = @embedFile("../samples/xz.bnf");
    var grammar = try g.Grammar.init(allr, fallr, src);
    defer grammar.deinit(allr);
    var table = try g.slr1Table(allr, grammar);
    defer g.tableFree(allr, &table);
    const tablesrc =
        \\0-S-s1, 0-E-s2, 0-z-s3, 
        \\1-$-$accept, 
        \\2-x-s4, 2-$-r0, 
        \\3-x-r2, 3-$-r2, 
        \\4-E-s5, 4-z-s3, 
        \\5-x-s4, 5-x-r1, 5-$-r1, 
    ;
    var extable = try g.parseSlr1Table(allr, t.failing_allocator, tablesrc, grammar);
    defer g.tableFree(allr, &extable);
    try t.expectEqual(@as(usize, 6), extable.items.len);
    try t.expect(g.tablesEql(extable, table));
}

test "slr1Table factor" {
    const src = @embedFile("../samples/factor.bnf");
    var grammar = try g.Grammar.init(allr, fallr, src);
    defer grammar.deinit(allr);
    try t.expectEqual(@as(usize, 6), grammar.productions.items.len);
    try t.expectEqual(@as(usize, 3), grammar.nonterminals.count());
    try t.expectEqual(@as(usize, 11), grammar.name_ids.count());
    try t.expectEqual(@as(usize, 11), grammar.id_names.count());
    // var a = try g.lr0_automaton(allr, grammar);
    // const expectedlens = [_][2]usize{ .{ 5, 7 }, .{ 1, 2 }, .{ 0, 1 }, .{ 5, 7 }, .{ 0, 1 }, .{ 4, 5 }, .{ 3, 3 }, .{ 2, 2 }, .{ 1, 2 }, .{ 0, 1 } };
    // for (expectedlens) |lens, i| {
    //     const tslenex = lens[0];
    //     const islenex = lens[1];
    //     const tslenact = a.states.items[i].transitions.count();
    //     const islenact = a.states.items[i].itemset.count();
    //     if (tslenex != tslenact)
    //         std.debug.print("WARNING: states[{}].transitions.count() {} != {} \n", .{ i, tslenact, tslenex });
    //     // try t.expectEqual(tslenex, tslenact);

    //     if (islenex != islenact)
    //         std.debug.print("WARNING: states[{}].itemset.count() {} != {} \n", .{ i, islenact, islenex });
    //     // try t.expectEqual(islenex, islenact);
    // }
    var table = try g.slr1Table(allr, grammar);
    defer g.tableFree(allr, &table);
    // std.debug.print("table {}\n", .{g.TableFmt.init(table, grammar)});
    const extablesrc =
        \\0-E-s1, 0-T-s2, 0-F-s3, 0-'('-s4, 0-"id"-s5, 
        \\1-'+'-s6, 1-$-$accept, 
        \\2-'*'-s7, 2-$-r1, 2-'+'-r1, 2-')'-r1, 
        \\3-'*'-r3, 3-$-r3, 3-'+'-r3, 3-')'-r3, 
        \\4-E-s8, 4-T-s2, 4-F-s3, 4-'('-s4, 4-"id"-s5, 
        \\5-'*'-r5, 5-$-r5, 5-'+'-r5, 5-')'-r5, 
        \\6-T-s9, 6-F-s3, 6-'('-s4, 6-"id"-s5, 
        \\7-F-s10, 7-'('-s4, 7-"id"-s5, 
        \\8-')'-s11, 8-'+'-s6, 
        \\9-'*'-s7, 9-$-r0, 9-'+'-r0, 9-')'-r0, 
        \\10-'*'-r2, 10-$-r2, 10-'+'-r2, 10-')'-r2, 
        \\11-'*'-r4, 11-$-r4, 11-'+'-r4, 11-')'-r4, 
    ;
    var extable = try g.parseSlr1Table(allr, t.failing_allocator, extablesrc, grammar);
    defer g.tableFree(allr, &extable);
    try t.expectEqual(@as(usize, 12), extable.items.len);

    try t.expect(g.tablesEql(extable, table));
}

test "slr1Table parser" {
    const src = @embedFile("../samples/factor.bnf");
    var grammar = try g.Grammar.init(allr, fallr, src);
    defer grammar.deinit(allr);
    var table = try g.slr1Table(allr, grammar);
    defer g.tableFree(allr, &table);
    // std.debug.print("table {}\n", .{g.TableFmt.init(table, grammar)});
    const inputs =
        \\id * id + id
    ;
    var inputit = std.mem.split(u8, inputs, "\n");
    var stack = std.ArrayListUnmanaged(g.StateId){};
    while (inputit.next()) |line| {
        stack.items.len = 0;
        try stack.append(allr, 0);
        var tokit = std.mem.split(u8, line, " ");
        var a = tokit.next();
        while (true) {
            const symid = grammar.name_ids.get(a orelse return error.NoInput) orelse return error.InvalidSymbol;
            const s = g.peekTop(g.StateId, stack) orelse return error.EmptyStack;
            std.debug.print("symid {} s {}\n", .{ symid, s });
            if (g.peekAt(g.Actions, table, s)) |actions| {
                if (actions.get(symid)) |action| {
                    if (action.shift) |shift| {
                        std.debug.print("shift {}\n", .{shift});
                        try stack.append(allr, shift);
                        a = tokit.next();
                    } else if (action.reduce.items.len > 0) {
                        std.debug.print("{any}\n", .{action.reduce.items});
                        return error.NotImplemented;
                    }
                } else return error.ErrorRecovery;
            } else return error.ErrorRecovery;
        }
    }
}
