const std = @import("std");
const bnf = @import("main.zig");
const Item = bnf.Item;
const ItemSet = bnf.ItemSet;
const ItemSetFmt = bnf.ItemSetFmt;

const t = std.testing;
// const allr = t.allocator;
// var gpa = std.heap.GeneralPurposeAllocator(.{ .stack_trace_frames = 10 }){};
// const allr = gpa.allocator();
// var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
// const allr = arena.allocator();
const allr = t.allocator;

test "zig grammar" {
    const src = @embedFile("../../grammarz/samples/zig.grammar");
    // const src = @embedFile("../samples/test.grammar");

    // comptime var prods: []const Production = &.{};
    var prods = try bnf.parseGrammar(allr, t.failing_allocator, src);
    defer bnf.parseFree(allr, prods);

    for (prods) |prod| {
        std.debug.print("'{s}' <- '{}'\n", .{ prod.name, prod.rule.root });
    }
}

test "TopLevelDecl" {
    const src =
        \\TopLevelDecl
        \\    <- (KEYWORD_export / KEYWORD_extern STRINGLITERALSINGLE? / (KEYWORD_inline / KEYWORD_noinline))? FnProto (SEMICOLON / Block)
        \\     / (KEYWORD_export / KEYWORD_extern STRINGLITERALSINGLE?)? KEYWORD_threadlocal? VarDecl
        \\     / KEYWORD_usingnamespace Expr SEMICOLON
    ;
    var prods = try bnf.parseGrammar(allr, t.failing_allocator, src);
    defer bnf.parseFree(allr, prods);

    try t.expectEqual(@as(usize, 1), prods.len);
    try t.expectEqual(@as(usize, 3), prods[0].rule.root.choice.len);
    try t.expect(prods[0].rule.root.choice[0] == .seq);
    try t.expectEqual(@as(usize, 3), prods[0].rule.root.choice[0].seq.len);
    try t.expect(prods[0].rule.root.choice[0].seq[0] == .optional);
    try t.expect(prods[0].rule.root.choice[0].seq[0].optional.* == .group);
    try t.expectEqual(@as(usize, 1), prods[0].rule.root.choice[0].seq[0].optional.group.len);
    try t.expect(prods[0].rule.root.choice[0].seq[0].optional.group[0] == .choice);
    try t.expectEqual(@as(usize, 3), prods[0].rule.root.choice[0].seq[0].optional.group[0].choice.len);
    try t.expect(prods[0].rule.root.choice[0].seq[0].optional.group[0].choice[0] == .name);
    try t.expect(prods[0].rule.root.choice[0].seq[0].optional.group[0].choice[1] == .seq);
    try t.expect(prods[0].rule.root.choice[0].seq[0].optional.group[0].choice[2] == .group);
    try t.expectEqual(@as(usize, 1), prods[0].rule.root.choice[0].seq[0].optional.group[0].choice[2].group.len);
    try t.expect(prods[0].rule.root.choice[0].seq[0].optional.group[0].choice[2].group[0] == .choice);
    try t.expectEqual(@as(usize, 2), prods[0].rule.root.choice[0].seq[0].optional.group[0].choice[2].group[0].choice.len);
    try t.expectEqual(@as(usize, 3), prods[0].rule.root.choice[1].seq.len);
    try t.expectEqual(@as(usize, 3), prods[0].rule.root.choice[2].seq.len);
}

test "keyword" {
    const src =
        \\keyword <- KEYWORD_align / KEYWORD_allowzero
        \\         / KEYWORD_anytype / KEYWORD_asm / KEYWORD_async / KEYWORD_await
    ;

    var prods = try bnf.parseGrammar(allr, t.failing_allocator, src);
    defer bnf.parseFree(allr, prods);

    // std.debug.print("{any}\n", .{prods[0].rule.root.choice});
    try t.expectEqual(@as(usize, 6), prods[0].rule.root.choice.len);

    try t.expect(prods[0].rule.root.choice[0] == .name);
    try t.expect(prods[0].rule.root.choice[1] == .name);
    try t.expect(prods[0].rule.root.choice[2] == .name);
    try t.expect(prods[0].rule.root.choice[3] == .name);
    try t.expect(prods[0].rule.root.choice[4] == .name);
    try t.expect(prods[0].rule.root.choice[5] == .name);
}

test "literals" {
    {
        const src =
            \\optchar0 <- '0'?
            \\nsqbkt0 <- ![0-9]
            \\str0 <- "asdf"
            \\str1 <- "asdf"'asdf'[asdf]
        ;
        var prods = try bnf.parseGrammar(allr, t.failing_allocator, src);
        defer bnf.parseFree(allr, prods);
        // for (prods) |p|
        //     std.debug.print("{s} <- {}\n", .{ p.name, p.rule.root });
        try t.expectEqual(@as(usize, 4), prods.len);

        try t.expect(prods[0].rule.root == .choice);
        try t.expect(prods[0].rule.root.choice.len == 1);
        try t.expect(prods[0].rule.root.choice[0] == .optional);
        try t.expect(prods[0].rule.root.choice[0].optional.* == .char_lit);
        try t.expect(prods[0].rule.root.choice[0].optional.char_lit.len == 1);

        try t.expect(prods[1].rule.root == .choice);
        try t.expect(prods[1].rule.root.choice.len == 1);
        try t.expect(prods[1].rule.root.choice[0] == .not);
        try t.expect(prods[1].rule.root.choice[0].not.* == .sqbkt_lit);
        try t.expect(prods[1].rule.root.choice[0].not.sqbkt_lit.len == 3);

        try t.expect(prods[2].rule.root == .choice);
        try t.expect(prods[2].rule.root.choice.len == 1);
        try t.expect(prods[2].rule.root.choice[0] == .str_lit);
        try t.expect(prods[2].rule.root.choice[0].str_lit.len == 4);

        try t.expect(prods[3].rule.root == .choice);
        try t.expect(prods[3].rule.root.choice.len == 1);
        try t.expect(prods[3].rule.root.choice[0] == .seq);
        try t.expect(prods[3].rule.root.choice[0].seq.len == 3);
        try t.expect(prods[3].rule.root.choice[0].seq[0] == .str_lit);
        try t.expect(prods[3].rule.root.choice[0].seq[1] == .char_lit);
        try t.expect(prods[3].rule.root.choice[0].seq[2] == .sqbkt_lit);
    }
}

test "literal escapes" {
    {
        const src =
            \\asdf <- ('//!' '\n' '\\')+
        ;
        var prods = try bnf.parseGrammar(allr, t.failing_allocator, src);
        defer bnf.parseFree(allr, prods);
        for (prods) |p|
            std.debug.print("{s} <- {}\n", .{ p.name, p.rule.root });
        try t.expect(prods.len == 1);
        try t.expect(prods[0].rule.root == .choice);
        try t.expect(prods[0].rule.root.choice.len == 1);
        try t.expect(prods[0].rule.root.choice[0] == .some);
        try t.expect(prods[0].rule.root.choice[0].some.* == .group);
        try t.expect(prods[0].rule.root.choice[0].some.group.len == 3);
        try t.expect(prods[0].rule.root.choice[0].some.group[1] == .char_lit);
        // TODO: this should be len == 1 w/ value == 10, not .{'\\', 'n'}
        try t.expect(prods[0].rule.root.choice[0].some.group[1].char_lit.len == 2);
    }
}

test "calc" {
    if (false) {
        const src = @embedFile("../samples/calc.bnf");
        var prods = try bnf.parseGrammar(allr, t.failing_allocator, src);
        defer bnf.parseFree(allr, prods);
        for (prods) |p|
            std.debug.print("{s} <- {}\n", .{ p.name, p.rule.root });

        const input =
            \\ 1 + 2 * 3 ^ 2
        ;
        var ginfo = try bnf.GrammarInfo.init(t.allocator, prods);
        defer ginfo.deinit(t.allocator);
        try bnf.parseLR(t.allocator, t.failing_allocator, ginfo, input);
    }
}

test "closure" {
    const src = @embedFile("../samples/xz.bnf");
    var prods = try bnf.parseGrammar(allr, t.failing_allocator, src);
    var ginfo = try bnf.GrammarInfo.init(allr, prods);

    var augmentedstart = try bnf.parseGrammar(allr, t.failing_allocator, "S' <- S");
    var auginfo = try bnf.GrammarInfo.init(allr, augmentedstart);
    try ginfo.addProductions(allr, augmentedstart);
    for (ginfo.grammar.items) |it|
        std.debug.print("{s} <- {}\n", .{ it.name, it.rule.root });

    var set0 = try auginfo.itemSetUsing(allr, ginfo, 0);
    defer {
        bnf.parseFree(allr, prods);
        ginfo.deinit(allr);
        bnf.parseFree(allr, augmentedstart);
        auginfo.deinit(allr);
        set0.deinit(allr);
    }
    try t.expectEqual(@as(usize, 1), set0.items.len);

    try t.expectEqualStrings("S'", ginfo.idToName(bnf.TypedSymbolId.init(set0.items[0].id, .nonterm)).?);

    // std.debug.print("set0 {}\n", .{ItemSetFmt.init(set0, ginfo)});
    var c0 = try bnf.closure(allr, set0, ginfo);
    defer c0.deinit(allr);

    std.debug.print("c0-2 {}\n", .{ItemSetFmt.init(c0, ginfo)});
    try t.expectEqual(@as(usize, 3), c0.items.len);
    try t.expectEqual(Item.init(ginfo.nameToTid("S'").?.id, 0), c0.items[0]);
    try t.expectEqual(Item.init(ginfo.nameToTid("S").?.id, 0), c0.items[1]);
    try t.expectEqual(Item.init(ginfo.nameToTid("E").?.id, 0), c0.items[2]);
}

test "goto1" {
    const src = @embedFile("../samples/factor.bnf");
    var augmentedstart = try bnf.parseGrammar(allr, t.failing_allocator, "E' <- E");
    var auginfo = try bnf.GrammarInfo.init(allr, augmentedstart);
    var prods = try bnf.parseGrammar(allr, t.failing_allocator, src);
    var ginfo = try bnf.GrammarInfo.init(allr, prods);
    try ginfo.addProductions(allr, augmentedstart);

    var set0 = try ginfo.itemSetFromNames(allr, &.{ "E'", "E" }, 0);
    // std.debug.print("set0 {}\n", .{ItemSetFmt.init(set0, ginfo)});
    try t.expectEqual(@as(usize, 2), set0.items.len);
    defer {
        ginfo.deinit(allr);
        bnf.parseFree(allr, prods);
        bnf.parseFree(allr, augmentedstart);
        auginfo.deinit(allr);
        set0.deinit(allr);
    }

    var g = try bnf.goto(allr, set0, ginfo.nameToTid("+").?, ginfo);
    defer g.deinit(allr);

    // std.debug.print("g {}\n", .{ItemSetFmt.init(g, ginfo)});

    try t.expectEqual(@as(usize, 3), g.items.len);
    try t.expectEqual(Item.init(ginfo.nameToTid("E").?.id, 2), g.items[0]);
    try t.expectEqual(Item.init(ginfo.nameToTid("T").?.id, 0), g.items[1]);
    try t.expectEqual(Item.init(ginfo.nameToTid("F").?.id, 0), g.items[2]);
}

test "goto2" {
    const src = @embedFile("../samples/xz.bnf");
    var augmentedstart = try bnf.parseGrammar(allr, t.failing_allocator, "S' <- S");
    var auginfo = try bnf.GrammarInfo.init(allr, augmentedstart);
    var prods = try bnf.parseGrammar(allr, t.failing_allocator, src);
    var ginfo = try bnf.GrammarInfo.init(allr, prods);
    try ginfo.addProductions(allr, augmentedstart);

    var set0 = try ginfo.itemSetFromNames(allr, &.{"S"}, 0);

    defer {
        ginfo.deinit(allr);
        bnf.parseFree(allr, prods);
        bnf.parseFree(allr, augmentedstart);
        auginfo.deinit(allr);
        set0.deinit(allr);
    }

    var g = try bnf.goto(allr, set0, ginfo.nameToTid("E").?, ginfo);
    defer g.deinit(allr);

    std.debug.print("g {}\n", .{ItemSetFmt.init(g, ginfo)});

    try t.expectEqual(@as(usize, 3), g.items.len);
    try t.expectEqual(Item.init(ginfo.nameToTid("S").?.id, 1), g.items[0]);
    try t.expectEqual(Item.init(ginfo.nameToTid("E").?.id, 1), g.items[1]);
    try t.expectEqual(Item.init(ginfo.nameToTid("E").?.id, 3), g.items[2]);
}

test "lr0items" {
    const src = @embedFile("../samples/xz.bnf");
    var augmentedstart = try bnf.parseGrammar(allr, t.failing_allocator, "S' <- S");
    var prods = try bnf.parseGrammar(allr, t.failing_allocator, src);
    var ginfo = try bnf.GrammarInfo.init(allr, prods);
    try ginfo.addProductions(allr, augmentedstart);
    defer {
        bnf.parseFree(allr, augmentedstart);
        bnf.parseFree(allr, prods);
        ginfo.deinit(allr);
    }

    // for (ginfo.grammar.items) |prod| {
    //     std.debug.print("{s} <- {}\n", .{ prod.name, prod.rule.root });
    // }

    var set0 = try ginfo.itemSetFromNames(allr, &.{"S'"}, 0);

    var itemsets = try bnf.lr0items(allr, set0, ginfo);
    defer {
        set0.deinit(allr);
        for (itemsets.items) |*itemset| itemset.deinit(allr);
        itemsets.deinit(allr);
    }

    try t.expectEqual(@as(usize, 5), itemsets.items.len);
    const Spid = ginfo.nameToProdId("S'").?;
    const Sid = ginfo.nameToProdId("S").?;
    const Eid = ginfo.nameToProdId("E").?;
    try t.expectEqualSlices(
        Item,
        &.{ Item.init(Spid, 0), Item.init(Sid, 0), Item.init(Eid, 0) },
        itemsets.items[0].items,
    );
    try t.expectEqualSlices(
        Item,
        &.{Item.init(Spid, 1)},
        itemsets.items[1].items,
    );
    try t.expectEqualSlices(
        Item,
        &.{ Item.init(Sid, 1), Item.init(Eid, 1), Item.init(Eid, 3) },
        itemsets.items[2].items,
    );
    try t.expectEqualSlices(
        Item,
        &.{ Item.init(Eid, 2), Item.init(Eid, 0) },
        itemsets.items[3].items,
    );
    try t.expectEqualSlices(
        Item,
        &.{Item.init(Eid, 1)},
        itemsets.items[4].items,
    );
}

test "createTables" {
    const src = @embedFile("../samples/factor.bnf");
    var prods = try bnf.parseGrammar(allr, t.failing_allocator, src);

    var ginfo = try bnf.GrammarInfo.init(allr, prods);

    defer {
        bnf.parseFree(allr, prods);
        ginfo.deinit(allr);
    }
    var tables = try bnf.createTables(allr, ginfo, "E' <- E");
    var writer = std.io.getStdErr().writer();

    {
        std.debug.print("symbols\n", .{});
        for (ginfo.name_sym.keys()) |name| {
            std.debug.print("{s}-{}\n", .{ name, bnf.TidFmt.init(ginfo.nameToTid(name).?, ginfo) });
        }
    }
    {
        std.debug.print("actions\n", .{});
        var it = tables.action.iterator();
        while (it.next()) |ent| {
            const rc = ent.key_ptr.*;
            std.debug.print("({},{})-{}\n", .{ bnf.TypedColRow.Fmt.init(rc, tables.ginfo), rc.row, ent.value_ptr.* });
        }
    }
    {
        std.debug.print("gotos\n", .{});
        var it = tables.goto.iterator();
        while (it.next()) |ent| {
            const rc = ent.key_ptr.*;
            std.debug.print("({},{})-{}\n", .{ bnf.TypedColRow.Fmt.init(rc, tables.ginfo), rc.row, ent.value_ptr.* });
        }
    }
    try tables.display(writer, 7);

    defer {
        tables.deinit(allr);
    }
}

test "duplicate production names" {
    const src = @embedFile("../samples/factor2.bnf");
    var prods = try bnf.parseGrammar(allr, t.failing_allocator, src);
    var ginfo = try bnf.GrammarInfo.init(allr, prods);
    defer {
        bnf.parseFree(allr, prods);
        ginfo.deinit(allr);
    }

    try t.expectEqual(@as(usize, 6), prods.len);
    const Etid = ginfo.nameToTid("E").?;
    const E0syms = ginfo.item_syms.get(.{ .id = Etid.id, .pos = 0 });
    try t.expect(E0syms != null);
    try t.expectEqual(@as(usize, 2), E0syms.?.items.len);
    try t.expectEqual(Etid, E0syms.?.items[0]);
    try t.expectEqual(ginfo.nameToTid("T").?, E0syms.?.items[1]);
}

// test "json" {
//     const src = @embedFile("../samples/json.bnf");
//     var prods = try bnf.parseGrammar(allr, t.failing_allocator, src);
//     defer bnf.parseFree(allr, prods);
//     // var ginfo = try bnf.GrammarInfo.init(allr, prods);
//     for (prods) |prod| {
//         std.debug.print("{s} <- {}\n", .{ prod.name, prod.rule.root });
//     }
// }
