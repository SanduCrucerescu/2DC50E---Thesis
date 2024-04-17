using System.Collections.Generic;
using System.Diagnostics;
using Antlr4.Runtime.Tree;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override Statement VisitGotoStatement(DelphiParser.GotoStatementContext cx)
    {
        if (cx.label() is { } label)
        {
            return new GotoStatement
            {
                Label = VisitLabel(label),
                Source = Source.Of(cx),
            };
        }
        
        if (cx.EXIT() is not null)
        {
            return new ExitStatement
            {
                Expr = VisitExpression(cx.expression()),
                Source = Source.Of(cx),
            };
        }

        if (cx.BREAK() is not null)
        {
            return new BreakStatement
            {
                Source = Source.Of(cx),
            };
        }
        
        Debug.Assert(cx.CONTINUE() is not null, "Must Exist");
        return new ContinueStatement
        {
            Source = Source.Of(cx),
        };
    }

    public override Ident VisitLabel(DelphiParser.LabelContext cx)
    {
        Debug.Assert(cx.TkIntNum() is null && cx.TkHexNum() is null, "Integers and Hex literals are not allowed as label names");
        Debug.Assert(cx.TkIdentifier() is not null || cx.usedKeywordsAsNames() is not null);

        if (cx.usedKeywordsAsNames() is { } kw)
        {
            return VisitUsedKeywordsAsNames(kw);
        }
        
        return new SimpleIdent {
            Text = cx.TkIdentifier().GetText(),
            Source = Source.Of(cx.TkIdentifier().Symbol),
        };
        
    }

    public override Ident VisitUsedKeywordsAsNames(DelphiParser.UsedKeywordsAsNamesContext cx)
    {
        Debug.Assert(cx.children.Count == 1);
        var child = cx.GetChild<ITerminalNode>(0);
        return new SimpleIdent
        {
            Text = child.GetText(),
            Source = Source.Of(child.Symbol),
        };
    }
}

public sealed class ContinueStatement : Statement
{
    public override IEnumerable<DelphiNode> Children => [];
    protected override bool IsLeaf => true;
}
public sealed class BreakStatement : Statement
{
    public override IEnumerable<DelphiNode> Children => [];
    protected override bool IsLeaf => true;
}
public sealed class GotoStatement : Statement
{
    public required Ident Label { get; init; }

    public override IEnumerable<DelphiNode> Children => [Label];
}

public sealed class ExitStatement : Statement
{
    public required Expr? Expr { get; init; }

    public override IEnumerable<DelphiNode> Children => Expr is not null ? [Expr] : [];
}

