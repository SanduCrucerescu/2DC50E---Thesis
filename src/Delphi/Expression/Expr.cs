using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override Expr VisitExpression(DelphiParser.ExpressionContext cx)
    {
        Debug.Assert(cx.simpleExpression().Length > 0 && cx.children.Count > 0, "Must be at least one simpleExpression");

        var expr = VisitSimpleExpression(cx.GetChild<DelphiParser.SimpleExpressionContext>(0));
        if (cx.simpleExpression().Length > 1)
        {
            var len = cx.simpleExpression().Length;
            for (var ix = 0; ix < len-1; ix++)
            {
                var relOp = VisitRelOp(cx.GetChild<DelphiParser.RelOpContext>(ix));
                var simpleExpr = VisitSimpleExpression(cx.GetChild<DelphiParser.SimpleExpressionContext>(ix+1));
                expr = expr.With(relOp, simpleExpr);
            }
        }

        return expr;
    }
    
    public override ExprList VisitExpressionList(DelphiParser.ExpressionListContext cx)
    {
        return new ExprList
        {
            Items = cx.expression().Select(VisitExpression).ToImmutableArray(),
            Source = Source.Of(cx),
        };
    }
}

public abstract class Expr() : DelphiNode(DelphiNodeKind.Expr)
{
    public virtual Expr With(Operator op, Expr expr)
    {
        return new BinaryExpr
        {
            Left = this,
            Right = expr,
            Op = op,
            Source = Source + op.Source + expr.Source,
        };
    }

    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitExpr(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitExpr(this);
}

public sealed class ExprList() :
    ImmutableDelphiCollection<
        ExprList,
        Expr
    >(DelphiNodeKind.Expr)
{
     protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitExprList(this);
     protected override void Accept(DelphiVisitor visitor) => visitor.VisitExprList(this);
}

// public sealed class ExprList : Expr
// {
//     public required List<Expr> Items { get; init; }
//
//     public override IEnumerable<DelphiNode> Children => Items;
//     protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitExprList(this);
//     protected voidverride T ept<DelphiVisitor<T> visitor) => visitor.VisitExprList(this);
// }

public sealed class ParenthesizedExpr : Expr
{
    public required Expr Expr { get; init; }
    public override IEnumerable<DelphiNode> Children => [Expr];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitParenthesizedExpr(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitParenthesizedExpr(this);
}

// public sealed class ChainedExpr : Expr
// {
//     public required Expr Left { get; init; }
//     public required Expr Right { get; init; }
//     public override IEnumerable<DelphiNode> Children => [Left, Right];
//     protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitChainedExpr(this);
//     protected voidverride T ept<DelphiVisitor<T> visitor) => visitor.VisitChainedExpr(this);
// }