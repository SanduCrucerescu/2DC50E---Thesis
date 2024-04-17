
using System;
using System.Collections.Generic;
using System.Linq;
using DelphiCSharp.Delphi;

namespace DelphiCSharp.Cs;

public partial class DelphiWalker
{
    public override Expr VisitExpr(Delphi.Expr expr)
    {
        return expr switch
        {
            Delphi.Designator designator => VisitDesignator(designator),
            Delphi.LitExpr litExpr => VisitLitExpr(litExpr),
            Delphi.BinaryExpr binaryExpr => VisitBinaryExpr(binaryExpr),
            Delphi.ParenthesizedExpr parens => VisitParenthesizedExpr(parens),
            RangeExpr rangeExpr => throw new NotImplementedException(),
            SetExpr setExpr => VisitSetExpr(setExpr),
            Delphi.UnaryExpr unaryExpr => VisitUnaryExpr(unaryExpr),
            _ => throw new ArgumentOutOfRangeException(nameof(expr))
        };
    }

    public override ExprList VisitExprList(Delphi.ExprList exprList)
    {
        return new ExprList
        {
            Items = exprList.Items.Select(VisitExpr).ToList(),
            Source = exprList.Source,
        };
    }

    public override ParenthesizedExpr VisitParenthesizedExpr(Delphi.ParenthesizedExpr cx)
    {
        return new ParenthesizedExpr
        {
            Expr = VisitExpr(cx.Expr),
            Source = cx.Source,
        };
    }
}

public abstract class Expr() : CsNode(CsNodeKind.Expr)
{
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitExpr(this);
}

public sealed class ExprList() :
    CsCollection<
        ExprList,
        Expr
    >(CsNodeKind.Expr)
{
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitExprList(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitExprList(this);
}

public sealed class ParenthesizedExpr : Expr
{
    public required Expr Expr { get; init; }
    public override IEnumerable<CsNode> Children => [Expr];
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitParenthesizedExpr(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitParenthesizedExpr(this);
}