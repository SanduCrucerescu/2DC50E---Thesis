using System.Collections.Generic;

namespace DelphiCSharp.Cs;

public partial class DelphiWalker
{
    public override HashSetExpr VisitSetExpr(Delphi.SetExpr cx)
    {
        return new HashSetExpr
        {
            Elems = VisitExprList(cx.Elems),
            Source = cx.Source,
        };
    }
}

public sealed class HashSetExpr : Expr
{
    public required ExprList Elems { get; init; }
    public override IEnumerable<CsNode> Children => [Elems];
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitHashSetExpression(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitHashSetExpression(this);
}