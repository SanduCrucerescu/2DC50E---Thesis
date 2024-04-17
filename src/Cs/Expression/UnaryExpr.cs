using System.Collections.Generic;
using DelphiCSharp.Delphi;

namespace DelphiCSharp.Cs;

public partial class DelphiWalker
{
    public override UnaryExpr VisitUnaryExpr(Delphi.UnaryExpr cx)
    {
        return new UnaryExpr
        {
            Value = VisitExpr(cx.Value),
            Op = VisitOperator(cx.Op),
            Source = cx.Source,
        };
    }
}

public sealed class UnaryExpr : Expr
{
    public required Expr Value { get; init; }
    public required Operator Op { get; init; }
    
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitExpr(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitExpr(this);
    public override IEnumerable<CsNode> Children => [Value, Op];
}