using System.Collections.Generic;

namespace DelphiCSharp.Delphi;

public sealed class RangeExpr : Expr
{
    public required Expr From { get; init; }
    public required Expr To { get; init; }

    public override IEnumerable<DelphiNode> Children => [From, To];

    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitRangeExpr(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitRangeExpr(this);
}

