using System.Collections.Generic;

namespace DelphiCSharp.Delphi;

public sealed class UnaryExpr : Expr
{
    public required Expr Value { get; init; }
    public required Operator Op { get; init; }

    public override IEnumerable<DelphiNode> Children => [Value, Op];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitUnaryExpr(this);
}

