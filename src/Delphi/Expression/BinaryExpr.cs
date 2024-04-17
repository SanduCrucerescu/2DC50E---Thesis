using System;
using System.Collections.Generic;

namespace DelphiCSharp.Delphi;

public sealed class BinaryExpr : Expr
{
    public required Expr Left { get; init; }
    public required Expr Right { get; init; }
    public required Operator Op { get; init; }

    public override IEnumerable<DelphiNode> Children => [Left, Right, Op];

    public override Expr With(Operator op, Expr expr)
    {
        return Op.Cmp(op) switch
        {
            Ord.Eq or Ord.Gt => new BinaryExpr
            {
                Left = this,
                Right = expr,
                Op = op,
                Source = Source + op.Source + expr.Source,
            },
            Ord.Lt => new BinaryExpr
            {
                Left = Left,
                Right = new BinaryExpr
                {
                    Left = Right,
                    Right = expr,
                    Op = op,
                    Source = Right.Source + op.Source + expr.Source,
                },
                Op = Op,
                Source = Source + op.Source + expr.Source,
            },
            _ => throw new ArgumentOutOfRangeException(nameof(op))
        };
    }

    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitBinaryExpr(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitBinaryExpr(this);
}
