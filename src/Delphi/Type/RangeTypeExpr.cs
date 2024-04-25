using System;
using System.Collections.Generic;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override RangeTypeExpr VisitSubRangeType(DelphiParser.SubRangeTypeContext cx)
    {
        return cx.constExpression() switch
        {
            [var from, var to] => new RangeTypeExpr
            {
                From = VisitConstExpression(from),
                To = VisitConstExpression(to),
                Source = Source.Of(cx),
            },
            [var to] => new RangeTypeExpr
            {
                From = new SimpleConstExpr
                {
                    Expr = new LitExpr
                    {
                        LitKind = LitKind.I32,
                        Value = 0,
                    }
                },
                To = VisitConstExpression(to),
                Source = Source.Of(cx),
            },
            _ => throw new ArgumentOutOfRangeException(nameof(cx.constExpression))
        };
    }
}

public sealed class RangeTypeExpr : TypeExpr
{
    public required ConstExpr From { get; init; }
    public required ConstExpr To { get; init; }

    public override IEnumerable<DelphiNode> Children => [From, To];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitRangeTypeExpr(this);
}

