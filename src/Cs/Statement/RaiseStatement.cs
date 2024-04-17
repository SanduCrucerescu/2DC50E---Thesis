using System.Collections.Generic;
using DelphiCSharp.Delphi;

namespace DelphiCSharp.Cs;

public partial class DelphiWalker
{
    public override ThrowStatement VisitRaiseStatement(RaiseStatement cx)
    {
        return new ThrowStatement
        {
            Expr = cx.Designator is { } des ? VisitDesignator(des) : null,
            Source = cx.Source,
        };
    }
}

public sealed class ThrowStatement : Statement
{
    public required Expr? Expr { get; init; }
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitStatement(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitStatement(this);

    public override IEnumerable<CsNode> Children => Expr is not null ? [Expr] : [];
}