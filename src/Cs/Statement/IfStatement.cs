using System;
using System.Collections.Generic;

namespace DelphiCSharp.Cs;

public partial class DelphiWalker
{
    public override IfStatement VisitIfStatement(Delphi.IfStatement cx)
    {
        return new IfStatement {
            Condition = VisitExpr(cx.Condition),
            Then = VisitStatement(cx.Then),
            Else = cx.Else is { } @else ? VisitStatement(@else) : null,
            Source = cx.Source,
        };
    }
}

public sealed class IfStatement : Statement
{
    public required Expr Condition { get; init; }
    public required Statement Then { get; init; }
    public required Statement? Else { get; init; }
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitStatement(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitStatement(this);

    public override IEnumerable<CsNode> Children => Else is not null ? [Condition, Then, Else] : [Condition, Then];
}