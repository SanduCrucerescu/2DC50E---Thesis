using System.Collections.Generic;
using System.Linq;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override IfStatement VisitIfStatement(DelphiParser.IfStatementContext cx)
    {
        return new IfStatement
        {
            Condition = VisitExpression(cx.expression()),
            Then = VisitStatement(cx.statement().First()!),
            Else = cx.statement().Length > 1 ? VisitStatement(cx.statement()[1]!) : null,
        };
    }
}

public sealed class IfStatement : Statement
{
    public required Expr Condition { get; init; }
    public required Statement Then { get; init; }
    public required Statement? Else { get; init; }

    public override IEnumerable<DelphiNode> Children => Else is not null ? [Condition, Then, Else] : [Condition, Then];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitIfStatement(this);
}

