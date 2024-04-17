using System.Collections.Generic;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override WhileStatement VisitWhileStatement(DelphiParser.WhileStatementContext cx)
    {
        return new WhileStatement
        {
            Condition = VisitExpression(cx.expression()),
            Statement = VisitStatement(cx.statement()),
            Source = Source.Of(cx)
        };
    }
}

public sealed class WhileStatement : Statement
{
    public required Expr Condition { get; init; }
    public required Statement Statement { get; init; }

    public override IEnumerable<DelphiNode> Children => [Condition, Statement];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitWhileStatement(this);
}