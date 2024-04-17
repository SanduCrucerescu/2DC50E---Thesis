using System.Collections.Generic;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override RepeatStatement VisitRepeatStatement(DelphiParser.RepeatStatementContext cx)
    {
        return new RepeatStatement
        {
            Statements = cx.statementList() is { } stmts 
                ? VisitStatementList(stmts)
                : [],
            Until = VisitExpression(cx.expression()),
            Source = Source.Of(cx),
        };
    }
}

public sealed class RepeatStatement : Statement
{
    public required StatementSection Statements { get; init; }
    public required Expr Until { get; init; }

    public override IEnumerable<DelphiNode> Children => [Statements, Until];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitRepeatStatement(this);
}
