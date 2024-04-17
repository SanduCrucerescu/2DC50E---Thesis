using System.Collections.Generic;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override CompoundStatement VisitCompoundStatement(DelphiParser.CompoundStatementContext cx)
    {
        return new CompoundStatement
        {
            Statements = cx.statementList() is { } stmts
                ? VisitStatementList(stmts)
                : [],
            Source = Source.Of(cx),
        };
    }
}

public sealed class CompoundStatement : Statement
{
    public required StatementSection Statements { get; init; }

    public override IEnumerable<DelphiNode> Children => [Statements];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitCompoundStatement(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitCompoundStatement(this);
}

