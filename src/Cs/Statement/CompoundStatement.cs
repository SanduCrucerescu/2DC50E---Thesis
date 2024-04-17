using System.Collections.Generic;

namespace DelphiCSharp.Cs;

public partial class DelphiWalker
{
    public override CompoundStatement VisitCompoundStatement(Delphi.CompoundStatement cx)
    {
        return new CompoundStatement
        {
            Statements = VisitStatementSection(cx.Statements),
            Source = cx.Source,
        };
    }
}

public sealed class CompoundStatement : Statement
{
    public required StatementList Statements { get; init; }
    
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitCompoundStatement(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitCompoundStatement(this);
    public override IEnumerable<CsNode> Children => [Statements];
}