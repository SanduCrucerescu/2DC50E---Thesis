using System.Collections.Immutable;
using System.Linq;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override CompoundStatement VisitBlockBody(DelphiParser.BlockBodyContext cx)
    {
        return cx.compoundStatement() is { } stmt
            ? VisitCompoundStatement(stmt)
            : new CompoundStatement
            {
                Statements = [],
                Source = Source.Of(cx),
            };
    }


    public override StatementSection VisitStatementList(DelphiParser.StatementListContext cx)
    {
        return new StatementSection
        {
            Items = cx.statement().Select(VisitStatement).ToImmutableArray(),
            Source = Source.Of(cx),
        };
    }
}

public sealed class StatementSection() :
    ImmutableDelphiCollection<
        StatementSection,
        Statement
    >(DelphiNodeKind.Statement)
{
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitStatementSection(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitStatementSection(this);
}
