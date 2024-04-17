using System.Collections.Generic;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override UnitBlock VisitUnitBlock(DelphiParser.UnitBlockContext cx)
    {
        var (init, final) = cx.unitInitialization() is { } unitInit
            ? (VisitUnitInitialization(unitInit), unitInit.unitFinalization() is { } unitFinal
                ? VisitUnitFinalization(unitFinal)
                : [])
            : ([], []);
        return new UnitBlock
        {
            Statements = cx.compoundStatement() is { } stmts
                ? VisitCompoundStatement(stmts).Statements
                : [],
            Init = init,
            Final = final,
        };
    }
    
    public override StatementSection VisitUnitInitialization(DelphiParser.UnitInitializationContext cx)
    {
        return VisitStatementList(cx.statementList());
    }

    public override StatementSection VisitUnitFinalization(DelphiParser.UnitFinalizationContext cx)
    {
        return VisitStatementList(cx.statementList());
    }
}

public sealed class UnitBlock() : DelphiNode(DelphiNodeKind.UnitBlock)
{
    public required StatementSection Init { get; init; }
    public required StatementSection Final { get; init; }
    public required StatementSection Statements { get; init; }

    public override IEnumerable<DelphiNode> Children =>
        [Init, Final, Statements];

    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitUnitBlock(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitUnitBlock(this);
}

