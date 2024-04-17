using System;
using System.Linq;
using DelphiCSharp.Delphi;

namespace DelphiCSharp.Cs;

public partial class DelphiWalker
{
    public override StatementList VisitStatementSection(StatementSection section)
    {
        return StatementList.From(section.Items.Select(VisitStatement));
    }
    
    public override Statement VisitStatement(Delphi.Statement statement)
    {
        return statement switch
        {
            Delphi.AssignStatement assign => VisitAssignStatement(assign),
            BreakStatement breakStatement => throw new NotImplementedException(),
            CaseStatement caseStatement => throw new NotImplementedException(),
            Delphi.CompoundStatement compound => VisitCompoundStatement(compound),
            ContinueStatement continueStatement => throw new NotImplementedException(),
            ExitStatement exitStatement => throw new NotImplementedException(),
            ForDownToStatement forDownToStatement => throw new NotImplementedException(),
            ForInStatement forInStatement => VisitForInStatement(forInStatement),
            ForToStatement forToStatement => VisitForToStatement(forToStatement),
            GotoStatement gotoStatement => throw new NotImplementedException(),
            Delphi.IfStatement ifStatement => VisitIfStatement(ifStatement),
            LabelledStatement labelledStatement => throw new NotImplementedException(),
            RaiseStatement raiseStatement => VisitRaiseStatement(raiseStatement),
            RepeatStatement repeatStatement => throw new NotImplementedException(),
            Delphi.SimpleStatement simpleStatement => VisitSimpleStatement(simpleStatement),
            Delphi.TryExceptStatement tryExceptStatement => VisitTryExceptStatement(tryExceptStatement),
            TryFinallyStatement tryFinallyStatement => VisitTryFinallyStatement(tryFinallyStatement),
            WhileStatement whileStatement => throw new NotImplementedException(),
            WithStatement withStatement => throw new NotImplementedException(),
            _ => throw new ArgumentOutOfRangeException(nameof(statement))
        };
    }

    public override AssignStatement VisitAssignStatement(Delphi.AssignStatement statement)
    {
        return new AssignStatement
        {
            Target = VisitExpr(statement.Target),
            Value = VisitExpr(statement.Value),
            Source = statement.Source,
        };
    }
}

public abstract class Statement() : CsNode(CsNodeKind.Statement);

public sealed class StatementList() :
    CsCollection<
        StatementList,
        Statement
    >(CsNodeKind.Statement)
{
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitStatementList(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitStatementList(this);
}