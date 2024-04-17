using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override CaseStatement VisitCaseStatement(DelphiParser.CaseStatementContext cx)
    {
        return new CaseStatement
        {
            Expr = VisitExpression(cx.expression()),
            Cases = new CaseArmList
            {
                Items = cx.caseItem().Select(VisitCaseItem).ToList(),
                Source = Source.Of(cx.caseItem()),
            },
            Source = Source.Of(cx),
        };
    }

    public override CaseArm VisitCaseItem(DelphiParser.CaseItemContext cx)
    {
        return new CaseArm
        {
            Cases = new ExprList
            {
                Items = cx.caseLabel().Select(VisitCaseLabel).ToImmutableArray(),
                Source = Source.Of(cx.caseLabel()),
            },
            Statement = VisitStatement(cx.statement()),
            Source = Source.Of(cx),
        };
    }

    public override Expr VisitCaseLabel(DelphiParser.CaseLabelContext cx)
    {
        return cx.expression() switch
        {
            [var left, var right] => new RangeExpr
            {
                From = VisitExpression(left),
                To = VisitExpression(right),
                Source = Source.Of(cx),
            },
            [var expr] => VisitExpression(expr),
            _ => throw new ArgumentOutOfRangeException(nameof(cx))
        };
    }
}

public sealed class CaseStatement : Statement
{
    public required Expr Expr { get; init; }
    public required CaseArmList Cases { get; init; }

    public override IEnumerable<DelphiNode> Children => [Expr, Cases];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitCaseStatement(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitCaseStatement(this);
}

public sealed class CaseArm() : DelphiNode(DelphiNodeKind.CaseArm)
{
    public required ExprList Cases { get; init; }
    public required Statement Statement;

    public override IEnumerable<DelphiNode> Children => [Cases, Statement];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitCaseArm(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitCaseArm(this);
}

public sealed class CaseArmList() :
    DelphiCollection<
        CaseArmList,
        CaseArm
    >(DelphiNodeKind.CaseArm)
{
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitCaseArmList(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitCaseArmList(this);
}