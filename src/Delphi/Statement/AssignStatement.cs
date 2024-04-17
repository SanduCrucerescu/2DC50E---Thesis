using System.Collections.Generic;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    private AssignStatement VisitAssignStatement(
        DelphiParser.DesignatorContext designatorCx, 
        DelphiParser.ExpressionContext exprCx)
    {
        return new AssignStatement
        {
            Target = VisitDesignator(designatorCx),
            Value = VisitExpression(exprCx),
            Source = Source.Of(designatorCx) + Source.Of(exprCx),
        };
    }
}

public sealed class AssignStatement : Statement
{
    public required Expr Target { get; init; }
    public required Expr Value { get; init; }

    public override IEnumerable<DelphiNode> Children => [Target, Value];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitStatement(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitStatement(this);
}


