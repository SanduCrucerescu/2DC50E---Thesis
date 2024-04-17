using System.Collections.Generic;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override Statement VisitSimpleStatement(DelphiParser.SimpleStatementContext cx)
    {
        if (cx.expression() is not null)
        {
            return VisitAssignStatement(cx.designator(), cx.expression());
        }

        return new SimpleStatement
        {
            Expr = VisitDesignator(cx.designator()),
            Source = Source.Of(cx),
        };
    }
}

public sealed class SimpleStatement : Statement
{
    public required Designator Expr { get; init; }

    public override IEnumerable<DelphiNode> Children => [Expr];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitSimpleStatement(this);
}

