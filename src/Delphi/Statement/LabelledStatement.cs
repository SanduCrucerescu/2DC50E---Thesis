using System.Collections.Generic;
using System.Diagnostics;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    private LabelledStatement VisitLabelledStatement(DelphiParser.StatementContext cx)
    {
        Debug.Assert(cx.label() is not null && cx.statement() is not null);
        return new LabelledStatement
        {
            Label = VisitLabel(cx.label()),
            Statement = VisitStatement(cx.statement()),
            Source = Source.Of(cx),
        };
    }
}

public sealed class LabelledStatement : Statement
{
    public required Ident Label { get; init; }
    public required Statement Statement { get; init; }

    public override IEnumerable<DelphiNode> Children => [Label, Statement];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitLabelledStatement(this);
}
