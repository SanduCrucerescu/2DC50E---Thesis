using System.Collections.Generic;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override RaiseStatement VisitRaiseStatement(DelphiParser.RaiseStatementContext cx)
    {
        return cx.designator() switch
        {
            [var des, var at] => new RaiseStatement
            {
                Designator = VisitDesignator(des),
                At = VisitDesignator(at),
                Source = Source.Of(cx),
            },
            [var des] => cx.AT() is not null
                ? new RaiseStatement
                {
                    Designator = null,
                    At = VisitDesignator(des),
                    Source = Source.Of(cx),
                }
                : new RaiseStatement
                {
                    Designator = VisitDesignator(des),
                    At = null,
                    Source = Source.Of(cx),
                },
            _ => new RaiseStatement
            {
                Designator = null,
                At = null,
                Source = Source.Of(cx),
            }
        };
    }
}

public sealed class RaiseStatement : Statement
{
    public required Designator? Designator { get; init; }
    public required Designator? At { get; init; }

    public override IEnumerable<DelphiNode> Children =>
        Designator is not null
            ? At is not null
                ? [Designator, At]
                : [Designator]
            : [];

    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitRaiseStatement(this);
}