using System.Collections.Generic;
using System.Linq;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override Unit VisitUnit(DelphiParser.UnitContext cx)
    {
        return new Unit
        {
            Head = VisitUnitHead(cx.unitHead()),
            Interface = VisitUnitInterface(cx.unitInterface()),
            Implementation = VisitUnitImplementation(cx.unitImplementation()),
            Block = VisitUnitBlock(cx.unitBlock()),
            Source = Source.Of(cx)
        };
    }
}

public sealed class Unit : File
{
    public required UnitHead Head { get; init; }
    public required UnitInterface Interface { get; init; }
    public required UnitImplementation Implementation { get; init; }
    public required UnitBlock Block { get; init; }

    public override IEnumerable<DelphiNode> Children => [Head, Interface, Implementation, Block];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitUnit(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitUnit(this);
}
