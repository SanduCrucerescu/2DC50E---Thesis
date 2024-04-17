using System.Collections.Generic;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override UnitImplementation VisitUnitImplementation(DelphiParser.UnitImplementationContext cx)
    {
        return new UnitImplementation
        {
            Uses = VisitUsesClause(cx.usesClause()),
            Body = VisitDeclSection(cx.declSection()),
            Source = Source.Of(cx),
        };
    }
}

public sealed class UnitImplementation() : DelphiNode(DelphiNodeKind.UnitImplementation)
{
    public required UsesSection Uses { get; init; }
    public required DeclSection Body { get; init; }

    public override IEnumerable<DelphiNode> Children => [Uses, Body];

    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitUnitImplementation(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitUnitImplementation(this);
}
