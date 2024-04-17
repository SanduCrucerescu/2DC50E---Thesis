using System;
using System.Collections.Generic;
using System.Linq;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override UnitHead VisitUnitHead(DelphiParser.UnitHeadContext cx)
    {
        return new UnitHead
        {
            Namespace = VisitNamespaceName(cx.namespaceName()),
            Directives = cx.hintingDirective().Select(d => d.GetText()).ToList(),
            Source = Source.Of(cx)
        };
    }
}

public sealed class UnitHead() : DelphiNode(DelphiNodeKind.UnitHead)
{
    public required Namespace Namespace { get; init; }
    public required List<string> Directives { get; init; }

    public override IEnumerable<DelphiNode> Children => [Namespace];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitUnitHead(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitUnitHead(this);

    public override int GetHashCode()
    {
        return HashCode.Combine(Namespace, Directives);
    }

    public static bool operator ==(UnitHead? left, UnitHead? right)
    {
        return Equals(left, right);
    }

    public static bool operator !=(UnitHead? left, UnitHead? right)
    {
        return !Equals(left, right);
    }
    private bool Equals(UnitHead other)
    {
        return Namespace.Equals(other.Namespace) && Directives.Equals(other.Directives);
    }

    public override bool Equals(object? obj)
    {
        return ReferenceEquals(this, obj) || obj is UnitHead other && Equals(other);
    }

}

