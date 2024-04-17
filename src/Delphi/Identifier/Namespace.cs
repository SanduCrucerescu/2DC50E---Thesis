using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override Namespace VisitNamespaceName(DelphiParser.NamespaceNameContext cx)
    {
        return new Namespace
        {
            Segments = new SimpleIdentList
            {
                Items = cx.ident().Select(VisitIdent).ToImmutableArray(),
                Source = Source.Of(cx.ident()),
            },
            Source = Source.Of(cx)
        };
    }

    public override NamespaceList VisitNamespaceNameList(DelphiParser.NamespaceNameListContext cx)
    {
        return new NamespaceList
        {
            Namespaces = cx.namespaceName().Select(VisitNamespaceName).ToList(),
            Source = Source.Of(cx)
        };
    }
}

public sealed class Namespace : Ident, IEquatable<Namespace>
{
    public required SimpleIdentList Segments { get; init; }

    public override IEnumerable<DelphiNode> Children => [Segments];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitNamespace(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitNamespace(this);
    
    public bool Equals(Namespace? other)
    {
        if (ReferenceEquals(null, other)) return false;
        if (ReferenceEquals(this, other)) return true;
        return base.Equals(other)&& Segments.Equals(other.Segments);
    }

    public override bool Equals(object? obj)
    {
        return ReferenceEquals(this, obj) || obj is Namespace other && Equals(other);
    }

    public override int GetHashCode()
    {
        return HashCode.Combine(base.GetHashCode(), Segments);
    }

    public static bool operator ==(Namespace? left, Namespace? right)
    {
        return Equals(left, right);
    }

    public static bool operator !=(Namespace? left, Namespace? right)
    {
        return !Equals(left, right);
    }
}

public sealed class NamespaceList() : DelphiNode(DelphiNodeKind.NamespaceList)
{
    public required List<Namespace> Namespaces { get; init; }

    public override IEnumerable<DelphiNode> Children => Namespaces;
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitNamespaceList(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitNamespaceList(this);
}


