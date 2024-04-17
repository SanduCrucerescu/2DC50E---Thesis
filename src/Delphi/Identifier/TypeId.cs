using System;
using System.Collections.Generic;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override TypeId VisitTypeId(DelphiParser.TypeIdContext cx)
    {
        return new TypeId
        {
            Namespace = cx.namespaceName() != null ? VisitNamespaceName(cx.namespaceName()) : null,
            QualifiedIdent = VisitIdent(cx.ident()),
            Source = Source.Of(cx)
        };
    }
}

public sealed class TypeId : Ident, IEquatable<TypeId>
{
    public required Namespace? Namespace { get; init; }
    public required SimpleIdent QualifiedIdent { get; init; }

    public override IEnumerable<DelphiNode> Children => Namespace != null ? [Namespace, QualifiedIdent] : [QualifiedIdent];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitTypeId(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitTypeId(this);
    
    public bool Equals(TypeId? other)
    {
        if (ReferenceEquals(null, other)) return false;
        if (ReferenceEquals(this, other)) return true;
        return base.Equals(other) && Equals(Namespace, other.Namespace) && QualifiedIdent.Equals(other.QualifiedIdent);
    }

    public override bool Equals(object? obj)
    {
        return ReferenceEquals(this, obj) || obj is TypeId other && Equals(other);
    }

    public override int GetHashCode()
    {
        return HashCode.Combine(base.GetHashCode(), Namespace, QualifiedIdent);
    }

    public static bool operator ==(TypeId? left, TypeId? right)
    {
        return Equals(left, right);
    }

    public static bool operator !=(TypeId? left, TypeId? right)
    {
        return !Equals(left, right);
    }
}
