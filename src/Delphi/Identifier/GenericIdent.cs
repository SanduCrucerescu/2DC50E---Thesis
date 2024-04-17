using System;
using System.Collections.Generic;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override GenericIdent VisitGenericTypeIdent(DelphiParser.GenericTypeIdentContext cx)
    {
        return new GenericIdent
        {
            QualifiedIdent = VisitQualifiedIdent(cx.qualifiedIdent()),
            Generics = cx.genericDefinition() is { } gs
                ? VisitGenericDefinition(gs)
                : [],
            Source = Source.Of(cx)
        };
    }
}

public sealed class GenericIdent : Ident, IEquatable<GenericIdent>
{
    public required QualifiedIdent QualifiedIdent { get; init; }
    public required GenericsList Generics { get; init; }

    public override IEnumerable<DelphiNode> Children => [QualifiedIdent, Generics];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitGenericIdent(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitGenericIdent(this);
    
    public bool Equals(GenericIdent? other)
    {
        if (ReferenceEquals(null, other)) return false;
        if (ReferenceEquals(this, other)) return true;
        return base.Equals(other) && QualifiedIdent.Equals(other.QualifiedIdent) && Generics.Equals(other.Generics);
    }

    public override bool Equals(object? obj)
    {
        return ReferenceEquals(this, obj) || obj is GenericIdent other && Equals(other);
    }

    public override int GetHashCode()
    {
        return HashCode.Combine(base.GetHashCode(), QualifiedIdent, Generics);
    }

    public static bool operator ==(GenericIdent? left, GenericIdent? right)
    {
        return Equals(left, right);
    }

    public static bool operator !=(GenericIdent? left, GenericIdent? right)
    {
        return !Equals(left, right);
    }
}

public sealed class GenericIdentList() :
    DelphiCollection<
        GenericIdentList,
        GenericIdent
    >(DelphiNodeKind.GenericIdent)
{
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitGenericIdentList(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitGenericIdentList(this);
}
