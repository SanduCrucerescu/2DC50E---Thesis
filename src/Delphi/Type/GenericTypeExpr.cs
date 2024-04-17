using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override TypeExprList VisitGenericPostfix(DelphiParser.GenericPostfixContext cx)
    {
        return new TypeExprList
        {
            Items = cx.typeExpr().Select(VisitTypeExpr).ToImmutableArray(),
            Source = Source.Of(cx),
        };
    }
}

public sealed class GenericTypeExpr : TypeExpr, IEquatable<GenericTypeExpr>
{
    public required TypeId Ident { get; init; }
    public required TypeExprList Generics { get; init; }

    public override IEnumerable<DelphiNode> Children => [Ident, Generics];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitGenericTypeExpr(this);
    
    public bool Equals(GenericTypeExpr? other)
    {
        if (ReferenceEquals(null, other)) return false;
        if (ReferenceEquals(this, other)) return true;
        return Ident.Equals(other.Ident) && Generics.Equals(other.Generics);
    }

    public override bool Equals(object? obj)
    {
        return ReferenceEquals(this, obj) || obj is GenericTypeExpr other && Equals(other);
    }

    public override int GetHashCode()
    {
        return HashCode.Combine(base.GetHashCode(), Ident, Generics);
    }

    public static bool operator ==(GenericTypeExpr? left, GenericTypeExpr? right)
    {
        return Equals(left, right);
    }

    public static bool operator !=(GenericTypeExpr? left, GenericTypeExpr? right)
    {
        return !Equals(left, right);
    }
}

public sealed class GenericTypeExprList() :
    DelphiCollection<
        GenericTypeExprList,
        GenericTypeExpr
    >(DelphiNodeKind.GenericType)
{
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitGenericTypeExprList(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitGenericTypeExprList(this);
}
