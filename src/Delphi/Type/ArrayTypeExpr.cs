using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override ArrayTypeExpr VisitArrayType(DelphiParser.ArrayTypeContext cx)
    {
        return new ArrayTypeExpr
        {
            SubTypeExpr = VisitArraySubType(cx.arraySubType()),
            Packed = false,
            Dimensions = new ExprList
            {
                Items = cx.arrayIndex().Select(VisitArrayIndex).ToImmutableArray(),
                Source = Source.Of(cx.arrayIndex()),
            },
            Source = Source.Of(cx),
        };
    }
    
    public override TypeExpr VisitArraySubType(DelphiParser.ArraySubTypeContext cx)
    {
        return cx.CONST() != null
            ? new ConstTypeExpr { Source = Source.Of(cx) }
            : VisitTypeExpr(cx.typeExpr()!);
    }

    public override Expr VisitArrayIndex(DelphiParser.ArrayIndexContext cx)
    {
        if (cx.typeId() is { } tid)
        {
            return new IdentDesignator
            {
                Ident = VisitTypeId(tid),
                Source = Source.Of(cx),
            };
        }

        return new RangeExpr
        {
            From = VisitExpression(cx.expression().First()),
            To = VisitExpression(cx.expression().Last()),
            Source = Source.Of(cx),
        };
    }
}

public sealed class ArrayTypeExpr : StructuredTypeExpr, IEquatable<ArrayTypeExpr>
{

    public required TypeExpr SubTypeExpr { get; init; }
    public required bool Packed { get; init; }
    public required ExprList Dimensions { get; init; }

    public override IEnumerable<DelphiNode> Children => [];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitStructuredTypeExpr(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitStructuredTypeExpr(this);
    
    public bool Equals(ArrayTypeExpr? other)
    {
        if (ReferenceEquals(null, other)) return false;
        if (ReferenceEquals(this, other)) return true;
        return SubTypeExpr.Equals(other.SubTypeExpr) && Packed == other.Packed && Dimensions.Equals(other.Dimensions);
    }

    public override bool Equals(object? obj)
    {
        return ReferenceEquals(this, obj) || obj is ArrayTypeExpr other && Equals(other);
    }

    public override int GetHashCode()
    {
        return HashCode.Combine(base.GetHashCode(), SubTypeExpr, Packed, Dimensions);
    }

    public static bool operator ==(ArrayTypeExpr? left, ArrayTypeExpr? right)
    {
        return Equals(left, right);
    }

    public static bool operator !=(ArrayTypeExpr? left, ArrayTypeExpr? right)
    {
        return !Equals(left, right);
    }
}
