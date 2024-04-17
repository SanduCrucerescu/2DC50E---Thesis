using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override TypeExpr VisitPointerType(DelphiParser.PointerTypeContext cx)
    {
        if (cx.POINTER() != null) return new UntypedPointerTypeExpr
        {
            Source = Source.Of(cx)
        };
        
        return new TypedPointerTypeExpr
        {
            TypeExpr = VisitTypeExpr(cx.typeExpr()),
            Source = Source.Of(cx),
        };
    }
    public override TypeExpr VisitSimpleType(DelphiParser.SimpleTypeContext cx)
    {
        if (cx.ident() is { } ident) return new IdentTypeExpr
        {
            Ident = VisitIdent(ident),
            Source = Source.Of(cx),
        };
        if (cx.subRangeType() is { } range) return VisitSubRangeType(range);
        if (cx.enumType() is { } enm) return VisitEnumType(enm);
        throw new ArgumentOutOfRangeException(nameof(cx));
    }
}

public sealed class VoidTypeExpr : TypeExpr;
public sealed class InferTypeExpr : TypeExpr;
public sealed class ConstTypeExpr : TypeExpr;

public sealed class VariantTypeExpr : TypeExpr;
public sealed class UntypedPointerTypeExpr : TypeExpr;
public sealed class TypedPointerTypeExpr : TypeExpr
{
    public required TypeExpr TypeExpr;
    public override IEnumerable<DelphiNode> Children => [TypeExpr];
}


public sealed class IdentTypeExpr : TypeExpr
{
    public required Ident Ident { get; init; }
    public override IEnumerable<DelphiNode> Children => [Ident];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitIdentTypeExpr(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitIdentTypeExpr(this);
}

