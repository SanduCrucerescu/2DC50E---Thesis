using System;
using System.Collections.Generic;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override TypeExpr VisitTypeExpr(DelphiParser.TypeExprContext cx)
    {
        if (cx.structuredType() is { } st) return VisitStructuredType(st);
        if (cx.pointerType() is { } ptr) return VisitPointerType(ptr);
        if (cx.stringType() is { } str) return VisitStringType(str);
        if (cx.procedureType() is { } proc) return VisitProcedureType(proc);
        if (cx.variantType() is { } _) return new VariantTypeExpr();
        if (cx.simpleType() is { } sit) return VisitSimpleType(sit);
        if (cx.genericType() is { } type) return VisitGenericType(type);

        throw new ArgumentOutOfRangeException(nameof(cx));
    }

    public override GenericTypeExpr VisitGenericType(DelphiParser.GenericTypeContext cx)
    {
        return new GenericTypeExpr
        {
            Ident = VisitTypeId(cx.typeId()),
            Generics = cx.genericPostfix() is { } postfix 
                ? VisitGenericPostfix(postfix)
                : [],
            Source = Source.Of(cx),
        };
    }
}

public abstract class TypeExpr() : DelphiNode(DelphiNodeKind.Type)
{
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitTypeExpr(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitTypeExpr(this);
    public override IEnumerable<DelphiNode> Children => [];
}

public sealed class TypeExprList() :
    ImmutableDelphiCollection<
        TypeExprList,
        TypeExpr
    >(DelphiNodeKind.Type)
{
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitTypeExprList(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitTypeExprList(this);
}
