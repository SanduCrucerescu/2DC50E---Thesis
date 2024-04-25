using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using DelphiCSharp.Delphi;

namespace DelphiCSharp.Cs;

public partial class DelphiWalker
{
    public override Type VisitTypeExpr(Delphi.TypeExpr cx)
    {
        return cx switch
        {
            StructuredTypeExpr structured => VisitStructuredTypeExpr(structured),
            EnumTypeExpr enumType => throw new NotImplementedException(),
            ConstTypeExpr constTypeExpr => throw new NotImplementedException(),
            FuncRefTypeExpr funcRefTypeExpr => throw new NotImplementedException(),
            FuncTypeExpr funcTypeExpr => throw new NotImplementedException(),
            GenericTypeExpr generic => VisitGenericTypeExpr(generic),
            IdentTypeExpr identTypeExpr => VisitIdentTypeExpr(identTypeExpr),
            InferTypeExpr inferTypeExpr => throw new NotImplementedException(),
            MethodTypeExpr methodTypeExpr => throw new NotImplementedException(),
            PagedAnsiStringTypeExpr pagedAnsiStringTypeExpr => throw new NotImplementedException(),
            RangeTypeExpr rangeTypeExpr => throw new NotImplementedException(),
            SizedStringTypeExpr sizedStringTypeExpr => throw new NotImplementedException(),
            TypedPointerTypeExpr typedPointerTypeExpr => throw new NotImplementedException(),
            UnpagedAnsiStringTypeExpr => throw new NotImplementedException(),
            UnsizedStringTypeExpr => BuiltIn.String(cx.Source),
            UntypedPointerTypeExpr => throw new NotImplementedException(),
            VariantTypeExpr => throw new NotImplementedException(),
            VoidTypeExpr => BuiltIn.Void(cx.Source),
            _ => throw new ArgumentOutOfRangeException(nameof(cx))
        };
    }
    
    public override Type VisitStructuredTypeExpr(StructuredTypeExpr structured)
    {
        return structured switch
        {
            ClassHelperTypeExpr clsHelper => throw new NotImplementedException(),
            // ClassOfTypeExpr clsOf => VisitClassOfType(clsOf),
            // ClassTypeExpr clsType => VisitClassType(clsType),
            InterfaceTypeExpr interfaceType => throw new NotImplementedException(),
            SimpleRecordTypeExpr simpleRecord => throw new NotImplementedException(),
            VariantRecordTypeExpr variantRecord => throw new NotImplementedException(),
            RecordHelperTypeExpr recordHelper => throw new NotImplementedException(),
            ArrayTypeExpr arrayType => VisitArrayTypeExpr(arrayType),
            FileTypeExpr fileType => throw new NotImplementedException(),
            SetTypeExpr setType => throw new NotImplementedException(),
            _ => throw new ArgumentOutOfRangeException(nameof(structured))
        };
    }
}

public abstract class Type() : CsNode(CsNodeKind.Type)
{
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitType(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitType(this);
}


public sealed class TypesList() :
    CsCollection<
        TypesList,
        Type
    >(CsNodeKind.Type)
{
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitTypesList(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitTypesList(this);
}


