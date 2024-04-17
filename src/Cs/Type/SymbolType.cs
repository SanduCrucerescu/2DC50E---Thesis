using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using DelphiCSharp.Delphi;

namespace DelphiCSharp.Cs;

public partial class DelphiWalker
{
    public override Type VisitGenericTypeExpr(GenericTypeExpr cx)
    {
        if (cx.Generics.Length == 0 && (cx.Ident.Namespace == null || cx.Ident.Namespace.Segments.Length == 0))
        {
            return CheckForBuiltin(VisitSimpleIdent(cx.Ident.QualifiedIdent));
        }
        
        return new GenericTypeSymbol
        {
            Name = VisitTypeId(cx.Ident),
            TypeParameters = [..cx.Generics.Select(VisitTypeExpr)],
            Source = cx.Source,
        };
    }

    public override TypesList VisitGenericTypeExprList(GenericTypeExprList cx)
    {
        return TypesList.From(cx.Select(VisitGenericTypeExpr));
    }

    public override TypesList VisitTypeExprList(TypeExprList cx)
    {
        return TypesList.From(cx.Select(VisitTypeExpr));
    }
}


public class BuiltIn : Type
{
    public required BuiltinType Type { get; init; }
    public required string Text { get; init; }

    [SetsRequiredMembers]
    public BuiltIn(BuiltinType type, string text, Source source)
    {
        Type = type;
        Text = text;
        Source = source;
    }

    protected override bool IsLeaf => true;
    public override IEnumerable<CsNode> Children => [];

    public static BuiltIn Var(Source source) => new(BuiltinType.Void, "var", source);
    public static BuiltIn Void(Source source) => new(BuiltinType.Void, "void", source);
    public static BuiltIn Bool(Source source) => new(BuiltinType.Bool, "bool", source);
    public static BuiltIn Char(Source source) => new(BuiltinType.Char, "char", source);
    public static BuiltIn String(Source source) => new(BuiltinType.String, "string", source);
    public static BuiltIn Int8(Source source) => new(BuiltinType.Int8, "sbyte", source);
    public static BuiltIn Int16(Source source) => new(BuiltinType.Int16, "short", source);
    public static BuiltIn Int32(Source source) => new(BuiltinType.Int32, "int", source);
    public static BuiltIn Int64(Source source) => new(BuiltinType.Int64, "long", source);
    public static BuiltIn UInt8(Source source) => new(BuiltinType.UInt8, "byte", source);
    public static BuiltIn UInt16(Source source) => new(BuiltinType.UInt16, "ushort", source);
    public static BuiltIn UInt32(Source source) => new(BuiltinType.UInt32, "uint", source);
    public static BuiltIn UInt64(Source source) => new(BuiltinType.UInt64, "ulong", source);
    public static BuiltIn Float32(Source source) => new(BuiltinType.Float32, "float", source);
    public static BuiltIn Float64(Source source) => new(BuiltinType.Float64, "double", source);
    public static BuiltIn Decimal(Source source) => new(BuiltinType.Decimal, "decimal", source);
    public static BuiltIn Object(Source source) => new(BuiltinType.Object, "object", source);
    public static BuiltIn Pointer(Source source) => new(BuiltinType.Pointer, "IntPtr", source);
}

public enum BuiltinType
{
    Var,
    Void,
    Bool,
    Char,
    String,
    Int8,
    Int16,
    Int32,
    Int64,
    UInt8,
    UInt16,
    UInt32,
    UInt64,
    Float32,
    Float64,
    Decimal,
    Object,
    Pointer,
}

