using System;
using DelphiCSharp.Delphi;

namespace DelphiCSharp.Cs;

public partial class DelphiWalker
{
    public override Type VisitIdentTypeExpr(IdentTypeExpr cx)
    {
        return VisitIdent(cx.Ident) switch
        {
            GenericDeclSymbol => throw new NotSupportedException("Not Supported for type expression"),
            GenericTypeSymbol generic => generic.TypeParameters.Length == 0 && generic.Name.Segments.Length == 0 ? CheckForBuiltin(generic.Name.Symbol) : generic,
            QualifiedSymbol qualified => qualified.Segments.Length == 0 ? CheckForBuiltin(qualified.Symbol) : qualified,
            SimpleSymbol simple => CheckForBuiltin(simple),
            _ => throw new ArgumentOutOfRangeException()
        };
    }

    private Type CheckForBuiltin(SimpleSymbol cx)
    {
        return cx.Text.ToLower() switch
        {
            "boolean" => BuiltIn.Bool(cx.Source),
            "char" => BuiltIn.Char(cx.Source),
            "string" => BuiltIn.String(cx.Source),
            "shortstring" => BuiltIn.String(cx.Source),
            "widestring" => BuiltIn.String(cx.Source),
            "unicodestring" => BuiltIn.String(cx.Source),
            
            "shortint" => BuiltIn.Int8(cx.Source),
            "smallint" => BuiltIn.Int16(cx.Source),
            "integer" => BuiltIn.Int32(cx.Source),
            "int64" => BuiltIn.Int64(cx.Source),
            
            "byte" => BuiltIn.UInt8(cx.Source),
            "word" => BuiltIn.UInt16(cx.Source),
            "cardinal" => BuiltIn.UInt32(cx.Source),
            "uint64" => BuiltIn.UInt64(cx.Source),
            
            "single" => BuiltIn.Float32(cx.Source),
            "double" => BuiltIn.Float64(cx.Source),
            "extended" => BuiltIn.Decimal(cx.Source),
            "currency" => BuiltIn.Decimal(cx.Source),
            
            "variant" => BuiltIn.Object(cx.Source),
            "pointer" => BuiltIn.Pointer(cx.Source),
            _ => cx,
        };
    }
    
    // Delphi Type	C# Type
    //     Integer	int
    //     SmallInt	short
    //     Byte	byte
    //     Word	ushort
    //     Cardinal	uint
    //     Int64	long
    //     Byte	byte
    //     Real	float
    //     Single	float
    //     Double	double
    //     Extended	decimal
    //     Currency	decimal
    //     Boolean	bool
    //     Char	char
    //     ShortString	string
    //     AnsiString	string
    //     WideString	string
    //     UnicodeString	string
    //     Variant	object
    //     Pointer	IntPtr
    //     PChar	string (or IntPtr)
    //     TDateTime	DateTime
    //     TDate	DateTime
    //     TTime	TimeSpan
    //     TObject	object
    //     TComponent	System.ComponentModel.Component
}