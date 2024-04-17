using System;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override TypeExpr VisitStructuredType(DelphiParser.StructuredTypeContext cx)
    {
        var res = VisitStructuredTypePart(cx.structuredTypePart());
        res.IsPacked = cx.PACKED() != null;
        return res;
    }

    public override StructuredTypeExpr VisitStructuredTypePart(DelphiParser.StructuredTypePartContext cx)
    {
        if (cx.arrayType() is { } at) return VisitArrayType(at);
        if (cx.setType() is { } st) return VisitSetType(st);
        if (cx.fileType() is { } ft) return VisitFileType(ft);
        if (cx.classDecl() is { } ct) return VisitClassDecl(ct);
        throw new ArgumentOutOfRangeException(nameof(cx));
    }
}


public abstract class StructuredTypeExpr: TypeExpr
{
    public bool IsPacked { get; set; }
}

