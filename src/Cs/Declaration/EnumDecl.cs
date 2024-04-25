
using System.Collections.Generic;
using System.Linq;
using DelphiCSharp.Delphi;

namespace DelphiCSharp.Cs;

public partial class DelphiWalker
{
    public EnumDecl VisitEnumDecl(Delphi.TypeDecl cx)
    {
        return new EnumDecl
        {
            Modifiers = [],
            Name = VisitSimpleIdent(cx.Name),
            Body = VisitEnumTypeExpr((cx.TypeExpr as EnumTypeExpr)!),
            Source = cx.Source,
        };
    }

    public override EnumBody VisitEnumTypeExpr(EnumTypeExpr cx)
    {
        return VisitEnumVariantList(cx.Variants);
    }

    public override EnumMemberDecl VisitEnumVariant(EnumVariant cx)
    {
        return new EnumMemberDecl
        {
            Symbol = VisitSimpleIdent(cx.Name),
            Expr = cx.Value is { } val ? VisitConstExpr(val) : null,
            Source = cx.Source,
        };
    }

    public override EnumBody VisitEnumVariantList(EnumVariantList cx)
    {
        return EnumBody.From(cx.Select(VisitEnumVariant));
    }
}

public sealed class EnumDecl : TypeDecl
{
    public override required TypeModifierList Modifiers { get; init; }
    public override required SimpleSymbol Name { get; init; }
    public required EnumBody Body { get; init; }

    public override IEnumerable<CsNode> Children => [Modifiers, Name, Body]; 
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitEnumType(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitEnumType(this);
}

public sealed class EnumBody() :
    CsCollection<
        EnumBody,
        EnumMemberDecl
    >(CsNodeKind.EnumMemberDecl)
{
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitEnumBody(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitEnumBody(this);
}


public sealed class EnumMemberDecl : Decl
{
    public required SimpleSymbol Symbol { get; init; }
    public required ConstExpr? Expr { get; init; }

    public override IEnumerable<CsNode> Children => Expr is not null ? [Symbol, Expr] : [Symbol];
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitEnumMember(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitEnumMember(this);
}
