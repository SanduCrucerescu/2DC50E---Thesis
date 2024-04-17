using System;
using System.Collections.Generic;
using System.Diagnostics;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override TypeDecl VisitTypeDeclaration(DelphiParser.TypeDeclarationContext cx)
    {
        var ident = VisitIdent(cx.ident());
        var generics = cx.genericDefinition() is { } generic
            ? VisitGenericDefinition(generic)
            : [];
        return new TypeDecl
        {
            Name = ident,
            Generics = generics,
            TypeExpr = VisitTypeExpr(cx.typeExpr()),
            Source = Source.Of(cx),
        };
    }
}

public sealed class TypeDecl : Decl
{
    public required SimpleIdent Name { get; init; }
    public required GenericsList Generics { get; init; }
    public required TypeExpr TypeExpr { get; init; }

    public override IEnumerable<DelphiNode> Children => [Name, TypeExpr];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitTypeDecl(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitTypeDecl(this);
}

