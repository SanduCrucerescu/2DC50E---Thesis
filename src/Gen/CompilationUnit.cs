using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.Linq;
using DelphiCSharp.Cs;
using DelphiCSharp.Delphi;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;
using ClassDecl = DelphiCSharp.Cs.ClassDecl;
using Generic = DelphiCSharp.Cs.Generic;
using TypeDecl = DelphiCSharp.Cs.TypeDecl;

namespace DelphiCSharp.Gen;

public partial class CsWalker
{
    public override CSharpSyntaxNode VisitNamespaceDecl(NamespaceDecl cx)
    {
        var ns = NamespaceDeclaration(VisitSymbol(cx.Symbol))
            .AddUsings([..cx.Usings.Items.Select(VisitUsingDecl)])
            .AddMembers([..cx.Types.Items.Select(VisitTypeDecl)]);
        return CompilationUnit()
            .AddMembers(ns);
    }

    public override MemberDeclarationSyntax VisitTypeDecl(TypeDecl decl)
    {
        return decl switch
        {
            ClassDecl classDecl => VisitClassDecl(classDecl),
            EnumDecl enumDecl => VisitEnumType(enumDecl),
            InterfaceTypeDecl trait => throw new NotImplementedException(),
            DelegateDecl del => VisitDelegateDecl(del),
            _ => throw new ArgumentOutOfRangeException(nameof(decl))
        };
    }

    public override TypeSyntax VisitGeneric(Generic generic)
    {
        return GenericName(
            Identifier(generic.Argument.Text),
            VisitGenericConstraintList(generic.Constraints)
        );
    }

    public override TypeParameterConstraintSyntax VisitGenericConstraint(Cs.GenericConstraint constraint)
    {
        return constraint switch
        {
            ClassConstraint => ClassOrStructConstraint(SyntaxKind.ClassConstraint),
            StructConstraint => ClassOrStructConstraint(SyntaxKind.StructConstraint),
            NewConstraint => ConstructorConstraint(),
            TypeConstraint type => TypeConstraint(VisitGeneric(type.Generic)),
            _ => throw new ArgumentOutOfRangeException(nameof(constraint))
        };
    }

    public override TypeArgumentListSyntax VisitGenericConstraintList(Cs.GenericConstraintsList constraints)
    {
        return TypeArgumentList([..constraints
            .Where(con => con is TypeConstraint)
            .Select(con => IdentifierName((con as TypeConstraint)!.Generic.Argument.Text))
        ]);
    }
    
    public override UsingDirectiveSyntax VisitUsingDecl(UsingDecl uses)
    {
        return UsingDirective(VisitSymbol(uses.Symbol));
    }

}
