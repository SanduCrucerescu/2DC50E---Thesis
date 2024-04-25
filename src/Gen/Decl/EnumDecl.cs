using System.Linq;
using DelphiCSharp.Cs;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace DelphiCSharp.Gen;

public partial class CsWalker
{
    public override EnumDeclarationSyntax VisitEnumType(EnumDecl cx)
    {
        return EnumDeclaration(VisitSimpleSymbol(cx.Name).Identifier)
            .WithModifiers(VisitTypeModifiers(cx.Modifiers))
            .WithMembers(SeparatedList(
                cx.Body.Select(VisitEnumMember)
            ));
    }

    public override EnumMemberDeclarationSyntax VisitEnumMember(EnumMemberDecl cx)
    {
        return EnumMemberDeclaration(VisitSimpleSymbol(cx.Symbol).Identifier)
            .WithEqualsValue(cx.Expr is { } expr ? EqualsValueClause(VisitExpr(expr)) : default);
    }
}
