using System.Linq;
using DelphiCSharp.Cs;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace DelphiCSharp.Gen;

using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

public partial class CsWalker
{
    public override DelegateDeclarationSyntax VisitDelegateDecl(DelegateDecl cx)
    {
        return DelegateDeclaration(VisitType(cx.ReturnType), VisitSimpleSymbol(cx.Name).Identifier)
            .WithModifiers(VisitTypeModifiers(cx.Modifiers))
            .WithTypeParameterList(cx.TypeArguments.Length > 0
                ? TypeParameterList([..cx.TypeArguments.Items.Select(arg => TypeParameter(arg.Text))])
                : default)
            .WithParameterList(ParameterList(SeparatedList(cx.Params.Select(VisitMethodParam))));
    }
}