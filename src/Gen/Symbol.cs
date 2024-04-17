using System;
using System.Diagnostics;
using System.Linq;
using DelphiCSharp.Cs;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace DelphiCSharp.Gen;

public partial class CsWalker
{
    public override NameSyntax VisitSymbol(Symbol symbol)
    {
        return symbol switch
        {
            GenericTypeSymbol type => VisitGenericTypeSymbol(type),
            GenericDeclSymbol generic => VisitGenericDeclSymbol(generic),
            QualifiedSymbol qualified => VisitQualifiedSymbol(qualified),
            SimpleSymbol simple => IdentifierName(simple.Text),
            _ => throw new ArgumentOutOfRangeException(nameof(symbol))
        };
    }

    public override SimpleNameSyntax VisitSimpleSymbol(SimpleSymbol symbol)
    {
        return IdentifierName(symbol.Text);
    }

    public override NameSyntax VisitQualifiedSymbol(QualifiedSymbol cx)
    {
        if (cx.Segments.Length > 0)
        {
            NameSyntax left = VisitSimpleSymbol(cx.Segments.Items[0]);
            if (cx.Segments.Length > 1)
            {
                left = cx.Segments.Items[1..].Aggregate(left, (curr, seg) => QualifiedName(curr, VisitSimpleSymbol(seg)));
            }
            return QualifiedName(
                left,
                VisitSimpleSymbol(cx.Symbol)
            );
        }

        return VisitSimpleSymbol(cx.Symbol);
    }

    public override NameSyntax VisitGenericDeclSymbol(GenericDeclSymbol cx)
    {
        return GenericName(
            Identifier(cx.Name.Text),
            TypeArgumentList(cx.GenericArguments.Length > 0 ? [..cx.GenericArguments.Select(arg => IdentifierName(arg.Text))] : default)
        );
    }

    public override NameSyntax VisitGenericTypeSymbol(GenericTypeSymbol cx)
    {
        if (cx.Name.Segments.Length > 0)
        {
            NameSyntax left = VisitSimpleSymbol(cx.Name.Segments.Items[0]);
            if (cx.Name.Segments.Length > 1)
            {
                left = cx.Name.Segments.Items[1..].Aggregate(left, (curr, seg) => QualifiedName(curr, VisitSimpleSymbol(seg)));
            }

            return QualifiedName(
                left,
                cx.TypeParameters.Length > 0
                    ? GenericName(
                        VisitSimpleSymbol(cx.Name.Symbol).Identifier,
                        TypeArgumentList([..cx.TypeParameters.Select(VisitType)])
                    )
                    : VisitSimpleSymbol(cx.Name.Symbol)
            );
        }

        return cx.TypeParameters.Length > 0
            ? GenericName(VisitSimpleSymbol(cx.Name.Symbol).Identifier,
                TypeArgumentList([..cx.TypeParameters.Select(VisitType)]))
            : VisitSimpleSymbol(cx.Name.Symbol);
    }

    public override TypeArgumentListSyntax VisitGenericsList(GenericsList generics)
    {
        return TypeArgumentList([..generics.Items.Select(item => (TypeSyntax)IdentifierName(item.Argument.Text))]);
    }
}

