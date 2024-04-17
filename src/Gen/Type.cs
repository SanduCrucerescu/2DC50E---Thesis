using System;
using System.Collections.Generic;
using DelphiCSharp.Cs;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Type = DelphiCSharp.Cs.Type;

using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace DelphiCSharp.Gen;

public partial class CsWalker
{
    public override TypeSyntax VisitType(Type cx)
    {
        return cx switch
        {
            ArrayType arr => ArrayType(
                VisitType(arr.Subtype), 
                new SyntaxList<ArrayRankSpecifierSyntax>([ArrayRankSpecifier([OmittedArraySizeExpression()])])
            ),
            Symbol sym => VisitSymbol(sym),
            BuiltIn builtIn => ParseTypeName(builtIn.Text),
            _ => throw new ArgumentOutOfRangeException(nameof(cx))
        };
    }
}
