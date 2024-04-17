using System;
using DelphiCSharp.Cs;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace DelphiCSharp.Gen;

public partial class CsWalker
{
    public override LiteralExpressionSyntax VisitLitExpr(LitExpr cx)
    {
        return cx.LitKind switch
        {
            LitKind.U32 => LiteralExpression(
                SyntaxKind.NumericLiteralExpression,
                Literal((uint)cx.Value!)),
            LitKind.U64 => LiteralExpression(
                SyntaxKind.NumericLiteralExpression,
                Literal((ulong)cx.Value!)),
            LitKind.I32 => LiteralExpression(
                SyntaxKind.NumericLiteralExpression,
                Literal((int)cx.Value!)),
            LitKind.I64 => LiteralExpression(
                SyntaxKind.NumericLiteralExpression,
                Literal((long)cx.Value!)) ,
            LitKind.F32 => LiteralExpression(
                SyntaxKind.NumericLiteralExpression,
                Literal((float)cx.Value!)),
            LitKind.F64 => LiteralExpression(
                SyntaxKind.NumericLiteralExpression,
                Literal((double)cx.Value!)),
            LitKind.Hex => LiteralExpression(
                SyntaxKind.NumericLiteralExpression,
                Literal(((ulong)cx.Value!).ToString("X"))),
            LitKind.String => LiteralExpression(
                SyntaxKind.StringLiteralExpression,
                Literal(((string)cx.Value!).Trim('\''))),
            LitKind.True => LiteralExpression(SyntaxKind.TrueLiteralExpression),
            LitKind.False => LiteralExpression(SyntaxKind.FalseLiteralExpression),
            LitKind.Null => LiteralExpression(SyntaxKind.NullLiteralExpression),
            _ => throw new ArgumentOutOfRangeException()
        };
    }
}