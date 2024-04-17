using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using DelphiCSharp.Cs;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;
using InvocationExpression = System.Linq.Expressions.InvocationExpression;

namespace DelphiCSharp.Gen;

public partial class CsWalker
{
    public override ExpressionSyntax VisitExpr(Expr cx)
    {
        return cx switch
        {
            PrimaryExpr primary => VisitPrimaryExpr(primary),
            ArrayCreationExpr arr => throw new NotImplementedException(),
            ElementAccessExpr elem => VisitElementAccessExpr(elem),
            ConstExpr constExpr => throw new NotImplementedException(),
            SymbolExpr sym => VisitSymbolExpr(sym),
            BinaryExpr bin => VisitBinaryExpr(bin),
            UnaryExpr unary => VisitUnaryExpr(unary),
            ChainedExpr chained => VisitChainedExpr(chained),
            InvocationExpr invocation => VisitInvocationExpr(invocation),
            ParenthesizedExpr paren => VisitParenthesizedExpr(paren),
            HashSetExpr hs => VisitHashSetExpression(hs),
            MemberAccessExpr memberAccess => VisitMemberAccessExpr(memberAccess),
            _ => throw new ArgumentOutOfRangeException(nameof(cx))
        };
    }

    public override MemberAccessExpressionSyntax VisitMemberAccessExpr(MemberAccessExpr cx)
    {
        return MemberAccessExpression(
            SyntaxKind.SimpleMemberAccessExpression,
            VisitExpr(cx.Expr),
            VisitSimpleSymbolExpr(cx.Member)
        );
    }

    public override ElementAccessExpressionSyntax VisitElementAccessExpr(ElementAccessExpr cx)
    {
        return ElementAccessExpression(
            VisitExpr(cx.AccessedElement),
            BracketedArgumentList(SeparatedList(cx.Expressions.Select(expr => Argument(VisitExpr(expr)))))
        );
    }

    public override ObjectCreationExpressionSyntax VisitHashSetExpression(HashSetExpr cx)
    {
        return ObjectCreationExpression(
            IdentifierName("HashSet"),
            ArgumentList(),
            InitializerExpression(SyntaxKind.CollectionInitializerExpression, SeparatedList(cx.Elems.Select(VisitExpr).ToList()))
        );
    }

    public override ParenthesizedExpressionSyntax VisitParenthesizedExpr(ParenthesizedExpr cx)
    {
        return ParenthesizedExpression(
            VisitExpr(cx.Expr)
        );
    }

    public override InvocationExpressionSyntax VisitInvocationExpr(InvocationExpr cx)
    {
        return InvocationExpression(
            VisitExpr(cx.Expr),
            ArgumentList(SeparatedList(cx.Args.Select(arg => Argument(VisitExpr(arg)))))
        );
    }

    public override ExpressionSyntax VisitChainedExpr(ChainedExpr cx)
    {
        var chain = ExprChain(cx);
        Debug.Assert(chain.Count >= 2);
        return (cx.Left, cx.Right) switch
        {
            _ => throw new NotImplementedException(),
        };
    }

    private List<Expr> ExprChain(Expr cx)
    {
        return cx switch
        {
            ChainedExpr chained => [..ExprChain(chained.Left), ..ExprChain(chained.Right)],
            _ => [cx]
        };
    }

    public override ExpressionSyntax VisitUnaryExpr(UnaryExpr cx)
    {
        return cx.Op.OpKind switch
        {
            // OpKind.Eq => expr,
            // OpKind.Neq => expr,
            // OpKind.Gt => expr,
            // OpKind.Geq => expr,
            // OpKind.Lt => expr,
            // OpKind.Leq => expr,
            OpKind.Plus => PrefixUnaryExpression(SyntaxKind.UnaryPlusExpression, VisitExpr(cx.Value)),
            OpKind.Minus => PrefixUnaryExpression(SyntaxKind.UnaryMinusExpression, VisitExpr(cx.Value)),
            // OpKind.Star => expr,
            // OpKind.Slash => expr,
            // OpKind.Mod => expr,
            // OpKind.Shl => expr,
            // OpKind.Shr => expr,
            OpKind.Not => PrefixUnaryExpression(SyntaxKind.LogicalNotExpression, VisitExpr(cx.Value)),
            // OpKind.Is => expr,
            // OpKind.In => expr,
            // OpKind.As => expr,
            OpKind.Ref => PrefixUnaryExpression(SyntaxKind.AddressOfExpression, VisitExpr(cx.Value)), // TODO : Might have to turn it into a `&` reference expression
            // OpKind.Or => expr,
            // OpKind.XOr => expr,
            // OpKind.BOr => expr,
            // OpKind.BAnd => expr,
            // OpKind.LAnd => expr,
            // OpKind.LOr => expr,
            _ => throw new ArgumentOutOfRangeException()
        };
    }

    public override ExpressionSyntax VisitBinaryExpr(BinaryExpr cx)
    {
        return BinaryExpression(
            VisitOperator(cx.Op),
            VisitExpr(cx.Left),
            VisitExpr(cx.Right)
        );
    }

    public static new SyntaxKind VisitOperator(Operator cx)
    {
        return cx.OpKind switch
        {
            OpKind.Plus => SyntaxKind.AddExpression,
            OpKind.Minus => SyntaxKind.SubtractExpression,
            OpKind.Star => SyntaxKind.MultiplyExpression,
            OpKind.Slash => SyntaxKind.DivideExpression,
            OpKind.Eq => SyntaxKind.EqualsExpression,
            OpKind.Neq => SyntaxKind.NotEqualsExpression,
            OpKind.Lt => SyntaxKind.LessThanExpression,
            OpKind.Leq => SyntaxKind.LessThanOrEqualExpression,
            OpKind.Gt => SyntaxKind.GreaterThanExpression,
            OpKind.Geq => SyntaxKind.GreaterThanOrEqualExpression,
            OpKind.LAnd => SyntaxKind.LogicalAndExpression,
            OpKind.LOr => SyntaxKind.LogicalOrExpression,
            OpKind.Mod => SyntaxKind.ModuloExpression,
            _ => throw new ArgumentOutOfRangeException()
        };
    }

    public override ExpressionSyntax VisitPrimaryExpr(PrimaryExpr cx)
    {
        return cx switch
        {
            LitExpr lit => VisitLitExpr(lit),
            ObjectCreationExpr obj => VisitObjectCreationExpr(obj),
            _ => throw new ArgumentOutOfRangeException(nameof(cx))
        };
    }

    public override ExpressionSyntax VisitSymbolExpr(SymbolExpr cx)
    {
        return VisitSymbol(cx.Symbol);
    }
    
    public override SimpleNameSyntax VisitSimpleSymbolExpr(SimpleSymbolExpr cx)
    {
        return VisitSimpleSymbol(cx.Symbol);
    }

    public override ExpressionSyntax VisitObjectCreationExpr(ObjectCreationExpr cx)
    {
        throw new NotImplementedException();
    }
}
