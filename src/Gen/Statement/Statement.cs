using System;
using System.Linq;
using DelphiCSharp.Cs;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace DelphiCSharp.Gen;

public partial class CsWalker
{
    public override StatementSyntax VisitStatement(Statement cx)
    {
        return cx switch
        {
            AssignStatement assign => VisitAssignStatement(assign),
            SimpleStatement simple => ExpressionStatement(VisitExpr(simple.Expr)),
            CompoundStatement compound => Block(compound.Statements.Select(VisitStatement)),
            IfStatement ifStatement => VisitIfStatement(ifStatement),
            ForStatement forStatement => VisitForStatement(forStatement),
            ForEachStatement forEachStatement => VisitForEachStatement(forEachStatement),
            ThrowStatement throwStatement => VisitThrowStatement(throwStatement),
            TryCatchStatement tryCatchStatement => VisitTryCatchStatement(tryCatchStatement),
            VariableDeclStatement variableDeclStatement => VisitVariableDeclStatement(variableDeclStatement),
            ReturnStatement returnStatement => VisitReturnStatement(returnStatement),
            _ => throw new ArgumentOutOfRangeException(nameof(cx))
        };
    }

    public override ReturnStatementSyntax VisitReturnStatement(ReturnStatement cx)
    {
        return ReturnStatement(
            VisitExpr(cx.Value)
        );
    }

    public override LocalDeclarationStatementSyntax VisitVariableDeclStatement(VariableDeclStatement cx)
    {
        return LocalDeclarationStatement(VariableDeclaration(
            VisitType(cx.Variable.Type),
            SeparatedList(cx.Variable.Names.Select(nm => VariableDeclarator(Identifier(nm.Text))
                .WithInitializer(cx.Variable.Expr is { } expr ? EqualsValueClause(VisitExpr(expr)) : default))
            )
        ));
    }

    public override ThrowStatementSyntax VisitThrowStatement(ThrowStatement cx)
    {
        return ThrowStatement(cx.Expr is not null ? VisitExpr(cx.Expr) : default);
    }

    public override TryStatementSyntax VisitTryCatchStatement(TryCatchStatement cx)
    {
        return TryStatement(
            Block(cx.Statements.Select(VisitStatement)),
            List(cx.Handlers.Select(VisitCatchHandler)),
            FinallyClause(Block(cx.Finally.Select(VisitStatement)))
        );
    }

    public override CatchClauseSyntax VisitCatchHandler(CatchHandler cx)
    {
        return CatchClause(
            CatchDeclaration(VisitType(cx.Type), cx.Alias is not null ? VisitSimpleSymbol(cx.Alias).Identifier : default),
            default,
            Block()
        );
    }

    public override ForEachStatementSyntax VisitForEachStatement(ForEachStatement cx)
    {
        return ForEachStatement(
            ParseTypeName("var"),
            VisitSimpleSymbol(cx.Item).Identifier,
            VisitExpr(cx.Iter),
            VisitStatement(cx.Statement)
        );
    }

    public override ForStatementSyntax VisitForStatement(ForStatement cx)
    {
        var forInitializer = VariableDeclaration(
            PredefinedType(Token(SyntaxKind.IntKeyword)),
            SingletonSeparatedList(VariableDeclarator(Identifier("i"))
                .WithInitializer(
                    EqualsValueClause(
                        VisitExpr(cx.InitialValue)
                    )
                )
            ));
        
        var forCondition = BinaryExpression(
            SyntaxKind.LessThanExpression,
            IdentifierName("i"),
            VisitExpr(cx.EndValue));

        return ForStatement(
            forInitializer,
            SeparatedList<ExpressionSyntax>(),
            forCondition,
            SingletonSeparatedList((ExpressionSyntax)PostfixUnaryExpression(SyntaxKind.PostIncrementExpression, IdentifierName("i"))),
            VisitStatement(cx.Statement)
        );
    }

    public override StatementSyntax VisitIfStatement(IfStatement cx)
    {
        return IfStatement(
            VisitExpr(cx.Condition),
            VisitStatement(cx.Then),
            cx.Else is { } @else ? ElseClause(VisitStatement(@else)) : null
        );
    }

    public override BlockSyntax VisitCompoundStatement(CompoundStatement cx)
    {
        return Block(cx.Statements.Select(VisitStatement));
    }

    public override ExpressionStatementSyntax VisitAssignStatement(AssignStatement cx)
    {
        return ExpressionStatement(
            AssignmentExpression(
                SyntaxKind.SimpleAssignmentExpression,
                VisitExpr(cx.Target),
                VisitExpr(cx.Value)
            )
        );
    }

    public override BlockSyntax VisitStatementList(StatementList cx)
    {
        return Block(
            cx.Select(VisitStatement)
        );
    }
}

