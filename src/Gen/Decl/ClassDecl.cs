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
using ClassFieldDecl = DelphiCSharp.Cs.ClassFieldDecl;
using CompoundStatement = DelphiCSharp.Cs.CompoundStatement;
using MethodDecl = DelphiCSharp.Cs.MethodDecl;
using MethodKind = DelphiCSharp.Cs.MethodKind;
using PropertyDecl = DelphiCSharp.Cs.PropertyDecl;

namespace DelphiCSharp.Gen;

public partial class CsWalker
{
    public override ClassDeclarationSyntax VisitClassDecl(ClassDecl cx)
    {
        var cls = ClassDeclaration(Identifier(cx.Name.Text))
            .WithModifiers(VisitTypeModifiers(cx.Modifiers))
            .WithBaseList(cx.Parents.Length > 0 
                ? BaseList(SeparatedList<BaseTypeSyntax>(cx.Parents.Select(p => SimpleBaseType(VisitType(p)))))
                : default)
            .WithTypeParameterList(cx.TypeArguments.Length > 0
                ? TypeParameterList([..cx.TypeArguments.Items.Select(arg => TypeParameter(arg.Text))])
                : default)
            .WithConstraintClauses(
                [
                    ..cx.TypeConstraints.Items.Select(cons => TypeParameterConstraintClause(
                        IdentifierName(cons.Argument.Text),
                        [..cons.Constraints.Items.Select(VisitGenericConstraint)]
                    ))
                ]
            )
            .AddMembers([
                ..cx.Properties.Select(VisitPropertyDecl),
                ..cx.Fields.Select(VisitClassFieldDecl),
                ..cx.Methods.Select(VisitMethodDecl),
                ..cx.Types.Select(VisitTypeDecl),
            ]);
        

        return cls;
    }

    public SyntaxTokenList VisitTypeModifiers(TypeModifierList cx)
    {
        return TokenList((List<SyntaxToken>)
        [
            ..cx.Select(mod => mod switch
            {
                AbstractTypeModifier => Token(SyntaxKind.AbstractKeyword),
                InternalTypeModifier => Token(SyntaxKind.InternalKeyword),
                PrivateTypeModifier => Token(SyntaxKind.PrivateKeyword),
                ProtectedTypeModifier => Token(SyntaxKind.ProtectedKeyword),
                PublicTypeModifier => Token(SyntaxKind.PublicKeyword),
                SealedTypeModifier => Token(SyntaxKind.SealedKeyword),
                StaticTypeModifier => Token(SyntaxKind.StaticKeyword),
                UnsafeTypeModifier => Token(SyntaxKind.UnsafeKeyword),
                NewTypeModifier => throw new InvalidEnumArgumentException(),
                _ => throw new ArgumentOutOfRangeException(nameof(mod))
            })
        ]);
    }
    
    public override MemberDeclarationSyntax VisitPropertyDecl(PropertyDecl cx)
    {
        return PropertyDeclaration(
            VisitType(cx.Type),
            VisitSimpleSymbol(cx.Symbol).Identifier
        );
    }

    public override MemberDeclarationSyntax VisitClassFieldDecl(ClassFieldDecl cx)
    {
        return FieldDeclaration(
            VariableDeclaration(VisitType(cx.Type))
                .WithVariables(SeparatedList([..cx.Names.Select(nm => VariableDeclarator(Identifier(nm.Text)))]))
            ).WithModifiers(TokenList(
            [
                ..cx.Modifiers.Select(mod => mod switch
                {
                    ConstFieldModifier => Token(SyntaxKind.ConstKeyword),
                    InternalFieldModifier => Token(SyntaxKind.InternalKeyword),
                    OverrideFieldModifier => Token(SyntaxKind.OverrideKeyword),
                    PrivateFieldModifier => Token(SyntaxKind.PrivateKeyword),
                    ProtectedFieldModifier => Token(SyntaxKind.ProtectedKeyword),
                    PublicFieldModifier => Token(SyntaxKind.PublicKeyword),
                    ReadonlyFieldModifier => Token(SyntaxKind.ReadOnlyKeyword),
                    SealedFieldModifier => Token(SyntaxKind.SealedKeyword),
                    StaticFieldModifier => Token(SyntaxKind.StaticKeyword),
                    VirtualFieldModifier => Token(SyntaxKind.VirtualKeyword),
                    _ => throw new ArgumentOutOfRangeException(nameof(mod))
                })
            ]
        ));
    }

    public override MemberDeclarationSyntax VisitMethodDecl(MethodDecl cx)
    {
        // if (cx.Head.MethodKind is MethodKind.Constructor or MethodKind.Destructor)
        // {
        //     cx.Head.Modifiers.Add(MethodModifier.Static(Source.Conjured));
        // }
        
        var ls = cx.Head.Modifiers.Select(mod => mod.ModKind switch
        {
            MethodModifierKind.Dynamic => throw new NotImplementedException(),
            MethodModifierKind.Inline => throw new NotImplementedException(),
            MethodModifierKind.None => default,
            MethodModifierKind.Abstract => Token(SyntaxKind.AbstractKeyword),
            MethodModifierKind.Internal => Token(SyntaxKind.InternalKeyword),
            MethodModifierKind.New => Token(SyntaxKind.NewKeyword),
            MethodModifierKind.Override => Token(SyntaxKind.OverrideKeyword),
            MethodModifierKind.Private => Token(SyntaxKind.PrivateKeyword),
            MethodModifierKind.Protected => Token(SyntaxKind.ProtectedKeyword),
            MethodModifierKind.Public => Token(SyntaxKind.PublicKeyword),
            MethodModifierKind.Sealed => Token(SyntaxKind.SealedKeyword),
            MethodModifierKind.Static => Token(SyntaxKind.StaticKeyword),
            MethodModifierKind.Unsafe => Token(SyntaxKind.UnsafeKeyword),
            MethodModifierKind.Virtual => Token(SyntaxKind.VirtualKeyword),
            _ => throw new ArgumentOutOfRangeException(nameof(mod))
        }).ToList();
        
        var mods = TokenList([..ls.Where(mod => mod.Value is not null)]);
        
        if (cx.Head.ReturnType is null)
        {
            return cx.Head.MethodKind switch
            {
                MethodKind.Constructor => ConstructorDeclaration(VisitSimpleSymbol(cx.Head.Name).Identifier)
                    .WithModifiers(mods)
                    .WithParameterList(ParameterList(SeparatedList(cx.Head.Params.Select(VisitMethodParam))))
                    .WithBody(cx.Body is CompoundStatement body ? VisitCompoundStatement(body) : Block(VisitStatement(cx.Body))),
                MethodKind.Destructor => DestructorDeclaration(VisitSimpleSymbol(cx.Head.Name).Identifier)
                    .WithModifiers(mods)
                    .WithParameterList(ParameterList(SeparatedList(cx.Head.Params.Select(VisitMethodParam))))
                    .WithBody(cx.Body is CompoundStatement body ? VisitCompoundStatement(body) : Block(VisitStatement(cx.Body))),
                MethodKind.Operator => throw new NotImplementedException(),
                _ => throw new ArgumentOutOfRangeException()
            };
        }

        var resDecl = LocalDeclarationStatement(VariableDeclaration(VisitType(cx.Head.ReturnType!),
            SeparatedList([VariableDeclarator(Identifier("Result"))])));
        var retStatement = ReturnStatement(IdentifierName("Result"));
        BlockSyntax methodBody;
        if (cx.Body is CompoundStatement compound)
        {
            if (cx.Head.ReturnType is BuiltIn { Type: BuiltinType.Void })
            {
                methodBody = VisitCompoundStatement(compound);
            }
            else
            {
                var stmts = new List<StatementSyntax> { resDecl };
                stmts.AddRange(VisitCompoundStatement(compound).Statements);
                stmts.Add(retStatement);
                methodBody = Block(stmts);
            }
        }
        else
        {
            methodBody = cx.Head.ReturnType is BuiltIn { Type: BuiltinType.Void } 
                ? Block(VisitStatement(cx.Body)) 
                : Block(resDecl, VisitStatement(cx.Body), retStatement);
        }
        
        return MethodDeclaration(VisitType(cx.Head.ReturnType!), VisitSimpleSymbol(cx.Head.Name).Identifier)
            .WithModifiers(mods)
            .WithParameterList(ParameterList(SeparatedList(cx.Head.Params.Select(VisitMethodParam))))
            .WithBody(methodBody);
    }

    public override ParameterSyntax VisitMethodParam(MethodParam cx)
    {
        return Parameter(VisitSimpleSymbol(cx.Name).Identifier)
            .WithType(VisitType(cx.Type))
            .WithModifiers(TokenList([
                cx.Modifier switch
                {
                    null => default,
                    InParamModifier => Token(SyntaxKind.InKeyword),
                    OutParamModifier => Token(SyntaxKind.OutKeyword),
                    RefParamModifier => Token(SyntaxKind.RefKeyword),
                    _ => throw new ArgumentOutOfRangeException()
                }
            ]))
            .WithDefault(cx.Default is { } expr ? EqualsValueClause(VisitExpr(expr)) : default);
    }
}