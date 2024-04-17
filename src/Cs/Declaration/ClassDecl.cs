using System;
using System.Collections.Generic;
using DelphiCSharp.Delphi;

namespace DelphiCSharp.Cs;

public partial class DelphiWalker
{
    public override ClassTranslationHelper VisitClassOfType(ClassOfTypeExpr cx)
    {
        return new ClassTranslationHelper
        {
            Modifier = null,
            Parents = [VisitTypeId(cx.Type)],
            Fields = [],
            Properties = [],
            Types = [],
            Methods = [],
            Source = cx.Source,
        };
    }

    public override ClassTranslationHelper VisitClassType(ClassTypeExpr classExpr)
    {
        var res = new ClassTranslationHelper
        {
            Modifier = classExpr.State switch
            {
                AbstractClassState => TypeModifier.Abstract(classExpr.State.Source),
                SealedClassState => TypeModifier.Sealed(classExpr.State.Source),
                _ => null,
            },
            Parents = VisitGenericTypeExprList(classExpr.Parents),
            Fields = [],
            Properties = [],
            Types = [],
            Methods = [],
            Source = classExpr.Source,
        };

        foreach (var visSection in classExpr.Members)
        {
            var vis = FieldModifierFromVisibility(visSection.Visibility);
            foreach (var field in visSection.Fields)
            {
                res.Fields.Add(new ClassFieldDecl
                {
                    Modifiers = FieldModifierList.From([vis]),
                    Type = VisitTypeExpr(field.Type),
                    Names = VisitSimpleIdentList(field.Idents),
                    Expr = null,
                    Source = field.Source,
                });
            }
            
            foreach (var varDecl in visSection.Vars)
            {
                res.Fields.Add(new ClassFieldDecl
                {
                    Modifiers = FieldModifierList.From([vis]),
                    Type = VisitTypeExpr(varDecl.Type),
                    Names = VisitSimpleIdentList(varDecl.Idents),
                    Expr = varDecl.Expr is { } expr ? VisitExpr(expr) : null,
                    Source = varDecl.Source,
                });
            }

            foreach (var field in visSection.ClassVars)
            {
                res.Fields.Add(new ClassFieldDecl
                {
                    Modifiers = FieldModifierList.From([FieldModifier.Static(Source.Conjured), vis]),
                    Type = VisitTypeExpr(field.Type),
                    Names = VisitSimpleIdentList(field.Idents),
                    Expr = field.Expr is { } expr ? VisitExpr(expr) : null,
                    Source = field.Source,
                });
            }

            foreach (var constDecl in visSection.Consts)
            {
                res.Fields.Add(new ClassFieldDecl
                {
                    Modifiers = FieldModifierList.From([FieldModifier.Const(Source.Conjured), vis]),
                    Type = VisitTypeExpr(constDecl.Type),
                    Names = [VisitSimpleIdent(constDecl.Ident)],
                    Expr = VisitConstExpr(constDecl.Expr),
                    Source = constDecl.Source,
                });
            }

            foreach (var propDecl in visSection.Properties)
            {
                var prop = VisitPropertyDecl(propDecl);
                prop.Modifiers.Add(vis);
                res.Properties.Add(prop);
            }

            foreach (var method in visSection.Methods)
            {
                var head = VisitMethodHead(method.Head);
                head.Modifiers.Add(MethodModifierFromVisibility(visSection.Visibility));
                res.Methods.Add(new MethodDecl
                {
                    Head = head,
                    LocalDecls = VisitDeclSection(method.Block.Decls),
                    Body = VisitStatement(method.Block.Body),
                    Source = method.Source,
                });
            }

            foreach (var type in visSection.Types)
            {
                var ty = VisitTypeDecl(type);
                ty.Modifiers.Add(TypeModifierFromVisibility(visSection.Visibility));
                res.Types.Add(ty);
            }
        }

        return res;
    }

    private FieldModifier FieldModifierFromVisibility(Visibility vis)
    {
        return vis.VisKind switch
        {
            VisibilityKind.Private or VisibilityKind.StrictPrivate => FieldModifier.Private(vis.Source),
            VisibilityKind.Protected or VisibilityKind.StrictProtected => FieldModifier.Protected(vis.Source),
            VisibilityKind.Public => FieldModifier.Public(vis.Source),
            VisibilityKind.Published => FieldModifier.Public(vis.Source),
            VisibilityKind.Automated => throw new NotSupportedException("Not supported in c#"),
            _ => throw new ArgumentOutOfRangeException(nameof(vis))
        };
    }

    private MethodModifier MethodModifierFromVisibility(Visibility vis)
    {
        return vis.VisKind switch
        {
            VisibilityKind.Private or VisibilityKind.StrictPrivate => MethodModifier.Private(vis.Source),
            VisibilityKind.Protected or VisibilityKind.StrictProtected => MethodModifier.Protected(vis.Source),
            VisibilityKind.Public => MethodModifier.Public(vis.Source),
            VisibilityKind.Published => MethodModifier.Public(vis.Source),
            VisibilityKind.Automated => throw new NotSupportedException("Not supported in c#"),
            _ => throw new ArgumentOutOfRangeException(nameof(vis))
        };
    }

    private TypeModifier TypeModifierFromVisibility(Visibility vis)
    {
        return vis.VisKind switch
        {
            VisibilityKind.Private or VisibilityKind.StrictPrivate => TypeModifier.Private(vis.Source),
            VisibilityKind.Protected or VisibilityKind.StrictProtected => TypeModifier.Protected(vis.Source),
            VisibilityKind.Public => TypeModifier.Public(vis.Source),
            VisibilityKind.Published => TypeModifier.Public(vis.Source),
            VisibilityKind.Automated => throw new NotSupportedException("Not supported in c#"),
            _ => throw new ArgumentOutOfRangeException(nameof(vis))
        };
    }
}

public sealed class ClassDecl : TypeDecl
{
    public override required TypeModifierList Modifiers { get; init; }
    public override required SimpleSymbol Name { get; init; }
    public required SimpleSymbolsList TypeArguments { get; init; }
    public required GenericsList TypeConstraints { get; init; }
    public required TypesList Parents { get; init; }
    public required ClassFieldDeclList Fields { get; init; }
    public required PropertyDeclList Properties { get; init; }
    public required TypeDeclList Types { get; init; }
    public required MethodDeclList Methods { get; init; }
    

    public static ClassDecl Nil => new()
    {
        Modifiers = [],
        Name = new SimpleSymbol("", Source.Nil),
        TypeArguments = [],
        TypeConstraints = [],
        Parents = [],
        Fields = [],
        Properties = [],
        Types = [],
        Methods = [],
        Source = Source.Nil,
    };

    public override IEnumerable<CsNode> Children => 
        [Modifiers, Name, Parents, TypeArguments, TypeConstraints, Fields, Properties, Types, Methods];
}

public sealed class ClassTranslationHelper() : CsNode(CsNodeKind.TranslationHelper)
{
    public required TypeModifier? Modifier { get; init; }
    public required TypesList Parents { get; init; }
    public required ClassFieldDeclList Fields { get; init; }
    public required PropertyDeclList Properties { get; init; }
    public required TypeDeclList Types { get; init; }
    public required MethodDeclList Methods { get; init; }

    public static ClassTranslationHelper Nil => new()
    {
        Modifier = null,
        Parents = [],
        Fields = [],
        Properties = [],
        Types = [],
        Methods = [],
        Source = Source.Nil,
    };

    public override IEnumerable<CsNode> Children => [];

    protected override T Accept<T>(CsVisitor<T> visitor) => throw new Exception("This is only for helping the translation from delphi to c#, not part of the actual tree.");
    protected override void Accept(CsVisitor visitor) => throw new Exception("This is only for helping the translation from delphi to c#, not part of the actual tree.");
}

