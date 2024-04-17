using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using DelphiCSharp.Delphi;
using DelphiCSharp.Semantics;

namespace DelphiCSharp.Cs;

public partial class DelphiWalker
{
    public override CsNode VisitFile(Delphi.File file)
    {
        return file switch
        {
            Delphi.Program program => VisitProgram(program),
            Delphi.Unit unit => VisitUnit(unit),
            _ => throw new ArgumentOutOfRangeException(nameof(file))
        };
    }
    
    public override NamespaceDecl VisitProgram(Delphi.Program cx)
    {
        var fields = ClassFieldDeclList.From(
            cx.Block.Decls.Vars
                .Select(var =>
                {
                    var varDecl = VisitVarDecl(var);
                    return new ClassFieldDecl
                    {
                        Modifiers = [
                            FieldModifier.Private(Source.Conjured),
                            FieldModifier.Static(Source.Conjured)
                        ],
                        Type = varDecl.Type,
                        Names = varDecl.Names,
                        Expr = varDecl.Expr,
                        Source = varDecl.Source,
                    };
                })
        );
        fields.AddRange(ClassFieldDeclList.From(
            cx.Block.Decls.Consts
                .Select(@const =>
                    {
                        var constDecl = VisitConstDecl(@const);
                        return new ClassFieldDecl
                        {
                            Modifiers =
                            [
                                FieldModifier.Private(Source.Conjured),
                                FieldModifier.Const(Source.Conjured)
                            ],
                            Type = constDecl.Type,
                            Names = constDecl.Names,
                            Expr = constDecl.Expr,
                            Source = constDecl.Source
                        };
                    }
                ))
        );
        
        var mainClass = new ClassDecl
        {
            Modifiers = TypeModifierList.From([
                TypeModifier.Public(Source.Conjured),
                TypeModifier.Static(Source.Conjured)
            ]),
            Name = new SimpleSymbol("Program", Source.Conjured),
            TypeArguments = [],
            TypeConstraints = [],
            Parents = [],
            Fields = fields,
            Properties = [],
            Types = VisitTypeDeclSection(cx.Block.Decls.Types).ForEach(decl => decl.Modifiers.Add(TypeModifier.Private(Source.Conjured))),
            Methods = MethodDeclList.From(VisitMethodDeclSection(cx.Block.Decls.Methods)
                .Where(method =>
                {
                    var nsName = cx.Namespace.Segments.Last().Text;
                    var isTopLevelFunc = method.Head.Parent?.Text == nsName || method.Head.Parent is null;
                    var methodExists = Root.Resolve(method.Head.Name.Text, nsName) is not null;
                    return isTopLevelFunc && methodExists;
                }))
                .ForEach(method => method.Head.Modifiers.AddRange([MethodModifier.Private(Source.Conjured),  MethodModifier.Static(Source.Conjured)]))
        };
        
        var mainMethod = new MethodDecl
        {
            Head = new MethodHead
            {
                Name = new SimpleSymbol("Main", Source.Conjured),
                Parent = new SimpleSymbol("Program", Source.Conjured),
                ReturnType = BuiltIn.Void(Source.Conjured),
                TypeArguments = [],
                TypeConstraints = [],
                MethodKind = MethodKind.Method,
                Params = MethodParamsList.From([new MethodParam
                {
                    Modifier = null,
                    Name = new SimpleSymbol("args", Source.Conjured),
                    Type = new ArrayType
                    {
                        Subtype = BuiltIn.String(Source.Conjured),
                        Dimensions = [],
                        Source = Source.Conjured,
                    },
                    Default = null,
                    Source = Source.Conjured,
                }]),
                Modifiers = MethodModifierSet.From([
                    MethodModifier.Public(Source.Conjured), 
                    MethodModifier.Static(Source.Conjured),
                ]),
            },
            Body = VisitStatement(cx.Block.Body),
            LocalDecls = [],
            Source = cx.Block.Source,
        };

        mainClass.Methods.Items.Insert(0, mainMethod);
        
        return new NamespaceDecl
        {
            Symbol = VisitNamespace(cx.Namespace),
            Usings = VisitUsesFileSection(cx.Uses),
            Types = TypeDeclList.From([mainClass]),
            Source = cx.Source,
        };
    }

    public override NamespaceDecl VisitUnit(Unit unit)
    {
        var publicScope = VisitUnitInterface(unit.Interface);
        var privateScope = VisitUnitImplementation(unit.Implementation);

        var usingList = publicScope.Usings;
        usingList.AddRange(privateScope.Usings);

        var scope = new ClassDecl
        {
            Name = new SimpleSymbol("Scope", Source.Conjured),
            Modifiers = TypeModifierList.From([
                TypeModifier.Public(Source.Conjured), 
                TypeModifier.Static(Source.Conjured)
            ]),
            TypeArguments = [],
            TypeConstraints = [],
            Parents = [],
            Fields = [],
            Properties = [],
            Types = [],
            Methods = [],
            Source = unit.Source,
        };

        foreach (var privateField in privateScope.Context.Fields)
        {
            if (publicScope.Context.Fields.Items.Find(f => f.Names == privateField.Names) is { } field)
            {
                field.Modifiers.Add(FieldModifier.Public(Source.Conjured));
                scope.Fields.Add(new ClassFieldDecl
                {
                    Modifiers = field.Modifiers,
                    Names = field.Names,
                    Type = field.Type,
                    Expr = privateField.Expr,
                    Source = field.Source + privateField.Source,
                });
            }
            else
            {
                scope.Fields.Add(privateField);
            }
        }

        var privMs = privateScope.Context.Methods
            .Where(m =>
            {
                var nsName = unit.Head.Namespace.Segments.Last().Text;
                var isTopLevelFunc = m.Head.Parent?.Text == nsName || m.Head.Parent is null;
                var methodExistsOnTopLevel = Root.Resolve(m.Head.Name.Text, nsName) is not null;
                return isTopLevelFunc && methodExistsOnTopLevel;
            });
        var pubMs = publicScope.Context.Methods
            .Where(m =>
            {
                var nsName = unit.Head.Namespace.Segments.Last().Text;
                var isTopLevelFunc = m.Head.Parent?.Text == nsName || m.Head.Parent is null;
                var methodExistsOnTopLevel = Root.Resolve(m.Head.Name.Text, nsName) is not null;
                return isTopLevelFunc && methodExistsOnTopLevel;
            }).ToList();

        foreach (var privateMethod in privMs)
        {
            if (pubMs.Find(m => m.Head == privateMethod.Head) is { } method)
            {
                method.Head.Modifiers.Add(MethodModifier.Public(Source.Conjured));
                scope.Methods.Add(new MethodDecl
                {
                    Head = method.Head,
                    LocalDecls = privateMethod.LocalDecls,
                    Body = privateMethod.Body,
                    Source = method.Source + privateMethod.Source,
                });
            }
            else
            {
                scope.Methods.Add(privateMethod);
            }
            // privateMethod.Head.Print("PRIVATE HEAD");
            // if (publicScope.Context.Methods.Items.Find(m => m.Head == privateMethod.Head) is { } method)
            // {
            //     // method.Head.Print("HEAD");
            //     // (method.Head == privateMethod.Head).Print("EQ");
            //     var m = Root.Resolve<MethodSymbol>(
            //         method.Head.Name.Text,
            //         unit.Head.Namespace.Segments.Last().Text);
            //
            //     if (m is null) continue;
            //     m.AssociatedNodes.ForEach(n => n.Print("METHOD"));
            //
            //     // method.Head.Modifiers.Add(MethodModifier.Public(Source.Conjured));
            //     
            //     // scope.Methods.Add(new MethodDecl
            //     // {
            //     // Head = method.Head,
            //     // LocalDecls = privateMethod.LocalDecls,
            //     // Body = privateMethod.Body,
            //     // Source = method.Source + privateMethod.Source,
            //     // });
            // }
            // else
            // {
            //     scope.Methods.Add(privateMethod);
            // }
        }

        scope.Types.AddRange(publicScope.Context.Types);
        scope.Types.AddRange(privateScope.Context.Types);

        return new NamespaceDecl
        {
            Symbol = VisitIdent(unit.Head.Namespace),
            Usings = usingList,
            Types = TypeDeclList.From([scope]),
        };
    }
    
    public override Symbol VisitUnitHead(UnitHead head)
    {
        if (head.Directives.Count > 0)
        {
            throw new NotImplementedException(
                "Not planned on being supported for now, as this is outside of the current scope");
        }

        return VisitIdent(head.Namespace);
    }

    public override UnitScope VisitUnitInterface(UnitInterface unitInterface)
    {
        var scopedVariables = ClassFieldDeclList.From(
            VisitVarDeclSection(unitInterface.Vars)
                .Select(varDecl => new ClassFieldDecl
                {
                    Modifiers = [
                        FieldModifier.Public(Source.Conjured),
                        FieldModifier.Static(Source.Conjured)
                    ],
                    Type = varDecl.Type,
                    Names = varDecl.Names,
                    Expr = varDecl.Expr,
                    Source = varDecl.Source,
                })
        );

        scopedVariables.AddRange(ClassFieldDeclList.From(
            VisitConstDeclSection(unitInterface.Consts)
                .Select(constDecl => new ClassFieldDecl
                {
                    Modifiers = [
                        FieldModifier.Public(Source.Conjured),
                        FieldModifier.Const(Source.Conjured),
                    ],
                    Type = constDecl.Type,
                    Names = constDecl.Names,
                    Expr = constDecl.Expr,
                    Source = constDecl.Source,
                }))
        );

        return new UnitScope
        {
            Usings = VisitUsesSection(unitInterface.Uses),
            Context = new ClassDecl
            {
                Name = new SimpleSymbol("Scope", unitInterface.Source),
                Modifiers = [
                    TypeModifier.Public(Source.Conjured), 
                    TypeModifier.Static(Source.Conjured)
                ],
                TypeArguments = [],
                TypeConstraints = [],
                Parents = [],
                Fields = scopedVariables,
                Properties = [],
                Methods = VisitFuncDeclSection(unitInterface.Funcs),
                Types = VisitTypeDeclSection(unitInterface.Types)
                    .ForEach(ty => ty.Modifiers.Add(TypeModifier.Public(Source.Nil)))
            },
            Source = unitInterface.Source,
        };
    }

    public override UnitScope VisitUnitImplementation(UnitImplementation impl)
    {
        var scopedVariables = ClassFieldDeclList.From(
            VisitVarDeclSection(impl.Body.Vars)
            .Items
            .Select(varDecl => new ClassFieldDecl
            {
                Modifiers = FieldModifierList.From([
                    FieldModifier.Private(Source.Conjured),
                    FieldModifier.Static(Source.Conjured)
                ]),
                Type = varDecl.Type,
                Names = varDecl.Names,
                Expr = varDecl.Expr,
                Source = varDecl.Source,
            }));

        scopedVariables.AddRange(ClassFieldDeclList.From(
            VisitConstDeclSection(impl.Body.Consts)
            .Items
            .Select(constDecl => new ClassFieldDecl
            {
                Modifiers = [
                    FieldModifier.Private(Source.Conjured), 
                    FieldModifier.Const(Source.Conjured)
                ],
                Type = constDecl.Type,
                Names = constDecl.Names,
                Expr = constDecl.Expr,
                Source = constDecl.Source,
            })));

        var methods = VisitFuncDeclSection(impl.Body.Funcs);
        methods.AddRange(VisitMethodDeclSection(impl.Body.Methods));

        return new UnitScope
        {
            Usings = VisitUsesSection(impl.Uses),
            Context = new ClassDecl
            {
                Name = new SimpleSymbol("Scope", impl.Source),
                Modifiers = [
                    TypeModifier.Public(Source.Conjured), 
                    TypeModifier.Static(Source.Conjured)
                ],
                TypeArguments = [],
                TypeConstraints = [],
                Parents = [],
                Fields = scopedVariables,
                Properties = [],
                Methods = methods
                    .ForEach(met => met.Head.Modifiers.AddRange([
                        MethodModifier.Private(Source.Conjured), MethodModifier.Static(Source.Conjured)]
                    )),
                Types = VisitTypeDeclSection(impl.Body.Types)
                    .ForEach(ty => ty.Modifiers.Add(TypeModifier.Private(Source.Conjured)))
            },
            Source = impl.Source,
        };
    }
}

public sealed class UnitScope() : CsNode(CsNodeKind.Scope)
{
    public required UsingDeclsList Usings { get; init; }
    public required ClassDecl Context { get; init; }

    public override IEnumerable<CsNode> Children => [Usings, Context];
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitUnitScope(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitUnitScope(this);
}