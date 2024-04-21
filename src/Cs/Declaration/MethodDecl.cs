using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using DelphiCSharp.Delphi;
using DelphiCSharp.Semantics;

namespace DelphiCSharp.Cs;

public partial class DelphiWalker
{
    public override MethodDecl VisitMethodDecl(Delphi.MethodDecl decl)
    {
        return new MethodDecl
        {
            Head = VisitMethodHead(decl.Head),
            Body = VisitStatement(decl.Block.Body),
            LocalDecls = VisitDeclSection(decl.Block.Decls),
            Source = decl.Source,
        };
    }

    public override MethodDeclList VisitMethodDeclSection(MethodDeclSection section)
    {
        return MethodDeclList.From(section.Items.Select(VisitMethodDecl));
    }

    public override MethodHead VisitMethodHead(Delphi.MethodHead head)
    {
        var name = VisitMethodName(head.Name);
        var mods = VisitMethodDirectiveList(head.Signature.Directives) ;
        
        return new MethodHead
        {
            Name = name.Name,
            Parent = name.Parent,
            TypeArguments = name.GenericArguments,
            TypeConstraints = name.GenericConstraints,
            MethodKind = head.Signature.MethodKind.Kind switch
            {
                EMethodKind.Constructor => MethodKind.Constructor,
                EMethodKind.Destructor => MethodKind.Destructor,
                EMethodKind.Operator => MethodKind.Operator,
                EMethodKind.Function or EMethodKind.Procedure => MethodKind.Method,
                _ => throw new ArgumentOutOfRangeException()
            },
            ReturnType = head.Signature.ReturnType is not null ? VisitTypeExpr(head.Signature.ReturnType) : null,
            Params = VisitFormalParamsSection(head.Signature.Params),
            Modifiers = mods,
            Source = head.Source,
        };
    }

    public override GenericDeclSymbol VisitMethodName(MethodName cx)
    {
        Debug.Assert(cx.Segments.Items.Count > 0);
        var generic = VisitGenericIdent(cx.Segments.Items[0]);
        if (cx.Segments.Items.Count > 1)
        {
            foreach (var segment in cx.Segments)
            {
                var temp = VisitGenericIdent(segment);
                generic.GenericArguments.AddRange(temp.GenericArguments);
                generic.GenericConstraints.AddRange(temp.GenericConstraints);
                generic = new GenericDeclSymbol
                {
                    Name = temp.Name,
                    Parent = generic.Name,
                    GenericArguments = generic.GenericArguments,
                    GenericConstraints = generic.GenericConstraints,
                };
            }
        }

        return generic;
    }

    public override MethodModifierSet VisitMethodDirectiveList(MethodDirectiveList cx)
    {
        return new MethodModifierSet
        {
            Items = cx.Items.Select(VisitMethodDirective).ToHashSet(),
            Source = cx.Source,
        };
    }

    public override MethodModifier VisitMethodDirective(MethodDirective directive)
    {
        return directive.DirectiveKind switch
        {
            MethodDirectiveKind.Abstract => MethodModifier.Abstract(directive.Source),
            MethodDirectiveKind.Class => MethodModifier.Static(directive.Source),
            MethodDirectiveKind.Dynamic=> MethodModifier.Dynamic(directive.Source),
            MethodDirectiveKind.Final => MethodModifier.Sealed(directive.Source),
            MethodDirectiveKind.Inline => MethodModifier.Inline(directive.Source),
            MethodDirectiveKind.Override => MethodModifier.Override(directive.Source),
            MethodDirectiveKind.Reintroduce => MethodModifier.New(directive.Source),
            MethodDirectiveKind.Static => MethodModifier.Static(directive.Source),
            MethodDirectiveKind.Virtual => MethodModifier.Virtual(directive.Source),
            MethodDirectiveKind.Overload => MethodModifier.Nil(directive.Source),
            // PascalMethodDirective 
            //     or CDeclMethodDirective 
            //     or RegisterMethodDirective 
            //     or SafeCallMethodDirective 
            //     or StdCallMethodDirective => throw new NotImplementedException(),
            _ => throw new ArgumentOutOfRangeException(nameof(directive))
        };
    }
    
    public override MethodDecl VisitFuncDecl(FuncDecl decl)
    {
        var locals = DeclList.From([]);
        foreach (var localVar in VisitVarDeclSection(decl.LocalVars).Items)
        {
            locals.Add(localVar);
        }

        foreach (var localConst in VisitConstDeclSection(decl.LocalConsts).Items)
        {
            locals.Add(localConst);
        }

        foreach (var localFunc in VisitFuncDeclSection(decl.LocalFuncs).Items)
        {
            locals.Add(localFunc);
        }

        return new MethodDecl
        {
            Head = VisitFuncHead(decl.Head),
            LocalDecls = locals,
            Body = VisitStatement(decl.Body),
        };
    }
    
    public override MethodDeclList VisitFuncDeclSection(FuncDeclSection section)
    {
        return MethodDeclList.From(section.Items.Select(VisitFuncDecl));
    }
    
    public override MethodHead VisitFuncHead(FuncHead head)
    {
        var name = VisitSimpleIdent(head.Name);
        
        return new MethodHead
        {
            Name = name,
            Parent = null,
            TypeArguments = [],
            TypeConstraints = [],
            MethodKind = MethodKind.Method,
            ReturnType = BuiltIn.Void(head.Signature.ReturnTypeExpr.Source),
            Params = VisitFormalParamsSection(head.Signature.Params),
            Modifiers = VisitFuncDirectiveList(head.Signature.Directives),
            Source = head.Source,
        };
    }

    public override MethodModifierSet VisitFuncDirectiveList(FuncDirectiveList directiveList)
    {
        return MethodModifierSet.From(directiveList.Items.Select(VisitFuncDirective));
    }

    public override MethodModifier VisitFuncDirective(FuncDirective directive)
    {
        return directive switch
        {
            InlineFuncDirective => MethodModifier.Inline(directive.Source),
            OverrideFuncDirective => MethodModifier.Override(directive.Source),
            UnsafeFuncDirective => MethodModifier.Unsafe(directive.Source),
            CDeclFuncDirective or PascalFuncDirective 
                or RegisterFuncDirective 
                or SafeCallFuncDirective 
                or StdCallFuncDirective => throw new NotImplementedException(),
            _ => throw new ArgumentOutOfRangeException(nameof(directive))
        };
    }

    public override MethodParamsList VisitFormalParamsSection(FormalParamsSection section)
    {
        return new MethodParamsList
        {
            Items = section.Items.Aggregate(new List<MethodParam>(), (parms, param) =>
            {
                parms.AddRange(VisitFormalParam(param).Items);
                return parms;
            }).ToList(),
            
            Source = section.Source,
        };
    }

    public override MethodParamsList VisitFormalParam(FormalParam param)
    {
        return new MethodParamsList
        {
            Items = param.Names.Select(name => new MethodParam
            {
                Name = VisitSimpleIdent(name),
                Type = VisitTypeExpr(param.Type),
                Modifier = param.Modifier switch
                {
                    null or ConstModifier => null,
                    OutModifier => ParamModifier.Out,
                    VarModifier => ParamModifier.Ref,
                    _ => throw new ArgumentOutOfRangeException()
                },
                Default = param.Default is { } expr ? VisitExpr(expr) : null,
                Source = name.Source,
            }).ToList(),
            Source = param.Source,
        };
    }
}

public sealed class MethodDecl : Decl
{
    public required MethodHead Head { get; init; }
    public required DeclList LocalDecls { get; init; }
    public required Statement Body { get; init; }

    public override IEnumerable<CsNode> Children => [Head, LocalDecls, Body];
}

public sealed class MethodDeclList() :
    CsCollection<
        MethodDeclList,
        MethodDecl
    >(CsNodeKind.MethodDecl)
{
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitMethodDeclList(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitMethodDeclList(this);
}

public sealed class MethodHead() : CsNode(CsNodeKind.FuncHead), IEquatable<MethodHead>
{
    public required MethodKind MethodKind { get; init; }
    public required SimpleSymbol Name { get; init; }
    public required SimpleSymbol? Parent { get; init; }
    public required SimpleSymbolsList TypeArguments { get; init; }
    public required GenericsList TypeConstraints { get; init; }
    public required Type? ReturnType { get; init; }
    public required MethodParamsList Params { get; init; }
    public required MethodModifierSet Modifiers { get; init; }

    public override IEnumerable<CsNode> Children =>
        ReturnType is not null
            ? [Name, TypeArguments, TypeConstraints, ReturnType, Params, Modifiers]
            : [Name, TypeArguments, TypeConstraints, Params, Modifiers];
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitFuncHead(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitFuncHead(this);

    public bool EqualsSignature(MethodHead? other)
    {
        if (ReferenceEquals(null, other)) return false;
        if (ReferenceEquals(this, other)) return true;
        return base.Equals(other)
               && MethodKind == other.MethodKind
               && TypeArguments.Equals(other.TypeArguments)
               && TypeConstraints.Equals(other.TypeConstraints)
               && Equals(ReturnType, other.ReturnType)
               && Params.Equals(other.Params);
    }
    
    public bool Equals(MethodHead? other)
    {
        if (ReferenceEquals(null, other)) return false;
        if (ReferenceEquals(this, other)) return true;
        return base.Equals(other) 
               && MethodKind == other.MethodKind 
               && Name.Equals(other.Name) 
               && TypeArguments.Equals(other.TypeArguments) 
               && TypeConstraints.Equals(other.TypeConstraints) 
               && Equals(ReturnType, other.ReturnType) 
               && Params.Equals(other.Params) 
               && Modifiers.Equals(other.Modifiers);
    }

    public override bool Equals(object? obj)
    {
        return ReferenceEquals(this, obj) || obj is MethodHead other && Equals(other);
    }

    public override int GetHashCode()
    {
        return HashCode.Combine(base.GetHashCode(), (int)MethodKind, Name, TypeArguments, TypeConstraints, ReturnType, Params, Modifiers);
    }

    public static bool operator ==(MethodHead? left, MethodHead? right)
    {
        return Equals(left, right);
    }

    public static bool operator !=(MethodHead? left, MethodHead? right)
    {
        return !Equals(left, right);
    }

}

public enum MethodKind
{
    Method,
    Constructor,
    Destructor,
    Operator,
}

public sealed class MethodParamsList() :
    CsCollection<
        MethodParamsList,
        MethodParam>
    (CsNodeKind.FuncParam)
{
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitMethodParamList(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitMethodParamList(this);
}

public sealed class MethodParam() : CsNode(CsNodeKind.FuncParam)
{
    public required ParamModifier? Modifier { get; init; }
    public required Type Type { get; init; }
    public required SimpleSymbol Name { get; init; }
    public required Expr? Default { get; init; }


    public override IEnumerable<CsNode> Children =>
        Modifier is not null
            ? Default is not null
                ? [Modifier, Type, Name, Default]
                : [Modifier, Type, Name]
            : [Type, Name];
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitMethodParam(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitMethodParam(this);
}
