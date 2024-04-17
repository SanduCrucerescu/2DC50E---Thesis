using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    private DeclSection VisitDeclSection(IEnumerable<DelphiParser.DeclSectionContext> cxs)
    {
        var decls = DeclSection.Nil;
        foreach (var s in cxs)
        {
            decls.Set(VisitDeclSection(s));
        }

        return decls;
    }

    public override DelphiNode VisitDeclSection(DelphiParser.DeclSectionContext? cx)
    {
        if (cx == null) return DeclSection.Nil;

        // if (cx.labelDeclSection() is { } ls) return VisitLabelDeclSection(ls);
        if (cx.constSection() is { } cs) return VisitConstSection(cs);
        if (cx.typeSection() is { } ts) return VisitTypeSection(ts);
        if (cx.varSection() is  { } vs) return VisitVarSection(vs);
        if (cx.procDecl() is { } proc) return VisitProcDecl(proc);
        if (cx.methodDecl() is { } method) return VisitMethodDecl(method);
        // if (cx.exportsSection() is { } exports) return VisitExportsSection(exports);
        // if (cx.exportedProcHeading() is { } procExport) return VisitExportedProcHeading(procExport);

        throw new ArgumentOutOfRangeException(nameof(cx));
    }
}

public sealed class DeclSection(): DelphiNode(DelphiNodeKind.Decl)
{
    public required TypeDeclSection Types { get; init; }
    public required VarDeclSection Vars { get; init; }
    public required ConstDeclSection Consts { get; init; }
    public required FuncDeclSection Funcs { get; init; }
    public required MethodDeclSection Methods { get; init; }

    public override IEnumerable<DelphiNode> Children => 
        [Types, Vars, Consts, Funcs, Methods];

    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitDeclSection(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitDeclSection(this);

    [SetsRequiredMembers]
    private DeclSection(Source source) : this()
    {
        Types = TypeDeclSection.Nil;
        Vars = VarDeclSection.Nil;
        Consts = ConstDeclSection.Nil;
        Funcs = FuncDeclSection.Nil;
        Methods = MethodDeclSection.Nil;
        Source = source;
    }

    public static DeclSection Of(Visitor visitor, DelphiParser.DeclSectionContext[] cx)
    {
        var res = Nil;
        foreach (var s in cx)
        {
            res.Set(visitor.VisitDeclSection(s));
        }

        return res;
    }

    public static DeclSection Nil => new(Source.Nil);
    
    public void Set(Node<DelphiNode, DelphiNodeKind> section)
   {
        switch (section)
        {
            case ConstDeclSection consts: Consts.AddRange(consts); break;
            case FuncDecl func: Funcs.Add(func); break;
            case FuncDeclSection funcs: Funcs.AddRange(funcs); break;
            case MethodDecl method: Methods.Add(method); break;
            case MethodDeclSection methods: Methods.AddRange(methods); break;
            case TypeDeclSection types: Types.AddRange(types);break;
            case VarDeclSection vars: Vars.AddRange(vars); break;
        };
    }
}

