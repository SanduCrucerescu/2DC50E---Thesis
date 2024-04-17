using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override UnitInterface VisitUnitInterface(DelphiParser.UnitInterfaceContext cx)
    {
        var unitInterface = new UnitInterface(Source.Of(cx));
        foreach (var section in cx.interfaceDecl())
        {
            unitInterface.Set(VisitInterfaceDecl(section));
        }

        return unitInterface;
    }

    public override Node<DelphiNode, DelphiNodeKind> VisitInterfaceDecl(DelphiParser.InterfaceDeclContext cx)
    {
        if (cx.constSection() is { } cs) return VisitConstSection(cs);
        if (cx.typeSection() is { } ts) return VisitTypeSection(ts);
        if (cx.varSection() is { } vs) return VisitVarSection(vs);
        if (cx.procDecl() is { } proc) return VisitProcDecl(proc);
        
        throw new ArgumentOutOfRangeException(nameof(cx));
    }
}

public sealed class UnitInterface() : DelphiNode(DelphiNodeKind.UnitInterface)
{
    public required UsesSection Uses { get; init; }
    public required FuncDeclSection Funcs { get; init; }
    public required TypeDeclSection Types { get; init; }
    public required VarDeclSection Vars { get; init; }
    public required ConstDeclSection Consts { get; init; }

    [SetsRequiredMembers]
    public UnitInterface(Source source) : this()
    {
        Uses = UsesSection.Nil;
        Funcs = FuncDeclSection.Nil;
        Types = TypeDeclSection.Nil;
        Vars = VarDeclSection.Nil;
        Consts = ConstDeclSection.Nil;
        Source = source;
    }

    public override IEnumerable<DelphiNode> Children =>
        [Uses, Funcs, Types, Vars, Consts];

    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitUnitInterface(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitUnitInterface(this);
    
    public void Set(Node<DelphiNode, DelphiNodeKind> section)
    {
        switch (section)
        {
            case FuncDecl decl: Funcs.Add(decl); break;
            case ConstDeclSection consts: Consts.AddRange(consts); break;
            case FuncDeclSection funcs: Funcs.AddRange(funcs); break;
            case TypeDeclSection types: Types.AddRange(types);break;
            case VarDeclSection vars: Vars.AddRange(vars); break;
            default: return;
        }
    }
}
