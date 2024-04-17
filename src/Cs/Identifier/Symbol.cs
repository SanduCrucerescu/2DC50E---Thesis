using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using DelphiCSharp.Delphi;

namespace DelphiCSharp.Cs;

public partial class DelphiWalker
{
    public override Symbol VisitIdent(Ident ident)
    {
        return ident switch
        {
            GenericIdent genericIdent => VisitGenericIdent(genericIdent),
            Namespace ns => VisitNamespace(ns),
            NamespaceFileName nsfFileName => VisitNamespaceFileName(nsfFileName),
            TypeId typeId => VisitTypeId(typeId),
            QualifiedIdent qualifiedIdent => VisitQualifiedIdent(qualifiedIdent),
            SimpleIdent simpleIdent => VisitSimpleIdent(simpleIdent),
            _ => throw new ArgumentOutOfRangeException(nameof(ident))
        };
    }

    public override SymbolsList VisitIdentList(IdentList identList)
    {
        return new SymbolsList
        {
            Items = identList.Items.Select(VisitIdent).ToList(),
            Source = identList.Source,
        };
    }

    public override SimpleSymbol VisitSimpleIdent(SimpleIdent cx)
    {
        return new SimpleSymbol(cx.Text, cx.Source);
    }

    public override SimpleSymbolsList VisitSimpleIdentList(SimpleIdentList cx)
    {
        return [..cx.Select(VisitSimpleIdent)];
    }

    public override GenericDeclSymbol VisitGenericIdent(GenericIdent genericIdent)
    {
        var generics = genericIdent.Generics.Items.Select(VisitGeneric).ToList();
        var name = VisitQualifiedIdent(genericIdent.QualifiedIdent);
        return new GenericDeclSymbol
        {
            Name = name.Symbol,
            Parent = name.Segments.Length > 0 ? name.Segments.Last() : null,
            GenericArguments = SimpleSymbolsList.From(generics.Select(generic => generic.Argument)),
            GenericConstraints = GenericsList.From(generics)
        };
    }

    public override QualifiedSymbol VisitQualifiedIdent(QualifiedIdent cx)
    {
        return new QualifiedSymbol
        {
             Segments = VisitSimpleIdentList(cx.Segments),
             Symbol = VisitSimpleIdent(cx.Ident),
             Source = cx.Source,
        };
    }
}

public abstract class Symbol : Type
{
    // public abstract SimpleSymbol QualifiedName { get; }
}

public sealed class SymbolsList() :
    CsCollection<
        SymbolsList,
        Symbol
    >(CsNodeKind.Name)
{
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitSymbolsList(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitSymbolsList(this);
}

public sealed class SimpleSymbol() : Symbol
{
    public required string Text { get; init; }
    // public override SimpleSymbol QualifiedName => this;

    [SetsRequiredMembers]
    public SimpleSymbol(string text, Source source) : this()
    {
        Text = text;
        Source = source;
    }

    protected override bool IsLeaf => true;
    public override IEnumerable<CsNode> Children => [];
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitSimpleSymbol(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitSimpleSymbol(this);
}


public sealed class SimpleSymbolsList() :
    CsCollection<
        SimpleSymbolsList,
        SimpleSymbol
    >(CsNodeKind.SimpleName)
{
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitSimpleNameList(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitSimpleNameList(this);
}

public sealed class QualifiedSymbol : Symbol
{
    public required SimpleSymbolsList Segments { get; init; }
    public required SimpleSymbol Symbol { get; init; }

    // public override SimpleSymbol QualifiedName => Symbol;
    public override IEnumerable<CsNode> Children => [Segments, Symbol];
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitQualifiedSymbol(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitQualifiedSymbol(this);
}

public sealed class GenericTypeSymbol : Symbol
{
    public required QualifiedSymbol Name { get; init; }
    public required TypesList TypeParameters { get; init; }

    // public override SimpleSymbol QualifiedName => Name.QualifiedName;
    public override IEnumerable<CsNode> Children => [Name, TypeParameters];
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitGenericTypeSymbol(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitGenericTypeSymbol(this);
}

public sealed class GenericDeclSymbol : Symbol
{
    public required SimpleSymbol? Parent { get; init; }
    public required SimpleSymbol Name { get; init; }
    public required SimpleSymbolsList GenericArguments { get; init; }
    public required GenericsList GenericConstraints { get; init; }

    // public override SimpleSymbol QualifiedName => Name.QualifiedName;
    public override IEnumerable<CsNode> Children => [];
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitGenericDeclSymbol(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitGenericDeclSymbol(this);
    // throw new Exception("This class is a conversion helper and not part of the tree.");

}

