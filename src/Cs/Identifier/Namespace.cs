using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;
using DelphiCSharp.Delphi;

namespace DelphiCSharp.Cs;

public partial class DelphiWalker
{
    public override QualifiedSymbol VisitNamespace(Namespace cx)
    {
        Debug.Assert(cx.Segments.Items.Length > 0);
        var syms = VisitSimpleIdentList(cx.Segments);
        return new QualifiedSymbol
        {
            Segments = syms.Length > 0 ? [..syms.Items[..^1]] : [],
            Symbol = syms.Items[^1],
            Source = cx.Source,
        };
    }

    public override SymbolsList VisitNamespaceList(NamespaceList nsList)
    {
        return new SymbolsList
        {
            Items = nsList.Namespaces.Select(ns => (Symbol)VisitNamespace(ns)).ToList(),
            Source = nsList.Source,
        };
    }

    public override QualifiedSymbol VisitTypeId(TypeId typeId)
    {
        return new QualifiedSymbol
        {
            Segments = typeId.Namespace is { } ns ? VisitSimpleIdentList(ns.Segments) : [],
            Symbol = VisitSimpleIdent(typeId.QualifiedIdent),
            Source = typeId.Source,
        };
    }

    public override Symbol VisitNamespaceFileName(NamespaceFileName namespaceFileName)
    {
        if (namespaceFileName.FileName is not null)
        {
            throw new NotImplementedException(
                "Not planned on being supported for now, as this is outside of the current scope");
        }

        return VisitNamespace(namespaceFileName.Namespace);
    }

    public override SymbolsList VisitNamespaceFileNameList(NamespaceFileNameList list)
    {
        return new SymbolsList
        {
            Items = list.Items.Select(VisitNamespaceFileName).ToList(),
            Source = list.Source,
        };
    }
}