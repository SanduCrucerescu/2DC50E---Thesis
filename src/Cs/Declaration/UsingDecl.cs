using System.Collections.Generic;
using System.Linq;
using DelphiCSharp.Delphi;

namespace DelphiCSharp.Cs;

public partial class DelphiWalker
{
    public override UsingDeclsList VisitUsesFileSection(UsesFileSection section)
    {
        return new UsingDeclsList
        {
            Items = section.Items.Select(ns => new UsingDecl
            {
                Symbol = VisitNamespaceFileName(ns)
            }).ToList(),
            Source = section.Source,
        };
    }

    public override UsingDeclsList VisitUsesSection(UsesSection section)
    {
        return new UsingDeclsList
        {
            Items = section.Items.Select(ns => new UsingDecl
            {
                Symbol = VisitNamespace(ns)
            }).ToList(),
            Source = section.Source,
        };
    }
}

public sealed class UsingDecl : Decl
{
    public required Symbol Symbol { get; init; }

    public override IEnumerable<CsNode> Children => [Symbol];
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitUsingDecl(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitUsingDecl(this);
}

public sealed class UsingDeclsList() :
    CsCollection<
        UsingDeclsList,
        UsingDecl
    >(CsNodeKind.UsingSymbolStatement)
{
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitUsingDeclsList(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitUsingDeclsList(this);
}