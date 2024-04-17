using System.Linq;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override UsesFileSection VisitUsesFileClause(DelphiParser.UsesFileClauseContext? cx)
    {
        return cx == null 
            ? [] 
            : new UsesFileSection
        {
            Items = VisitNamespaceFileNameList(cx.namespaceFileNameList()).Items,
            Source = Source.Of(cx),
        };
    }

    public override UsesSection VisitUsesClause(DelphiParser.UsesClauseContext? cx)
    {
        return cx == null 
            ? UsesSection.Nil 
            : new UsesSection
        {
            Items = VisitNamespaceNameList(cx.namespaceNameList()).Namespaces,
            Source = [Source.Of(cx)],
        };
    }

}

public sealed class UsesFileSection() :
    ImmutableDelphiCollection<
        UsesFileSection,
        NamespaceFileName
    >(DelphiNodeKind.NamespaceFileName)
{
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitUsesFileSection(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitUsesFileSection(this);
}

public sealed class UsesSection() :
    DelphiCollection<
        UsesSection,
        Namespace>
    (DelphiNodeKind.Namespace)
{
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitUsesSection(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitUsesSection(this);
}
