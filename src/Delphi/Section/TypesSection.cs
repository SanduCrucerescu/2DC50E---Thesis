using System.Linq;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override TypeDeclSection VisitTypeSection(DelphiParser.TypeSectionContext cx)
    {
        return new TypeDeclSection
        {
            Items = cx.typeDeclaration().Select(VisitTypeDeclaration).ToList(),
            Source = [Source.Of(cx)]
        };
    }
}

public sealed class TypeDeclSection() :
    DelphiCollection<
        TypeDeclSection,
        TypeDecl
    >(DelphiNodeKind.TypeDecl)
{
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitTypeDeclSection(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitTypeDeclSection(this);
}
