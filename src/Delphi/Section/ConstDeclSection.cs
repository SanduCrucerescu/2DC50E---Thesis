using System;
using System.Linq;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override ConstDeclSection VisitConstSection(DelphiParser.ConstSectionContext cx)
    {
        if (cx.constKey().GetText() != "const")
        {
            throw new NotImplementedException("Not Supported");
        }
        
        return new ConstDeclSection
        {
            Items = cx.constDeclaration().Select(VisitConstDeclaration).ToList(),
            Source = [Source.Of(cx)]
        };
    }
}

public sealed class ConstDeclSection() : 
    DelphiCollection< 
        ConstDeclSection, 
        ConstDecl
    >(DelphiNodeKind.ConstDecl)
{
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitConstDeclSection(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitConstDeclSection(this);
}
