using System.Diagnostics;
using System.Linq;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override VarDeclSection VisitVarSection(DelphiParser.VarSectionContext cx)
    {
        Debug.Assert(cx.varKey().GetText() == "var", "threadvar not supported right now");
        return new VarDeclSection
        {
            Items = cx.varDeclaration().Select(VisitVarDeclaration).ToList(),
            Source = [Source.Of(cx)],
        };
    }
}

public sealed class VarDeclSection() : 
    DelphiCollection<
        VarDeclSection, 
        VarDecl
    >(DelphiNodeKind.VarDecl)
{
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitVarDeclSection(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitVarDeclSection(this);
}

public sealed class ClassVarDeclSection() : 
    DelphiCollection<
        ClassVarDeclSection, 
        VarDecl
    >(DelphiNodeKind.VarDecl)
{
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitClassVarDeclSection(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitClassVarDeclSection(this);
}
