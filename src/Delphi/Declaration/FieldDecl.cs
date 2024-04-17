using System.Collections.Generic;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override ClassFieldDecl VisitClassField(DelphiParser.ClassFieldContext cx)
    {
        return new ClassFieldDecl
        {
            Idents = VisitIdentList(cx.identList()),
            Type = VisitTypeExpr(cx.typeExpr()),
            Source = Source.Of(cx),
        };
    }
}

public sealed class ClassFieldDecl : Decl
{
    public required SimpleIdentList Idents { get; init; }
    public required TypeExpr Type { get; init; }
    
    public override IEnumerable<DelphiNode> Children => [Idents, Type];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitClassFieldDecl(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitClassFieldDecl(this);
}

public sealed class ClassFieldDeclSection() :
    DelphiCollection<
        ClassFieldDeclSection,
        ClassFieldDecl
    >(DelphiNodeKind.ClassFieldDecl)
{
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitClassFieldDeclSection(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitClassFieldDeclSection(this);
}