namespace DelphiCSharp.Delphi;

public abstract class Decl() : DelphiNode(DelphiNodeKind.Decl)
{
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitDecl(this);
}
