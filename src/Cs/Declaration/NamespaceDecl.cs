using System.Collections.Generic;

namespace DelphiCSharp.Cs;

public partial class DelphiWalker
{
}

public sealed class NamespaceDecl : MemberDecl
{
    public required Symbol Symbol { get;init; }
    public required UsingDeclsList Usings { get;init; }
    /// <summary>
    /// Even though in c# it is possible to have nested namespaces, Delphi
    /// only supports file level namespaces, hence there will never be sub-namespaces
    /// </summary>
    public required TypeDeclList Types { get; init; } 
    
    
    public override IEnumerable<CsNode> Children => [Symbol, Usings, Types];

    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitNamespaceDecl(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitNamespaceDecl(this);
}
