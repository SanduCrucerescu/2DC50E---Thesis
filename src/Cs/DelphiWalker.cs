
using System.Diagnostics.CodeAnalysis;
using DelphiCSharp.Delphi;
using DelphiCSharp.Semantics;
using Microsoft.CodeAnalysis.CSharp;

namespace DelphiCSharp.Cs;

public partial class DelphiWalker : DelphiVisitor<Node<CsNode, CsNodeKind>>
{
    public required Semantics.Scope Root { get; init; }

    [SetsRequiredMembers]
    public DelphiWalker(Scope root)
    {
        Root = root;
    }
}

