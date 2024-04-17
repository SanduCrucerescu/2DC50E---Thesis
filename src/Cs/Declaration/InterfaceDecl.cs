using System.Collections.Generic;
using DelphiCSharp.Delphi;

namespace DelphiCSharp.Cs;

public sealed class InterfaceTypeDecl : TypeDecl
{
    public override required TypeModifierList Modifiers { get; init; }
    public override required SimpleSymbol Name { get; init; }
    public required SimpleSymbolsList TypeArguments { get; init; }
    public required GenericsList TypeConstraints { get; init; }
    public required SymbolsList Parents { get; init; }
    public required InterfaceMemberDeclList Members { get; init; }

    
    public override IEnumerable<CsNode> Children => [Modifiers, Name, Parents, Members];
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitInterfaceTypeDecl(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitInterfaceTypeDecl(this);
}

public sealed class InterfaceMemberDecl : MemberDecl
{
    public required MethodDeclList Methods { get; init; }
    public required PropertyDeclList Props { get; init; }

    public override IEnumerable<CsNode> Children => [Methods, Props];
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitInterfaceMemberDecl(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitInterfaceMemberDecl(this);
}

public sealed class InterfaceMemberDeclList() :
    CsCollection<
        InterfaceMemberDeclList,
        InterfaceMemberDecl
    >(CsNodeKind.InterfaceMemberDecl)
{
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitInterfaceMemberDeclList(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitInterfaceMemberDeclList(this);
}

