using System.Collections.Generic;

namespace DelphiCSharp.Cs;

public abstract class MemberDecl : Decl
{
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitMemberDecl(this);
}

public sealed class MemberDeclList() :
    CsCollection<
        MemberDeclList,
        MemberDecl
    >(CsNodeKind.MemberDecl)
{
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitMemberDeclList(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitMemberDeclList(this);
}