using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Linq;

namespace DelphiCSharp.Cs;

public partial class DelphiWalker
{
    public override ClassFieldDecl VisitClassFieldDecl(Delphi.ClassFieldDecl decl)
    {
        return new ClassFieldDecl
        {
            Modifiers = [],
            Type = VisitTypeExpr(decl.Type),
            Names = VisitSimpleIdentList(decl.Idents),
            Expr = null,
            Source = decl.Source,
        };
    }

    public override ClassFieldDeclList VisitClassFieldDeclSection(Delphi.ClassFieldDeclSection section)
    {
        return ClassFieldDeclList.From(section.Items.Select(VisitClassFieldDecl));
    }
}

public sealed class ClassFieldDecl : MemberDecl
{
    public required FieldModifierList Modifiers { get; init; }
    public required Type Type { get; init; }
    public required SimpleSymbolsList Names { get;init; }
    public required Expr? Expr { get; init; }

    public override IEnumerable<CsNode> Children =>
        Expr is not null ? [Modifiers, Type, Names, Expr] : [Modifiers, Type, Names];
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitClassFieldDecl(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitClassFieldDecl(this);
}


public sealed class ClassFieldDeclList() :
    CsCollection<
        ClassFieldDeclList,
        ClassFieldDecl
    >(CsNodeKind.ClassFieldDecl)
{
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitClassFieldDeclList(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitClassFieldDeclList(this);
}

