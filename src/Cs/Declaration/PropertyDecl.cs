using System.Collections.Generic;
using System.Linq;

namespace DelphiCSharp.Cs;

public partial class DelphiWalker
{
    public override PropertyDecl VisitPropertyDecl(Delphi.PropertyDecl decl)
    {
        return new PropertyDecl
        {
            Modifiers = FieldModifierList.From(decl.IsClass 
                ? [FieldModifier.Static(Source.Conjured)] 
                : []),
            Type = VisitGenericIdent(decl.Type),
            Symbol = VisitSimpleIdent(decl.Ident),
            Source = decl.Source,
        };
    }

    public override PropertyDeclList VisitPropertyDeclSection(Delphi.PropertyDeclSection section)
    {
        return PropertyDeclList.From(section.Items.Select(VisitPropertyDecl));
    }
}

public sealed class PropertyDecl : MemberDecl
{
    public required FieldModifierList Modifiers { get; init; }
    public required Type Type { get; init; }
    public required SimpleSymbol Symbol { get; init; }
    
    // TODO GETTERS & SETTERS

    public override IEnumerable<CsNode> Children => [Type, Modifiers];
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitPropertyDecl(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitPropertyDecl(this);
}


public sealed class PropertyDeclList() :
    CsCollection<
        PropertyDeclList,
        PropertyDecl
    >(CsNodeKind.PropertyDecl)
{
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitPropertyDeclList(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitPropertyDeclList(this);
}