using System.Collections.Generic;
using System.Linq;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override EnumTypeExpr VisitEnumType(DelphiParser.EnumTypeContext cx)
    {
        return new EnumTypeExpr
        {
            Variants = new EnumVariantList
            {
                Items = cx.enumVariant().Select(VisitEnumVariant).ToList(),
            },
            Source = Source.Of(cx),
        };
    }

    public override EnumVariant VisitEnumVariant(DelphiParser.EnumVariantContext cx)
    {
        return new EnumVariant
        {
            Name = VisitIdent(cx.ident()),
            Value = cx.expression() is { } expr
                ? VisitExpression(expr)
                : null,
            Source = Source.Of(cx),
        };
    }
}

public sealed class EnumTypeExpr : TypeExpr
{
    public required EnumVariantList Variants { get; init; }

    public override IEnumerable<DelphiNode> Children => [Variants];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitEnumTypeExpr(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitEnumTypeExpr(this);
}


public sealed class EnumVariant() : DelphiNode(DelphiNodeKind.EnumVariant)
{
    public required SimpleIdent Name { get; init; }
    public required Expr? Value { get; init; }

    public override IEnumerable<DelphiNode> Children => Value is not null ? [Name, Value] : [Name];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitEnumVariant(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitEnumVariant(this);
}

public sealed class EnumVariantList() :
    DelphiCollection<
        EnumVariantList,
        EnumVariant
    >(DelphiNodeKind.EnumVariant)
{
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitEnumVariantList(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitEnumVariantList(this);
}

