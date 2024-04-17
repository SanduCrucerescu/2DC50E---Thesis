
using System.Collections.Generic;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override SetTypeExpr VisitSetType(DelphiParser.SetTypeContext cx)
    {
        return new SetTypeExpr
        {
            IsPacked = false,
            SubType = VisitTypeExpr(cx.typeExpr()),
            Source = Source.Of(cx),
        };
    }
}

public sealed class SetTypeExpr : StructuredTypeExpr
{
    public required TypeExpr SubType { get; init; }

    public override IEnumerable<DelphiNode> Children => [SubType];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitSetTypeExpr(this);
}

