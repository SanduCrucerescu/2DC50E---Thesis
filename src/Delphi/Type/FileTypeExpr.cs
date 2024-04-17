using System.Collections.Generic;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override FileTypeExpr VisitFileType(DelphiParser.FileTypeContext cx)
    {
        return new FileTypeExpr
        {
            SubType = VisitTypeExpr(cx.typeExpr()),
            Source = Source.Of(cx),
        };
    }
}

public sealed class FileTypeExpr : StructuredTypeExpr
{
    public TypeExpr? SubType { get; init; }

    public override IEnumerable<DelphiNode> Children => SubType is not null ? [SubType] : [];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitStructuredTypeExpr(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitStructuredTypeExpr(this);
} 

