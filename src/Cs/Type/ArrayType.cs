using System.Collections.Generic;

namespace DelphiCSharp.Cs;

public partial class DelphiWalker
{
    public ArrayType VisitArrayTypeExpr(Delphi.ArrayTypeExpr cx)
    {
        return new ArrayType
        {
            Subtype = VisitTypeExpr(cx.SubTypeExpr),
            Dimensions = VisitExprList(cx.Dimensions),
            Source = cx.Source,
        };
    }
}

public sealed class ArrayType : Type
{
    public required Type Subtype { get; init; }
    public required ExprList Dimensions { get; init; }
    
    public override IEnumerable<CsNode> Children => [Subtype];
}
