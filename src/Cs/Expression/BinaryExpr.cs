using System.Collections.Generic;
using DelphiCSharp.Delphi;

namespace DelphiCSharp.Cs;

public partial class DelphiWalker
{
    public override Expr VisitBinaryExpr(Delphi.BinaryExpr cx)
    {
        var op = VisitOperator(cx.Op);
        if (op is { OpKind: OpKind.In })
        {
            var containsMethod = new MemberAccessExpr
            {
                Expr = VisitExpr(cx.Left),
                Member = new SimpleSymbolExpr
                {
                    Symbol = new SimpleSymbol("Contains", op.Source),
                    Source = op.Source,
                }
            };
            return new InvocationExpr
            {
                Expr = containsMethod,
                Args = [VisitExpr(cx.Right)],
                Source = cx.Source,
            };
        }
        
        return new BinaryExpr
        {
            Left = VisitExpr(cx.Left),
            Right = VisitExpr(cx.Right),
            Op = op,
            Source = cx.Source,
        };
    }
}


public sealed class BinaryExpr : Expr
{
    public required Expr Left { get; init; }
    public required Expr Right { get; init; }
    public required Operator Op { get; init; }
    public override IEnumerable<CsNode> Children => [Left, Right];
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitBinaryExpr(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitBinaryExpr(this);
}
