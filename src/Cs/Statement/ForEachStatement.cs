using System.Collections.Generic;
using DelphiCSharp.Delphi;

namespace DelphiCSharp.Cs;

public partial class DelphiWalker
{
    public override ForEachStatement VisitForInStatement(ForInStatement cx)
    {
        return new ForEachStatement
        {
            Item = VisitSimpleIdent(cx.Ident),
            Iter = VisitExpr(cx.Iter),
            Statement = VisitStatement(cx.Statement),
            Source = cx.Source,
        };
    }
}

public sealed class ForEachStatement : Statement
{
    public required SimpleSymbol Item { get; init; }
    public required Expr Iter { get; init; }
    public required Statement Statement { get; init; }
    
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitForEachStatement(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitForEachStatement(this);
    public override IEnumerable<CsNode> Children => [Item, Iter, Statement];
}