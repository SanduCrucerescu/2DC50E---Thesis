using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using DelphiCSharp.Delphi;

namespace DelphiCSharp.Cs;

public partial class DelphiWalker
{
    public override ConstExpr VisitConstExpr(Delphi.ConstExpr expr)
    {
        return expr switch
        {
            Delphi.ArrayConstExpr arr => new ConstExpr(VisitArrayConstExpr(arr)),
            RecordConstExpr rec => new ConstExpr(VisitRecordConstExpr(rec)),
            RecordElementConstExpr recElem => throw new NotImplementedException(),
            SimpleConstExpr simple => throw new NotImplementedException(),
            _ => throw new ArgumentOutOfRangeException(nameof(expr))
        };
    }

    public override ExprList VisitConstExprList(Delphi.ConstExprList exprList)
    {
        return new ExprList
        {
            Items = exprList.Items.Select(expr => (Expr)VisitConstExpr(expr)).ToList(),
            Source = exprList.Source,
        };
    }
}

[method: SetsRequiredMembers]
public sealed class ConstExpr(Expr expr) : Expr
{
    public required Expr Expr { get; init; } = expr;

    public override IEnumerable<CsNode> Children => [Expr];
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitConstExpr(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitConstExpr(this);
}

public sealed class ConstExprList() :
    CsCollection<
        ConstExprList,
        ConstExpr
    >(CsNodeKind.ConstExpr)
{
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitConstExprList(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitConstExprList(this);
}
