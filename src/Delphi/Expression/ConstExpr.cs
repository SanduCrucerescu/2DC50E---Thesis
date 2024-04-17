using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override ConstExpr VisitConstExpression(DelphiParser.ConstExpressionContext cx)
    {
        if (cx.expression() is { } expr)
        {
            return new SimpleConstExpr
            {
                Expr = VisitExpression(expr),
                Source = Source.Of(expr)
            };
        }

        if (cx.recordConstExpression() is { Length: > 0 } record)
        {
            return new RecordConstExpr
            {
                Elems = new RecordElementConstExprList
                {
                    Items = record.Select(VisitRecordConstExpression).ToList(),
                    Source = Source.Of(record)
                },
                Source = Source.Of(record),
            };
        }
        
        Debug.Assert(cx.constExpression() is { Length: > 0 });
        return new ArrayConstExpr
        {
            Expessions = new ConstExprList
            {
                Items = cx.constExpression().Select(VisitConstExpression).ToList(),
                Source = Source.Of(cx.constExpression()),
            },
            Source = Source.Of(cx.constExpression()),
        };
    }

    public override RecordElementConstExpr VisitRecordConstExpression(DelphiParser.RecordConstExpressionContext cx)
    {
        return new RecordElementConstExpr
        {
            Ident = VisitIdent(cx.ident()),
            Expr = VisitConstExpression(cx.constExpression()),
            Source = Source.Of(cx),
        };
    }
}

public abstract class ConstExpr() : DelphiNode(DelphiNodeKind.ConstExpr)
{
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitConstExpr(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitConstExpr(this);
}

public sealed class SimpleConstExpr : ConstExpr
{
    public required Expr Expr { get; init; }
    
    public override IEnumerable<DelphiNode> Children => [Expr];
}

public sealed class ArrayConstExpr : ConstExpr
{
    public required ConstExprList Expessions { get; init; }

    public override IEnumerable<DelphiNode> Children => [Expessions];
}

public sealed class ConstExprList() :
    DelphiCollection<
        ConstExprList,
        ConstExpr
    >(DelphiNodeKind.ConstExpr)
{
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitConstExprList(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitConstExprList(this);
}

public sealed class RecordConstExpr : ConstExpr
{
    public required RecordElementConstExprList Elems { get; init; }

    public override IEnumerable<DelphiNode> Children => [Elems];
}

public sealed class RecordElementConstExprList() :
    DelphiCollection<
        RecordElementConstExprList,
        RecordElementConstExpr
    >(DelphiNodeKind.RecordConstElement)
{
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitRecordElementConstExprList(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitRecordElementConstExprList(this);
}

public sealed class RecordElementConstExpr : ConstExpr
{
    public required Ident Ident { get; init; }
    public required ConstExpr Expr { get; init; }

    public override IEnumerable<DelphiNode> Children => [Ident, Expr];
}
