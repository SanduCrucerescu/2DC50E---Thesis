using System;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;
using System.Text;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override Expr VisitFactor(DelphiParser.FactorContext cx)
    {

        Operator? op = null;
        if (cx.NOT() is not null) op = Operator.Not(Source.Of(cx));
        if (cx.PLUS() is not null) op = Operator.Plus(Source.Of(cx));
        if (cx.MINUS() is not null) op = Operator.Minus(Source.Of(cx));
        if (cx.POINTER2() is not null) op = Operator.Deref(Source.Of(cx));
        if (cx.AT2() is not null) op = Operator.At(Source.Of(cx));

        if (op is { } someOp)
        {
            return new UnaryExpr
            {
                Value = VisitFactor(cx.factor()),
                Op = someOp,
                Source = Source.Of(cx),
            };
        }

        (LitKind? litKind, object ? literal) = (null, null);
        if (cx.TRUE() is not null) (litKind, literal) = (LitKind.True, true);
        if (cx.FALSE() is not null) (litKind, literal) = (LitKind.False, false);
        if (cx.NIL() is not null) (litKind, literal) = (LitKind.Nil, null);
        if (cx.intNum() is { } num)
        {
            if (num.TkIntNum() is { } i) return VisitTkIntNum(i);
            return VisitTkHexNum(num.TkHexNum());
        }
        if (cx.realNum() is { } real) (litKind, literal) = (LitKind.F64, double.Parse(real.GetText()));
        
        if (litKind is { } someKind)
        {
            return new LitExpr
            {
                LitKind = someKind,
                Value = literal,
                Source = Source.Of(cx),
            };
        }

        if (cx.stringFactor() is { } sf) return VisitStringFactor(sf);
        if (cx.setSection() is { } ss) return VisitSetSection(ss);
        if (cx.designator() != null) return VisitDesignator(cx.designator());

        if (cx.LPAREN() is not null)
        {
            Debug.Assert(cx.POINTER2() is null && cx.DOT() is null, "Not implemented yet.");
            return new ParenthesizedExpr
            {
                Expr = VisitExpression(cx.expression().First()),
            };
        }

        if (cx.typeId() is { } tid)
        {
            return new IdentDesignator
            {
                Ident = VisitTypeId(tid),
                Source = Source.Of(tid),
            }.Chain(new CallDesignator
            {
                Expressions = new ExprList
                {
                    Items = cx.expression().Select(VisitExpression).ToImmutableArray(),
                    Source = Source.Of(cx.expression())
                }
            });
        }

        throw new ArgumentOutOfRangeException(nameof(cx));
    }

    public override Expr VisitStringFactor(DelphiParser.StringFactorContext cx)
    {
        var builder = new StringBuilder();
        foreach (var qs in cx.QuotedString())
        {
            builder.Append(qs);
        }

        return new LitExpr
        {
            LitKind = LitKind.String,
            Value = builder.ToString(),
            Source = Source.Of(cx),
        };
    }
}

