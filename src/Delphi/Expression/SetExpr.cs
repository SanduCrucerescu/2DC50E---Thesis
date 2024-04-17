
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Data;
using System.Linq;
using System.Xml.Schema;
using Antlr4.Runtime.Tree;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override SetExpr VisitSetSection(DelphiParser.SetSectionContext cx)
    {
        var exprs = new List<Expr>();

        if (cx.expression().Length == 0)
        {
            return new SetExpr
            {
                Elems = [],
                Source = Source.Of(cx),
            };
        }
        
        for (var ix = 1; ix < cx.children.Count - 1; ix++)
        {
            var child = cx.GetChild(ix);
            if (child is DelphiParser.ExpressionContext exprCx)
            {
                if (string.IsNullOrEmpty(exprCx.GetText()))
                {
                    return new SetExpr
                    {
                        Elems = [],
                        Source = Source.Of(cx),
                    };
                }
                
                exprs.Add(VisitExpression(exprCx));
            }

            if (child is not TerminalNodeImpl terminal) continue;
            if (terminal.Symbol.Type == DelphiParser.DOTDOT)
            {
                var last = exprs[^1];
                exprs.RemoveAt(exprs.Count-1);
                var next = VisitExpression((DelphiParser.ExpressionContext)cx.children[ix + 1]);
                exprs.Add(new RangeExpr
                {
                    From = last,
                    To = next,
                    Source = last.Source + next.Source,
                });
                ix++;
            }
        }
        
        return new SetExpr
        {
            Elems = new ExprList
            {
                Items = exprs.ToImmutableArray(),
                Source = Source.Of(cx.expression()),
            },
            Source = Source.Of(cx),
        };
    }
}

public sealed class SetExpr : Expr
{
    public required ExprList Elems { get; init; }
    public override IEnumerable<DelphiNode> Children => [Elems];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitSetExpr(this);
}

