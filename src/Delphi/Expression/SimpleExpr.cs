using System.Diagnostics;
using System.Linq;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override Expr VisitSimpleExpression(DelphiParser.SimpleExpressionContext cx)
    {
        Debug.Assert(cx.factor().Length > 0 && cx.children.Count > 0, "Must be at least one factor");

        var expr = VisitFactor(cx.GetChild<DelphiParser.FactorContext>(0));
        
        if (cx.factor().Length > 1)
        {
            var len = cx.factor().Length;
            for (var ix = 0; ix < len-1; ix++)
            {
                var op = VisitOperator(cx.GetChild<DelphiParser.OperatorContext>(ix));
                var ex = VisitFactor(cx.GetChild<DelphiParser.FactorContext>(ix+1));
                expr = expr.With(op, ex);
            }
        }

        return expr;
    }
}



