
using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Xml;
using DelphiCSharp.Delphi;

namespace DelphiCSharp.Cs;

public partial class DelphiWalker
{
    public override Expr VisitDesignator(Delphi.Designator designator)
    {
        return designator switch
        {
            AddressOfDesignator addrOf => throw new NotImplementedException(),
            ArrayDesignator array => VisitArrayDesignator(array),
            CallDesignator call => VisitCallDesignator(call),
            ChainedDesignator chained => VisitChainedDesignator(chained),
            DerefDesignator deref => throw new NotImplementedException(),
            GenericDesignator generic => throw new NotImplementedException(),
            IdentDesignator ident => VisitIdentDesignator(ident),
            InheritedDesignator inh => new SymbolExpr
            {
                Symbol = new SimpleSymbol("base", Source.Conjured),
                Source = designator.Source,
            },
            NilDesignator nil => throw new NotImplementedException(),
            _ => throw new ArgumentOutOfRangeException(nameof(designator))
        };
    }
    
    public override Node<CsNode, CsNodeKind> VisitDesignatorList(Delphi.DesignatorList designator)
    {
        throw new System.NotImplementedException();
    }

    public override SymbolExpr VisitIdentDesignator(IdentDesignator cx)
    {
        return new SymbolExpr
        {
            Symbol = VisitIdent(cx.Ident),
            Source = cx.Source,
        };
    }

    public override Expr VisitChainedDesignator(Delphi.ChainedDesignator cx)
    {
        var flattened = cx.Flattened;
        flattened.Reverse();
        var stack = new Stack<Designator>(flattened);
        Expr? curr = null;
        while (stack.TryPop(out var des))
        {
            Debug.Assert(des is not ChainedDesignator);
            curr = des switch
            {
                AddressOfDesignator addressOfDesignator => throw new NotImplementedException(),
                ArrayDesignator bracketed => curr is null
                    ? new ArrayCreationExpr
                    {
                        Expressions = VisitExprList(bracketed.Expressions),
                        Source = bracketed.Source,
                    }
                    : new ElementAccessExpr
                    {
                        AccessedElement = curr,
                        Expressions = VisitExprList(bracketed.Expressions),
                        Source = bracketed.Source,
                    },
                CallDesignator call => curr is null
                ? throw new NotImplementedException()
                : new InvocationExpr
                {
                    Expr = curr,
                    Args = VisitExprList(call.Expressions),
                    Source = call.Source,
                },
                DerefDesignator derefDesignator => throw new NotImplementedException(),
                GenericDesignator genericDesignator => throw new NotImplementedException(),
                IdentDesignator ident =>  VisitMemberAccessExpr(curr, ident),
                InheritedDesignator inherited => new SymbolExpr
                {
                    Symbol = new SimpleSymbol("base", inherited.Source),
                    Source = inherited.Source,
                },
                _ => throw new ArgumentOutOfRangeException(nameof(des))
            };
        }

        Debug.Assert(curr is not null);
        return curr;
    }

    public Expr VisitMemberAccessExpr(Expr? curr, IdentDesignator ident)
    {
        if (curr is null)
        {
            return VisitIdentDesignator(ident);
        }

        var sym = VisitIdent(ident.Ident);

        if (sym is QualifiedSymbol qualified)
        {
            if (qualified.Segments.Length == 0)
            {
                return new MemberAccessExpr
                {
                    Expr = curr,
                    Member = new SimpleSymbolExpr
                    {
                        Symbol = qualified.Symbol,
                        Source = qualified.Source,
                    },
                    Source = curr.Source + qualified.Symbol.Source,
                };
            }

            var first = new MemberAccessExpr
            {
                Expr = curr,
                Member = new SimpleSymbolExpr
                {
                    Symbol = qualified.Segments.Items[0],
                    Source = qualified.Segments.Items[0].Source,
                },
                Source = curr.Source,
            };

            if (qualified.Segments.Length > 0)
            {
                foreach (var segment in qualified.Segments.Items[1..])
                {
                    first = new MemberAccessExpr
                    {
                        Expr = first,
                        Member = new SimpleSymbolExpr
                        {
                            Symbol = segment,
                            Source = segment.Source,
                        },
                        Source = first.Source + segment.Source,
                    };
                }
            }

            return new MemberAccessExpr
            {
                Expr = first,
                Member = new SimpleSymbolExpr
                {
                    Symbol = qualified.Symbol,
                    Source = qualified.Symbol.Source,
                },
                Source = qualified.Source,
            };
        }

        if (sym is GenericTypeSymbol generic)
        {
            if (generic.Name.Segments.Length == 0)
            {
                return new MemberAccessExpr
                {
                    Expr = curr,
                    Member = new SimpleSymbolExpr
                    {
                        Symbol = generic.Name.Symbol,
                        Source = generic.Name.Symbol.Source,
                    },
                    Source = generic.Source,
                };
            }
            
            var first = new MemberAccessExpr
            {
                Expr = curr,
                Member = new SimpleSymbolExpr
                {
                    Symbol = generic.Name.Segments.Items[0],
                    Source = generic.Name.Segments.Items[0].Source,
                },
                Source = curr.Source,
            };

            if (generic.Name.Segments.Length > 0)
            {
                foreach (var segment in generic.Name.Segments.Items[1..])
                {
                    first = new MemberAccessExpr
                    {
                        Expr = first,
                        Member = new SimpleSymbolExpr
                        {
                            Symbol = segment,
                            Source = segment.Source,
                        },
                        Source = first.Source + segment.Source,
                    };
                }
            }

            return new MemberAccessExpr
            {
                Expr = first,
                Member = new SimpleSymbolExpr
                {
                    Symbol = generic.Name.Symbol,
                    Source = generic.Name.Symbol.Source,
                },
                Source = generic.Source,
            };
        }

        Debug.Assert(sym is SimpleSymbol);
        
        return new MemberAccessExpr
        {
            Expr = curr,
            Member = new SimpleSymbolExpr
            {
                Symbol = (SimpleSymbol)sym,
                Source = sym.Source,
            },
            Source = curr.Source + sym.Source,
        };
        
    }
    
    public override CallExpr VisitCallDesignator(CallDesignator cx)
    {
        return new CallExpr
        {
            Expressions = VisitExprList(cx.Expressions),
            Source = cx.Source,
        };
    }

    public override ArrayCreationExpr VisitArrayDesignator(ArrayDesignator cx)
    {
        return new ArrayCreationExpr
        {
            Expressions = VisitExprList(cx.Expressions),
            Source = cx.Source,
        };
    }
}


public sealed class ChainedExpr : Expr
{
    public required Expr Left { get; init; }
    public required Expr Right { get; init; }

    public override IEnumerable<CsNode> Children => [Left, Right];
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitExpr(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitExpr(this);
}

public sealed class CallExpr : Expr
{
    public required ExprList Expressions { get; init; }

    public override IEnumerable<CsNode> Children => [Expressions];
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitCallExpr(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitCallExpr(this);
}

public sealed class SymbolExpr : Expr
{
    public required Symbol Symbol { get; init; }

    public override IEnumerable<CsNode> Children => [Symbol];
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitSymbolExpr(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitSymbolExpr(this);
}

public sealed class SimpleSymbolExpr : Expr
{
    public required SimpleSymbol Symbol { get; init; }
    
    public override IEnumerable<CsNode> Children => [Symbol];
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitSimpleSymbolExpr(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitSimpleSymbolExpr(this);
}