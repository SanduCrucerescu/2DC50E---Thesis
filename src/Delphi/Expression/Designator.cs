using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override Designator VisitDesignator(DelphiParser.DesignatorContext cx)
    {
        Designator inherited = cx.INHERITED() is not null 
            ? new InheritedDesignator() 
            : new NilDesignator();
        Designator ident = cx.typeId() is { } tid
            ? new IdentDesignator { Ident = VisitTypeId(tid), Source = Source.Of(tid) }
            : new NilDesignator();

        return cx.designatorItem().Aggregate(
            inherited.Chain(ident), 
            (curr, item) => curr.Chain(VisitDesignatorItem(item)
        ));
    }

    public override Designator VisitDesignatorItem(DelphiParser.DesignatorItemContext cx)
    {
        if (cx.POINTER2() is not null) return new DerefDesignator();
        if (cx.AT2() is not null) return new AddressOfDesignator();
        
        if (cx.genericTypeIdent() is { Length: > 0 } generics)
        {
            return new GenericDesignator
            {
                Generic = new GenericIdentList
                {
                    Items = generics.Select(VisitGenericTypeIdent).ToList(),
                },
                Source = Source.Of(cx)
            };
        }

        if (cx.expressionList() is { } exprs) 
        {
            return new ArrayDesignator
            {
                Expressions = VisitExpressionList(exprs),
                Source = Source.Of(cx),
            };
        }
        
        Debug.Assert(
            cx.colonConstruct().Length == 0,
            "Colon Construct Not Supported for how"
        );
        
        return new CallDesignator
        {
            Expressions = new ExprList
            {
                Items = cx.expression().Select(VisitExpression).ToImmutableArray(),
                Source = Source.Of(cx.expression()),
            },
            Source = Source.Of(cx),
        };
    }
}

public abstract class Designator : Expr
{
    public virtual Designator Chain(Designator other)
    {
        return new ChainedDesignator
        {
            Left = this,
            Right = other,
            Source = Source + other.Source,
        };
    }

    public abstract List<Designator> Flattened { get; }
    public override IEnumerable<DelphiNode> Children => [];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitDesignator(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitDesignator(this);
}

public sealed class DesignatorList() :
    DelphiCollection<
        DesignatorList,
        Designator
    >(DelphiNodeKind.Designator)
{
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitDesignatorList(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitDesignatorList(this);
}

public sealed class NilDesignator : Designator
{
    public override Designator Chain(Designator other)
    {
        return other;
    }

    public override List<Designator> Flattened => [];
    public override IEnumerable<DelphiNode> Children => [];
}

public sealed class ChainedDesignator : Designator
{
    public required Designator Left { get; init; }
    public required Designator Right { get; init; }

    public override Designator Chain(Designator other)
    {
        return new ChainedDesignator
        {
            Left = Left.Chain(Right),
            Right = other,
            Source = Source + other.Source,
        };
    }

    public override List<Designator> Flattened => [..Left.Flattened, ..Right.Flattened];
    public override IEnumerable<DelphiNode> Children => [Left, Right];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitChainedDesignator(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitChainedDesignator(this);
}

public sealed class InheritedDesignator : Designator
{
    public override List<Designator> Flattened => [this];
}

public sealed class IdentDesignator : Designator
{
    public required Ident Ident { get; init; }
    public override List<Designator> Flattened => [this];
    public override IEnumerable<DelphiNode> Children => [Ident];
}

public sealed class DerefDesignator : Designator
{
    public override List<Designator> Flattened => [this];
}

public sealed class AddressOfDesignator : Designator
{
    public override List<Designator> Flattened => [this];
}

public sealed class GenericDesignator : Designator
{
    public required GenericIdentList Generic { get; init; }
    public override List<Designator> Flattened => [this];
    public override IEnumerable<DelphiNode> Children => [Generic];
}

public sealed class ArrayDesignator : Designator
{
    public required ExprList Expressions { get; init; }
    public override List<Designator> Flattened => [this];
    public override IEnumerable<DelphiNode> Children => [Expressions];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitDesignator(this);
}

public sealed class CallDesignator : Designator
{
    public required ExprList Expressions { get; init; }
    public override List<Designator> Flattened => [this];
    public override IEnumerable<DelphiNode> Children => [Expressions];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitCallDesignator(this);
}

