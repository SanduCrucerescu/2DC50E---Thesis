using System.Linq;

namespace DelphiCSharp;

public abstract class Visitor<TNode, TKind, TResult>
{
    public TResult Visit(Node<TNode, TKind> node)
    {
        return node.Accept(this);
    }
    
    public TResult VisitChildren(Node<TNode, TKind> node)
    {
        return node.Children
            .Select(child => child.Accept(this))
            .Aggregate(Default, AggregateResult);
    }

    public virtual TResult AggregateResult(TResult aggregate, TResult next) => next;
    public virtual TResult Default => default!;
}

public abstract class Visitor<TNode, TKind>
{
    public void Visit(Node<TNode, TKind> node)
    {
        node.Accept(this);
    }
    
    public void VisitChildren(Node<TNode, TKind> node)
    {
        foreach (var child in node.Children)
        {
            child.Accept(this);
        }
    }
}

