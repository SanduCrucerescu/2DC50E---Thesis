using System;
using System.Collections.Generic;
using System.ComponentModel;

namespace DelphiCSharp;

public abstract class Collection<TCol, TVal, TNode, TKind>(TKind itemKind) 
    : Node<TNode, TKind>(TNode.CollectionKind)
    where TCol : Collection<TCol, TVal, TNode, TKind>, new()
    where TVal : Node<TNode, TKind>
    where TNode: ICollectionKind<TKind>
{
    public readonly TKind ItemKind = itemKind;
    public List<TVal> Items { get; init; } = [];
    public void Add(TVal val) => Items.Add(val);

    public void AddRange(TCol other)
    {
        Items.AddRange(other.Items);
    }

    public static TCol Nil => new()
    {
        Items = [],
    };

    public override IEnumerable<TVal> Children => Items;

    public override int GetHashCode()
    {
        return HashCode.Combine(ItemKind, Items);
    }

    public static bool operator ==(Collection<TCol, TVal, TNode, TKind>? left, Collection<TCol, TVal, TNode, TKind>? right)
    {
        return Equals(left, right);
    }

    public static bool operator !=(Collection<TCol, TVal, TNode, TKind>? left, Collection<TCol, TVal, TNode, TKind>? right)
    {
        return !Equals(left, right);
    }
    
    private bool Equals(Collection<TCol, TVal, TNode, TKind> other)
    {
        return EqualityComparer<TKind>.Default.Equals(ItemKind, other.ItemKind) && Items.Equals(other.Items);
    }

    public override bool Equals(object? obj)
    {
        if (ReferenceEquals(null, obj)) return false;
        if (ReferenceEquals(this, obj)) return true;
        if (obj.GetType() != this.GetType()) return false;
        return Equals((Collection<TCol, TVal, TNode, TKind>)obj);
    }
}

