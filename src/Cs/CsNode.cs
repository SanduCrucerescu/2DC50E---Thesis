
using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Linq;

namespace DelphiCSharp.Cs;

public abstract class CsNode(CsNodeKind kind) :
    Node<CsNode, CsNodeKind>(kind), 
    ICollectionKind<CsNodeKind>
{
    public static CsNodeKind CollectionKind => CsNodeKind.Collection;

    public override T Accept<T>(Visitor<CsNode, CsNodeKind, T> visitor)
    {
        return Accept((CsVisitor<T>)visitor);
    }

    public override void Accept(Visitor<CsNode, CsNodeKind> visitor)
    {
        Accept((CsVisitor)visitor);
    }

    protected abstract T Accept<T>(CsVisitor<T> visitor);
    protected abstract void Accept(CsVisitor visitor);
    public abstract override IEnumerable<CsNode> Children { get; }
}

public abstract class CsSet<TCol, TVal>(CsNodeKind itemKind)
    : CsNode(CsNodeKind.Set),
        IEquatable<CsSet<TCol, TVal>>,
        IEnumerable<TVal>
    where TCol : CsSet<TCol, TVal>, new()
    where TVal : CsNode
{
    public readonly CsNodeKind ItemKind = itemKind;
    public HashSet<TVal> Items { get; init; } = [];
    public override IEnumerable<TVal> Children => Items;
    public int Count => Items.Count;
    
    public static TCol From(IEnumerable<TVal> enumerable)
    {
        return new TCol
        {
            Items = enumerable.ToHashSet(),
        };
    }
    
    public bool Add(TVal val) => Items.Add(val);

    public bool AddRange(TCol other)
    {
        var all = true;
        foreach (var val in other)
        {
            if (Add(val) is false) all = false;
        }

        return all;
    }
    
    public bool Equals(CsSet<TCol, TVal>? other)
    {
        if (ReferenceEquals(null, other)) return false;
        if (ReferenceEquals(this, other)) return true;
        return base.Equals(other) && ItemKind == other.ItemKind && Items.Equals(other.Items);
    }

    public override bool Equals(object? obj)
    {
        if (ReferenceEquals(null, obj)) return false;
        if (ReferenceEquals(this, obj)) return true;
        if (obj.GetType() != this.GetType()) return false;
        return Equals((CsSet<TCol, TVal>)obj);
    }

    public override int GetHashCode()
    {
        return HashCode.Combine(base.GetHashCode(), (int)ItemKind, Items);
    }

    public static bool operator ==(CsSet<TCol, TVal>? left, CsSet<TCol, TVal>? right)
    {
        return Equals(left, right);
    }

    public static bool operator !=(CsSet<TCol, TVal>? left, CsSet<TCol, TVal>? right)
    {
        return !Equals(left, right);
    }

    public IEnumerator<TVal> GetEnumerator()
    {
        return Items.GetEnumerator();
    }

    IEnumerator IEnumerable.GetEnumerator()
    {
        return GetEnumerator();
    }
}

public abstract class CsCollection<TCol, TVal>(CsNodeKind itemKind) 
    : CsNode(CsNodeKind.Collection), 
        IEquatable<CsCollection<TCol, TVal>>,
        IEnumerable<TVal>
    where TCol : CsCollection<TCol, TVal>, new()
    where TVal : CsNode
{
    public readonly CsNodeKind ItemKind = itemKind;
    public List<TVal> Items { get; init; } = [];
    public int Length => Items.Count;
    public override IEnumerable<TVal> Children => Items;
    
    public void Add(TVal val) => Items.Add(val);

    public void AddRange(TCol other)
    {
        Items.AddRange(other.Items);
    }

    public static TCol From(IEnumerable<TVal> enumerable)
    {
        return new TCol
        {
            Items = enumerable.ToList(),
        };
    }

    public TCol ForEach(Action<TVal> action)
    {
        foreach (var item in Items)
        {
            action(item);
        }

        return (TCol)this;
    }


    public bool Equals(CsCollection<TCol, TVal>? other)
    {
        if (ReferenceEquals(null, other)) return false;
        if (ReferenceEquals(this, other)) return true;
        return base.Equals(other) && ItemKind == other.ItemKind && Items.SequenceEqual(other.Items);
    }


    public override bool Equals(object? obj)
    {
        if (ReferenceEquals(null, obj)) return false;
        if (ReferenceEquals(this, obj)) return true;
        if (obj.GetType() != this.GetType()) return false;
        return Equals((CsCollection<TCol, TVal>)obj);
    }

    public override int GetHashCode()
    {
        return HashCode.Combine(base.GetHashCode(), (int)ItemKind, Items);
    }

    public static bool operator ==(CsCollection<TCol, TVal>? left, CsCollection<TCol, TVal>? right)
    {
        return Equals(left, right);
    }

    public static bool operator !=(CsCollection<TCol, TVal>? left, CsCollection<TCol, TVal>? right)
    {
        return !Equals(left, right);
    }
    
    public IEnumerator<TVal> GetEnumerator()
    {
        return Items.GetEnumerator();
    }
    
    IEnumerator IEnumerable.GetEnumerator()
    {
        return GetEnumerator();
    }
}


public enum CsNodeKind
{
    Scope,
    Expr,
    ConstExpr,
    Statement,
    Type,
    Name,
    SimpleName,
    Operator,
    
    Decl,
    TypeDecl,
    MethodDecl,
    VarDecl,
    MemberDecl,
    EnumMemberDecl,
    InterfaceMemberDecl,
    PropertyDecl,
    ClassFieldDecl,
    
    Modifier,
    FieldModifier,
    MethodModifier,
    ParamModifier,
    
    UsingSymbolStatement,
    CatchHandler,
    
    Generic,
    Constraint,
    GenericSeparated,
    
    FuncHead,
    FuncParam,
    
    Collection,
    Set,
    TranslationHelper,
}

