using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics.CodeAnalysis;
using System.Linq;

namespace DelphiCSharp.Delphi;

public abstract class DelphiNode(DelphiNodeKind kind) :
    Node<DelphiNode, DelphiNodeKind>(kind), 
    ICollectionKind<DelphiNodeKind>
{
    public static DelphiNodeKind CollectionKind => DelphiNodeKind.Collection;

    public override T Accept<T>(Visitor<DelphiNode, DelphiNodeKind, T> visitor)
    {
        return Accept((DelphiVisitor<T>)visitor);
    }

    public override void Accept(Visitor<DelphiNode, DelphiNodeKind> visitor)
    {
        Accept((DelphiVisitor)visitor);
    }

    protected abstract T Accept<T>(DelphiVisitor<T> visitor);
    protected abstract void Accept(DelphiVisitor visitor);
    
    public abstract override IEnumerable<DelphiNode> Children { get; }
}


public abstract class ImmutableDelphiCollection<TCol, TVal>(DelphiNodeKind itemKind)
    : DelphiNode(DelphiNodeKind.Collection),
        IEquatable<ImmutableDelphiCollection<TCol, TVal>>,
        IEnumerable<TVal>
    where TCol : ImmutableDelphiCollection<TCol, TVal>, new()
    where TVal : DelphiNode
{
    public readonly DelphiNodeKind ItemKind = itemKind;
    public ImmutableArray<TVal> Items { get; init; } = ImmutableArray<TVal>.Empty;
    public override IEnumerable<TVal> Children => Items;

    public int Length => Items.Length;
    
    public override int GetHashCode()
    {
        return HashCode.Combine(base.GetHashCode(), (int)ItemKind, Source, Items);
    }

    public IEnumerator<TVal> GetEnumerator()
    {
        return ((IEnumerable<TVal>)Items).GetEnumerator();
    }

    IEnumerator IEnumerable.GetEnumerator()
    {
        return GetEnumerator();
    }

    public bool Equals(ImmutableDelphiCollection<TCol, TVal>? other)
    {
        if (ReferenceEquals(null, other)) return false;
        if (ReferenceEquals(this, other)) return true;
        return ItemKind == other.ItemKind && Items.SequenceEqual(other.Items);
    }

    public override bool Equals(object? obj)
    {
        if (ReferenceEquals(null, obj)) return false;
        if (ReferenceEquals(this, obj)) return true;
        if (obj.GetType() != this.GetType()) return false;
        return Equals((ImmutableDelphiCollection<TCol, TVal>)obj);
    }
}


public abstract class DelphiCollection<TCol, TVal>(DelphiNodeKind itemKind) 
    : DelphiNode(DelphiNodeKind.Collection), 
        IEquatable<DelphiCollection<TCol, TVal>>,
        IEnumerable<TVal>
    where TCol : DelphiCollection<TCol, TVal>, new()
    where TVal : DelphiNode
{
    public new SpanList Source = [];
    public readonly DelphiNodeKind ItemKind = itemKind;
    public List<TVal> Items { get; init; } = [];
    public override IEnumerable<TVal> Children => Items;
    
    public void Add(TVal val) => Items.Add(val);

    public void AddRange(TCol other)
    {
        Items.AddRange(other.Items);
        Source.Add(other.Source);
    }

    public static TCol Nil => new()
    {
        Items = [],
        Source = [],
    };
    
    public static TCol From(IEnumerable<TVal> enumerable)
    {
        var csNodes = enumerable.ToList();
        return new TCol
        {
            Items = csNodes,
            Source = SpanList.From(csNodes.Select(n => n.Source))
        };
    }


    public bool Equals(DelphiCollection<TCol, TVal>? other)
    {
        if (ReferenceEquals(null, other)) return false;
        if (ReferenceEquals(this, other)) return true;
        return SyntaxKind == other.SyntaxKind && ItemKind == other.ItemKind && Items.SequenceEqual(other.Items);
    }

    public override bool Equals(object? obj)
    {
        if (ReferenceEquals(null, obj)) return false;
        if (ReferenceEquals(this, obj)) return true;
        if (obj.GetType() != GetType()) return false;
        return Equals((DelphiCollection<TCol, TVal>)obj);
    }

    public override int GetHashCode()
    {
        return HashCode.Combine(base.GetHashCode(), (int)ItemKind, Items);
    }

    public static bool operator ==(DelphiCollection<TCol, TVal>? left, DelphiCollection<TCol, TVal>? right)
    {
        return Equals(left, right);
    }

    public static bool operator !=(DelphiCollection<TCol, TVal>? left, DelphiCollection<TCol, TVal>? right)
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

public enum DelphiNodeKind
{
    File,
    Collection,
    Block,
    
    UnitHead,
    UnitBlock,
    UnitInterface,
    UnitImplementation,
    
    Decl,
    VarDecl,
    ConstDecl,
    TypeDecl,
    FuncDecl,
    MethodDecl,
    PropertyDecl,
    ClassFieldDecl,
    
    FuncHead,
    FuncDirective,
    
    MethodHead,
    MethodDirective,
    MethodKind,
    
    FormalParam,
    FormalParamModifier,
    
    RecordField,
    ReadWriteAccessor,
    RecordConstElement,
    RecordItemSection,
    RecordHelperItemSection,
    RecordVariantSection,
    Visibility,
    VisibilitySection,
    ClassState,
    ClassHelperItemSection,
    
    Ident,
    Namespace,
    NamespaceList,
    NamespaceFileName,
    NamespaceFileNameList,
    Generic,
    GenericIdent,
    GenericType,
    GenericTypeList,
    GenericConstraint,
    
    Statement,
    Expr,
    ConstExpr,
    Designator,
    Type,
    
    Operator,
    WithItem,
    TryHandler,
    EnumVariant,
    CaseArm,
    Label,
}