using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;

namespace DelphiCSharp.Cs;


public sealed class MethodModifier() : CsNode(CsNodeKind.MethodModifier), IEquatable<MethodModifier>
{
    public required MethodModifierKind ModKind { get; init; }
    
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitMethodModifier(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitMethodModifier(this);
    public override IEnumerable<CsNode> Children => [];
    protected override bool IsLeaf => true;

    [SetsRequiredMembers]
    public MethodModifier(MethodModifierKind kind, Source source) : this()
    {
        ModKind = kind;
        Source = source;
    }

    public static MethodModifier Nil(Source source) => new(MethodModifierKind.None, source);
    public static MethodModifier Public(Source source) => new(MethodModifierKind.Public, source);
    public static MethodModifier Private(Source source) => new(MethodModifierKind.Private, source);
    public static MethodModifier Protected(Source source) => new(MethodModifierKind.Protected, source);
    public static MethodModifier Internal(Source source) => new(MethodModifierKind.Internal, source);
    public static MethodModifier Sealed(Source source) => new(MethodModifierKind.Sealed, source);
    public static MethodModifier Static(Source source) => new(MethodModifierKind.Static, source);
    public static MethodModifier Override(Source source) => new(MethodModifierKind.Override, source);
    public static MethodModifier Virtual(Source source) => new(MethodModifierKind.Virtual, source);
    public static MethodModifier Abstract(Source source) => new(MethodModifierKind.Abstract, source);
    public static MethodModifier Inline(Source source) => new(MethodModifierKind.Inline, source);
    public static MethodModifier Unsafe(Source source) => new(MethodModifierKind.Unsafe, source);
    public static MethodModifier Dynamic(Source source) => new(MethodModifierKind.Dynamic, source);
    public static MethodModifier New(Source source) => new(MethodModifierKind.New, source);
    
    public bool Equals(MethodModifier? other)
    {
        if (ReferenceEquals(null, other)) return false;
        if (ReferenceEquals(this, other)) return true;
        return base.Equals(other) && ModKind == other.ModKind;
    }

    public override bool Equals(object? obj)
    {
        return ReferenceEquals(this, obj) || obj is MethodModifier other && Equals(other);
    }

    public override int GetHashCode()
    {
        return HashCode.Combine(base.GetHashCode(), (int)ModKind);
    }

    public static bool operator ==(MethodModifier? left, MethodModifier? right)
    {
        return Equals(left, right);
    }

    public static bool operator !=(MethodModifier? left, MethodModifier? right)
    {
        return !Equals(left, right);
    }
}

public enum MethodModifierKind
{
    None,
    Public,
    Private,
    Protected,
    Internal,
    Sealed,
    Static,
    Virtual,
    Override,
    Inline,
    Unsafe,
    Abstract,
    Dynamic,
    New,
}

public sealed class MethodModifierSet() :
    CsSet<
        MethodModifierSet,
        MethodModifier
    >(CsNodeKind.FieldModifier)
{
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitMethodModifierList(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitMethodModifierList(this);
}
