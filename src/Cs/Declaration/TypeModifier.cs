using System.Collections.Generic;
using DelphiCSharp.Delphi;

namespace DelphiCSharp.Cs;

public abstract class TypeModifier() : CsNode(CsNodeKind.Modifier)
{
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitModifier(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitModifier(this);
    public override IEnumerable<CsNode> Children => [];
    protected override bool IsLeaf => true;

    public static TModifier Of<TModifier>(Source source) where TModifier : TypeModifier, new() => new()
    {
        Source = source,
    };

    public static NewTypeModifier New(Source source) => Of<NewTypeModifier>(source);
    public static PublicTypeModifier Public(Source source) => Of<PublicTypeModifier>(source);
    public static PrivateTypeModifier Private(Source source) => Of<PrivateTypeModifier>(source);
    public static InternalTypeModifier Internal(Source source) => Of<InternalTypeModifier>(source);
    public static ProtectedTypeModifier Protected(Source source) => Of<ProtectedTypeModifier>(source);
    public static StaticTypeModifier Static(Source source) => Of<StaticTypeModifier>(source);
    public static SealedTypeModifier Sealed(Source source) => Of<SealedTypeModifier>(source);
    public static AbstractTypeModifier Abstract(Source source) => Of<AbstractTypeModifier>(source);
}

public sealed class NewTypeModifier : TypeModifier;
public sealed class PublicTypeModifier : TypeModifier;
public sealed class PrivateTypeModifier : TypeModifier;
public sealed class InternalTypeModifier : TypeModifier;
public sealed class ProtectedTypeModifier : TypeModifier;
public sealed class StaticTypeModifier : TypeModifier;
public sealed class SealedTypeModifier : TypeModifier;
public sealed class AbstractTypeModifier : TypeModifier;
public sealed class UnsafeTypeModifier : TypeModifier;

public sealed class TypeModifierList() :
    CsCollection<
        TypeModifierList,
        TypeModifier
    >(CsNodeKind.Modifier)
{
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitTypeModifierList(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitTypeModifierList(this);
}
