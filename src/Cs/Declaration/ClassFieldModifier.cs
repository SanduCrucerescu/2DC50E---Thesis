using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;

namespace DelphiCSharp.Cs;

public abstract class FieldModifier() : CsNode(CsNodeKind.MethodModifier)
{
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitFieldModifier(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitFieldModifier(this);
    public override IEnumerable<CsNode> Children => [];
    protected override bool IsLeaf => true;

    public FieldModifier(Source source) : this()
    {
        Source = source;
    }

    public static PublicFieldModifier Public(Source source) => new(source);
    public static PrivateFieldModifier Private(Source source) => new(source);
    public static ProtectedFieldModifier Protected(Source source) => new(source);
    public static InternalFieldModifier Internal(Source source) => new(source);
    public static ConstFieldModifier Const(Source source) => new(source);
    public static ReadonlyFieldModifier Readonly(Source source) => new(source);
    public static SealedFieldModifier Sealed(Source source) => new(source);
    public static StaticFieldModifier Static(Source source) => new(source);
    public static OverrideFieldModifier Override(Source source) => new(source);

}
public sealed class FieldModifierList() :
    CsCollection<
        FieldModifierList,
        FieldModifier
    >(CsNodeKind.FieldModifier)
{
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitFieldModifierList(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitFieldModifierList(this);
}

public sealed class PublicFieldModifier(Source source) : FieldModifier(source);
public sealed class PrivateFieldModifier(Source source) : FieldModifier(source);
public sealed class ProtectedFieldModifier(Source source) : FieldModifier(source);
public sealed class InternalFieldModifier(Source source) : FieldModifier(source);
public sealed class ConstFieldModifier(Source source) : FieldModifier(source);
public sealed class ReadonlyFieldModifier(Source source) : FieldModifier(source);
public sealed class SealedFieldModifier(Source source) : FieldModifier(source);
public sealed class StaticFieldModifier(Source source) : FieldModifier(source);
public sealed class VirtualFieldModifier(Source source) : FieldModifier(source);
public sealed class OverrideFieldModifier(Source source) : FieldModifier(source);
