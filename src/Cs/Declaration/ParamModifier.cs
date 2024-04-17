using System.Collections.Generic;

namespace DelphiCSharp.Cs;

public abstract class ParamModifier() : CsNode(CsNodeKind.ParamModifier)
{
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitParamModifier(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitParamModifier(this);
    protected override bool IsLeaf => true;
    public override IEnumerable<CsNode> Children => [];

    public static readonly InParamModifier In = new();
    public static readonly OutParamModifier Out = new();
    public static readonly RefParamModifier Ref = new();
}
public sealed class InParamModifier : ParamModifier;
public sealed class OutParamModifier : ParamModifier;
public sealed class RefParamModifier : ParamModifier;

