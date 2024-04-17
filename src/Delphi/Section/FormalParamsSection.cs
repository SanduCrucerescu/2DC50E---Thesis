using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override FormalParamsSection VisitFormalParameterSection(DelphiParser.FormalParameterSectionContext cx)
    {
        return cx.formalParameterList() is { } list
            ? VisitFormalParameterList(list)
            : [];
    }

    public override FormalParamsSection VisitFormalParameterList(DelphiParser.FormalParameterListContext cx)
    {
        return new FormalParamsSection
        {
            Items = cx.formalParameter().Select(fp => new FormalParam
            {
                Modifier = fp.parmType() is { } pt
                    ? pt.GetText() switch
                    {
                        "const" => new ConstModifier(),
                        "var" => new VarModifier(),
                        "out" => new OutModifier(),
                        _ => throw new ArgumentOutOfRangeException(nameof(pt))
                    }
                    : null,
                Names = VisitIdentList(fp.identList()),
                Type = fp.typeExpr() is { } t 
                    ? VisitTypeExpr(t)
                    : new InferTypeExpr(),
                Default = fp.expression() is { } expr 
                    ? VisitExpression(expr)
                    : null,
                Source = Source.Of(fp)
            }).ToImmutableArray(),
            Source = Source.Of(cx)
        };
    }

    public override FormalParam VisitFormalParameter(DelphiParser.FormalParameterContext cx)
    {
        return new FormalParam
        {
            Modifier = cx.parmType() is { } pt
                ? pt.GetText() switch
                {
                    "const" => ParamModifier.Const,
                    "var" => ParamModifier.Var,
                    "out" => ParamModifier.Out,
                    _ => throw new ArgumentOutOfRangeException(nameof(pt))
                }
                : null,
            Names = VisitIdentList(cx.identList()),
            Type = cx.typeExpr() is { } t 
                ? VisitTypeExpr(t)
                : new InferTypeExpr(),
            Default = cx.expression() is { } expr 
                ? VisitExpression(expr)
                : null,
            Source = Source.Of(cx),
        };
    }
}

public sealed class FormalParam() : DelphiNode(DelphiNodeKind.FormalParam)
{
    public required ParamModifier? Modifier { get; init; }
    public required TypeExpr Type { get; init; }
    public required SimpleIdentList Names { get; init; }
    public required Expr? Default { get; init; }

    public override IEnumerable<DelphiNode> Children =>
        Default is not null
            ? Modifier is not null
                ? [Modifier, Type, Names, Default]
                : [Type, Names, Default]
            : [Type, Names];
    
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitFormalParam(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitFormalParam(this);
}

public abstract class ParamModifier() : DelphiNode(DelphiNodeKind.FormalParamModifier)
{
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitParamModifier(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitParamModifier(this);
    public override IEnumerable<DelphiNode> Children => [];
    protected override bool IsLeaf => true;

    public static readonly ConstModifier Const = new();
    public static readonly VarModifier Var = new();
    public static readonly OutModifier Out = new();
}
public sealed class ConstModifier : ParamModifier;
public sealed class VarModifier : ParamModifier;
public sealed class OutModifier : ParamModifier;


public sealed class FormalParamsSection() :
    ImmutableDelphiCollection<
        FormalParamsSection,
        FormalParam
    >(DelphiNodeKind.FormalParam)
{
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitFormalParamsSection(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitFormalParamsSection(this);
}
