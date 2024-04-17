using System;
using System.Collections.Generic;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override TypeExpr VisitProcedureType(DelphiParser.ProcedureTypeContext cx)
    {
        if (cx.methodType() is { } mt) return VisitMethodType(mt);
        if (cx.simpleProcedureType() is { } pt) return VisitSimpleProcedureType(pt);
        if (cx.procedureReference() is { } pr) return VisitProcedureReference(pr);
        throw new ArgumentOutOfRangeException(nameof(cx));
    }

    public override MethodTypeExpr VisitMethodType(DelphiParser.MethodTypeContext cx)
    {
        return new MethodTypeExpr
        {
            Signature = VisitProcedureTypeHeading(cx.procedureTypeHeading()),
            Source = Source.Of(cx),
        };
    }

    public override FuncTypeExpr VisitSimpleProcedureType(DelphiParser.SimpleProcedureTypeContext cx)
    {
        return new FuncTypeExpr
        {
            Signature = VisitProcedureTypeHeading(cx.procedureTypeHeading()),
            Source = Source.Of(cx),
        };
    }

    public override FuncRefTypeExpr VisitProcedureReference(DelphiParser.ProcedureReferenceContext cx)
    {
        return new FuncRefTypeExpr
        {
            Signature = VisitProcedureTypeHeading(cx.procedureTypeHeading()),
            Source = Source.Of(cx),
        };
    }

    public override FuncSignature VisitProcedureTypeHeading(DelphiParser.ProcedureTypeHeadingContext cx)
    {
        return new FuncSignature
        {
            Params = cx.formalParameterSection() is { } parms 
                ? VisitFormalParameterSection(parms)
                : [],
            ReturnTypeExpr = cx.typeExpr() != null ? VisitTypeExpr(cx.typeExpr()) : new VoidTypeExpr(),
            Directives = FuncDirectiveList.Nil,
            Source = Source.Of(cx),
        };
    }
}

public sealed class FuncTypeExpr : TypeExpr
{
    public required FuncSignature Signature { get; init; }
    public override IEnumerable<DelphiNode> Children => [Signature];
}

public sealed class MethodTypeExpr : TypeExpr
{
    public required FuncSignature Signature { get; init; }
    public override IEnumerable<DelphiNode> Children => [Signature];
}

public sealed class FuncRefTypeExpr : TypeExpr
{
    public required FuncSignature Signature { get; init; }
    public override IEnumerable<DelphiNode> Children => [Signature];
}
