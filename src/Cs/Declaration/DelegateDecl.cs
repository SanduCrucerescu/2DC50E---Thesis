using System.Collections.Generic;
using System.Diagnostics;
using DelphiCSharp.Delphi;

namespace DelphiCSharp.Cs;

public partial class DelphiWalker
{
    public DelegateDecl VisitFuncTypeDecl(Delphi.TypeDecl decl)
    {
        Debug.Assert(decl.TypeExpr is FuncTypeExpr);
        var helper = (decl.TypeExpr as FuncTypeExpr)!;
        var separated = VisitGenericsList(decl.Generics);
        return new DelegateDecl
        {
            Name = VisitSimpleIdent(decl.Name),
            Modifiers = [],
            Params = VisitFormalParamsSection(helper.Signature.Params),
            TypeArguments = separated.Args,
            TypeConstraints = separated.Constraints,
            ReturnType = VisitTypeExpr(helper.Signature.ReturnTypeExpr),
            Source = decl.Source,
        };
    }
}

public sealed class DelegateDecl : TypeDecl
{
    public override required SimpleSymbol Name { get; init; }
    public override required TypeModifierList Modifiers { get; init; }
    public required MethodParamsList Params { get; init; }
    public required SimpleSymbolsList TypeArguments { get; init; }
    public required GenericsList TypeConstraints { get; init; }
    public required Type ReturnType { get; init; }

    public override IEnumerable<CsNode> Children => [Name, Modifiers, TypeArguments, TypeConstraints, ReturnType];
}

