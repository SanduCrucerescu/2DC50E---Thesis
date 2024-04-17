using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override FuncDecl VisitProcDecl(DelphiParser.ProcDeclContext cx)
    {
        var body = cx.procBody() is { } procBody
            ? VisitProcBody(procBody)
            : BlockSection.Nil;
        return new FuncDecl
        {
            Head = VisitProcDeclHeading(cx.procDeclHeading()),
            LocalVars = body.Decls.Vars,
            LocalConsts = body.Decls.Consts,
            LocalFuncs = body.Decls.Funcs,
            Body = body.Body,
            Source = Source.Of(cx)
        };
    }
    
    public override FuncHead VisitProcDeclHeading(DelphiParser.ProcDeclHeadingContext cx)
    {
        return new FuncHead
        {
            Name = VisitIdent(cx.ident()),
            Signature = new FuncSignature
            {
                Params = cx.formalParameterSection() is { } section
                    ? VisitFormalParameterSection(section)
                    : [],
                ReturnTypeExpr = cx.typeExpr() is {} typeExpr 
                    ? VisitTypeExpr(typeExpr) 
                    : new VoidTypeExpr(),
                Directives = new FuncDirectiveList
                {
                    Items = cx.functionDirective()
                        .Select(VisitFunctionDirective)
                        .ToList(),
                    Source = Source.Of(cx.functionDirective())
                }
            },
            Source = Source.Of(cx)
        };
    }

    public override BlockSection VisitProcBody(DelphiParser.ProcBodyContext cx)
    {
        if (cx.FORWARD() is not null) throw new NotImplementedException();
        if (cx.EXTERNAL() is not null) throw new NotImplementedException();
        Debug.Assert(cx.block() is not null, "Nothing else supported for now");
        return VisitBlock(cx.block());
    }
}


public sealed class FuncDecl() : DelphiNode(DelphiNodeKind.FuncDecl)
{
    public required FuncHead Head { get; init; }
    public required VarDeclSection LocalVars { get; init; }

    public required ConstDeclSection LocalConsts { get; init; }
    public required FuncDeclSection LocalFuncs { get; init; }
    public required Statement Body { get; init; }

    public override IEnumerable<DelphiNode> Children => [Head, LocalVars, LocalConsts, Body];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitFuncDecl(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitFuncDecl(this);
}

public sealed class FuncDeclSection() :
    DelphiCollection<
        FuncDeclSection,
        FuncDecl
    >(DelphiNodeKind.FuncDecl)
{
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitFuncDeclSection(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitFuncDeclSection(this);
}

public sealed class FuncHead() : DelphiNode(DelphiNodeKind.FuncHead)
{
    public required SimpleIdent Name { get; init; }
    public required FuncSignature Signature { get; init; }

    public override IEnumerable<DelphiNode> Children => [Name, Signature];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitFuncHead(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitFuncHead(this);
}

public sealed class FuncSignature() : DelphiNode(DelphiNodeKind.FuncHead)
{
    public required FormalParamsSection Params { get; init; }
    public required TypeExpr ReturnTypeExpr { get; init; }
    public required FuncDirectiveList Directives { get; init; }

    public override IEnumerable<DelphiNode> Children => [Params, ReturnTypeExpr, Directives];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitFuncSignature(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitFuncSignature(this);
}

