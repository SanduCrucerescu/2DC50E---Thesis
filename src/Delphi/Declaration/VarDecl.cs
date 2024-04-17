using System.Collections.Generic;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override VarDecl VisitVarDeclaration(DelphiParser.VarDeclarationContext cx)
    {
        return new VarDecl
        {
            Idents = VisitIdentList(cx.identList()),
            Type = VisitTypeExpr(cx.typeExpr()),
            Expr = null,
            Source = Source.Of(cx)
        };
    }
}


public sealed class VarDecl : Decl
{
    public required SimpleIdentList Idents;
    public required TypeExpr Type;
    public required Expr? Expr;

    public override IEnumerable<DelphiNode> Children =>
        Expr is not null ? [Idents, Type, Expr] : [Idents, Type];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitVarDecl(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitVarDecl(this);
}

