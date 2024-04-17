using System.Collections.Generic;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override ConstDecl VisitConstDeclaration(DelphiParser.ConstDeclarationContext cx)
    {
        return new ConstDecl
        {
            Ident = VisitIdent(cx.ident()),
            Type = cx.typeExpr() != null ? VisitTypeExpr(cx.typeExpr()) : new InferTypeExpr(),
            Expr = VisitConstExpression(cx.constExpression()),
            Source = Source.Of(cx),
        };
    }
}

public sealed class ConstDecl : Decl
{
    public required SimpleIdent Ident;
    public required TypeExpr Type;
    public required ConstExpr Expr;

    public override IEnumerable<DelphiNode> Children => [Ident, Type, Expr];
    public override void Accept(Visitor<DelphiNode, DelphiNodeKind> visitor)
    {
        throw new System.NotImplementedException();
    }

    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitConstDecl(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitConstDecl(this);
}

