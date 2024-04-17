using System.Collections.Generic;

namespace DelphiCSharp.Cs;

public partial class DelphiWalker
{
    public override ArrayCreationExpr VisitArrayConstExpr(Delphi.ArrayConstExpr cx)
    {
        return new ArrayCreationExpr
        {
            Expressions = VisitConstExprList(cx.Expessions),
            Source = cx.Source
        };
    }
    
    public override ObjectCreationExpr VisitRecordConstExpr(Delphi.RecordConstExpr record)
    {
        return new ObjectCreationExpr
        {
            Source = record.Source,
        };
    }

}


public abstract class PrimaryExpr : Expr
{
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitPrimaryExpr(this);
}

public sealed class ObjectCreationExpr : PrimaryExpr
{
    public override IEnumerable<CsNode> Children => [];
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitObjectCreationExpr(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitObjectCreationExpr(this);
}

public sealed class ArrayCreationExpr : Expr
{
    public required ExprList Expressions { get; init; }
    
    public override IEnumerable<CsNode> Children => [Expressions];
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitArrayCreationExpr(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitArrayCreationExpr(this);
}

public sealed class ElementAccessExpr : Expr
{
    public required Expr AccessedElement { get; init; }
    public required ExprList Expressions { get; init; }
    public override IEnumerable<CsNode> Children => [Expressions];
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitElementAccessExpr(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitElementAccessExpr(this);
}

public sealed class InvocationExpr : Expr
{
    public required Expr Expr { get; init; }
    public required ExprList Args { get; init; }


    public override IEnumerable<CsNode> Children => [Expr, Args];
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitInvocationExpr(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitInvocationExpr(this);
    
}

public sealed class MemberAccessExpr : Expr
{
    public required Expr Expr { get; init; }
    public required SimpleSymbolExpr Member { get; init; }
    public override IEnumerable<CsNode> Children => [Expr, Member];
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitMemberAccessExpr(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitMemberAccessExpr(this);
}