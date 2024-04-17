using System.Collections.Generic;

namespace DelphiCSharp.Cs;

public partial class DelphiWalker
{
    public override SimpleStatement VisitSimpleStatement(Delphi.SimpleStatement statement)
    {
        return new SimpleStatement
        {
            Expr = VisitExpr(statement.Expr),
            Source = statement.Source,
        };
    }
    
}



public sealed class SimpleStatement : Statement
{
    public required Expr Expr { get; init; }
    
    public override IEnumerable<CsNode> Children => [Expr];
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitSimpleStatement(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitSimpleStatement(this);
}