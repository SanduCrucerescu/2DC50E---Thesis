using System.Collections.Generic;

namespace DelphiCSharp.Cs;

public sealed class ReturnStatement : Statement
{
    public required Expr Value { get; init; }
    
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitReturnStatement(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitReturnStatement(this);

    public override IEnumerable<CsNode> Children => [Value];
}