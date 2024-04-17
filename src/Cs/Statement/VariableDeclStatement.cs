using System.Collections.Generic;

namespace DelphiCSharp.Cs;

public sealed class VariableDeclStatement : Statement
{
    public required VarDecl Variable { get; init; }
    
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitVariableDeclStatement(this);

    protected override void Accept(CsVisitor visitor) => visitor.VisitVariableDeclStatement(this);

    public override IEnumerable<CsNode> Children => [Variable];
}