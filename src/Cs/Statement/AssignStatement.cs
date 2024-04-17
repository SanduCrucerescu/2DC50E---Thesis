using System.Collections.Generic;
using Antlr4.Build.Tasks;

namespace DelphiCSharp.Cs;

public partial class DelphiWalker
{
    
}

public sealed class AssignStatement : Statement
{
    public required Expr Target { get; init; }
    public required Expr Value { get; init; }

    public override IEnumerable<CsNode> Children => [Target, Value];
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitAssignStatement(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitAssignStatement(this);
}

