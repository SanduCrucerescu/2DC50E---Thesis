using System.Collections.Generic;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override Program VisitProgram(DelphiParser.ProgramContext cx)
    {
        return new Program
        {
            Namespace = VisitNamespaceName(cx.programHead().namespaceName()),
            Uses = VisitUsesFileClause(cx.usesFileClause()),
            Block = cx.block() is { } block 
                ? VisitBlock(block)
                : BlockSection.Nil,
            Source = Source.Of(cx)
        };
    }
}

public sealed class Program() : File
{
    public required Namespace Namespace { get; init; }
    public required UsesFileSection Uses { get; init; }
    public required BlockSection Block { get; init; }

    public override IEnumerable<DelphiNode> Children => [Namespace, Uses, Block];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitProgram(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitProgram(this);
}
