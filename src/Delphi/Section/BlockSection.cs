using System.Collections.Generic;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override BlockSection VisitBlock(DelphiParser.BlockContext cx)
    {
        var block = new BlockSection
        {
            Decls = VisitDeclSection(cx.declSection()),
            Body = cx.blockBody() is { } body
                ? VisitBlockBody(body)
                : new CompoundStatement
                {
                    Statements = [],
                    Source = Source.Nil,
                },
            Source = Source.Of(cx)
        };
        
        return block;
    }

}

public sealed class BlockSection() : DelphiNode(DelphiNodeKind.Block)
{
    public required DeclSection Decls { get; init; }
    public required Statement Body { get; init; }

    public static BlockSection Nil => new()
    {
        Decls = DeclSection.Nil,
        Body = new CompoundStatement
        {
            Statements = [],
            Source = Source.Nil
        },
        Source = Source.Nil,
    };
    public override IEnumerable<DelphiNode> Children => [Decls, Body];

    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitBlock(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitBlock(this);
}

