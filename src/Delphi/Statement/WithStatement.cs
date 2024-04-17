using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override WithStatement VisitWithStatement(DelphiParser.WithStatementContext cx)
    {
        return new WithStatement
        {
            With = VisitWithItem(cx.withItem()),
            Statement = VisitStatement(cx.statement()),
            Source = Source.Of(cx),
        };
    }

    public override WithItem VisitWithItem(DelphiParser.WithItemContext cx)
    {
        if (cx.AS() is not null)
        {
            return new WithAsItem
            {
                With = VisitDesignator(cx.designator().First()),
                As = VisitDesignator(cx.designator().Last()),
                Source = Source.Of(cx),
            };
        }

        return new WithList
        {
            Designators = new DesignatorList
            {
                Items = cx.designator().Select(VisitDesignator).ToList(),
                Source = Source.Of(cx.designator()),
            },
            Source = Source.Of(cx),
        };
    }
}

public sealed class WithStatement : Statement
{
    public required WithItem With { get; init; }
    public required Statement Statement { get; init; }

    public override IEnumerable<DelphiNode> Children => [With, Statement];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitWithStatement(this);
}

public abstract class WithItem() : DelphiNode(DelphiNodeKind.WithItem);

public sealed class WithAsItem : WithItem
{
    public required Designator With { get; init; }
    public required Designator As { get; init; }

    public override IEnumerable<DelphiNode> Children => [With, As];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitWithAsItem(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitWithAsItem(this);
}

public sealed class WithList : WithItem
{
    public required DesignatorList Designators { get; init; }
    
    public override IEnumerable<DelphiNode> Children => [Designators];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitWithList(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitWithList(this);
}