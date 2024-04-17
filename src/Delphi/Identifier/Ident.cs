using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics.CodeAnalysis;
using System.Linq;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override SimpleIdent VisitIdent(DelphiParser.IdentContext cx)
    {
        var src = Source.Of(cx);
        return new SimpleIdent
        {
            Text = cx.usedKeywordsAsNames() != null 
                ? cx.usedKeywordsAsNames().GetText() 
                : cx.TkIdentifier().GetText(),
            Source = src,
        };
    }

    public override SimpleIdentList VisitIdentList(DelphiParser.IdentListContext cx)
    {
        return new SimpleIdentList
        {
            Items = cx.ident().Select(VisitIdent).ToImmutableArray(),
            Source = Source.Of(cx),
        };
    }
}

public abstract class Ident() : DelphiNode(DelphiNodeKind.Ident);

public sealed class IdentList() :
    ImmutableDelphiCollection<
        IdentList,
        Ident
    >(DelphiNodeKind.Ident)
{
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitIdentList(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitIdentList(this);
}

public sealed class SimpleIdent() : Ident
{
    public required string Text { get; init; }

    [SetsRequiredMembers]
    public SimpleIdent(string text) : this() => Text = text;

    protected override bool IsLeaf => true;
    public override IEnumerable<DelphiNode> Children => [];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitSimpleIdent(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitSimpleIdent(this);
}

public sealed class SimpleIdentList() :
    ImmutableDelphiCollection<
        SimpleIdentList,
        SimpleIdent
    >(DelphiNodeKind.Ident)
{
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitSimpleIdentList(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitSimpleIdentList(this);
}

