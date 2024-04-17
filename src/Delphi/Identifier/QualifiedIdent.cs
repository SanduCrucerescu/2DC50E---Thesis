using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override QualifiedIdent VisitQualifiedIdent(DelphiParser.QualifiedIdentContext cx)
    {
        if (cx.ident().Length == 1)
        {
            return new QualifiedIdent
            {
                Segments = [],
                Ident = VisitIdent(cx.ident().First()),
                Source = Source.Of(cx),
            };
        }

        var parts = cx.ident().Select(VisitIdent).ToList();
        var segments = parts.GetRange(0, parts.Count - 1).ToImmutableArray();
        return new QualifiedIdent
        {
            Segments = new SimpleIdentList 
            {
                Items = segments,
                Source = segments.Aggregate((Source)Source.Nil, (src, ident) => src.With(ident.Source))
            },
            Ident = parts[^1],
            Source = Source.Of(cx),
        };
    }
}

public sealed class QualifiedIdent : Ident, IEquatable<QualifiedIdent>
{
    public required SimpleIdentList Segments { get; init; }
    public required SimpleIdent Ident { get; init; }

    public override IEnumerable<DelphiNode> Children => [Segments, Ident];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitQualifiedIdent(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitQualifiedIdent(this);
    
    public bool Equals(QualifiedIdent? other)
    {
        if (ReferenceEquals(null, other)) return false;
        if (ReferenceEquals(this, other)) return true;
        return base.Equals(other) && Segments.Equals(other.Segments) && Ident.Equals(other.Ident);
    }

    public override bool Equals(object? obj)
    {
        return ReferenceEquals(this, obj) || obj is QualifiedIdent other && Equals(other);
    }

    public override int GetHashCode()
    {
        return HashCode.Combine(base.GetHashCode(), Segments, Ident);
    }

    public static bool operator ==(QualifiedIdent? left, QualifiedIdent? right)
    {
        return Equals(left, right);
    }

    public static bool operator !=(QualifiedIdent? left, QualifiedIdent? right)
    {
        return !Equals(left, right);
    }
}

