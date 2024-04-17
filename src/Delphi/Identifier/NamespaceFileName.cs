using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override NamespaceFileName VisitNamespaceFileName(DelphiParser.NamespaceFileNameContext cx)
    {
        return new NamespaceFileName
        {
            Namespace = VisitNamespaceName(cx.namespaceName()),
            FileName = cx.QuotedString() != null
                ? new SimpleIdent(cx.QuotedString().GetText())
                : null,
            Source = Source.Of(cx),
        };
    }

    public override NamespaceFileNameList VisitNamespaceFileNameList(DelphiParser.NamespaceFileNameListContext cx)
    {
        return new NamespaceFileNameList
        {
            Items = cx.namespaceFileName().Select(VisitNamespaceFileName).ToImmutableArray(),
            Source = Source.Of(cx)
        };
    }
}

public sealed class NamespaceFileName : Ident, IEquatable<NamespaceFileName>
{
    public required Namespace Namespace { get; init; }
    public required SimpleIdent? FileName { get; init; }

    public override IEnumerable<DelphiNode> Children => FileName != null ? [Namespace, FileName] : [Namespace];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitNamespaceFileName(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitNamespaceFileName(this);
    
    public bool Equals(NamespaceFileName? other)
    {
        if (ReferenceEquals(null, other)) return false;
        if (ReferenceEquals(this, other)) return true;
        return base.Equals(other) && Namespace.Equals(other.Namespace) && Equals(FileName, other.FileName);
    }

    public override bool Equals(object? obj)
    {
        return ReferenceEquals(this, obj) || obj is NamespaceFileName other && Equals(other);
    }

    public override int GetHashCode()
    {
        return HashCode.Combine(base.GetHashCode(), Namespace, FileName);
    }

    public static bool operator ==(NamespaceFileName? left, NamespaceFileName? right)
    {
        return Equals(left, right);
    }

    public static bool operator !=(NamespaceFileName? left, NamespaceFileName? right)
    {
        return !Equals(left, right);
    }
}

public sealed class NamespaceFileNameList() : 
    ImmutableDelphiCollection<
        NamespaceFileNameList,
        NamespaceFileName
    >(DelphiNodeKind.NamespaceFileName)
{
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitNamespaceFileNameList(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitNamespaceFileNameList(this);
}



