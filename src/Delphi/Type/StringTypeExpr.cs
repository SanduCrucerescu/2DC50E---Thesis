using System;
using System.Collections.Generic;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override TypeExpr VisitStringType(DelphiParser.StringTypeContext cx)
    {
        if (cx.expression() != null) return new SizedStringTypeExpr
        {
            Size = VisitExpression(cx.expression()),
            Source = Source.Of(cx),
        };

        if (cx.TYPE() != null)
        {
            if (cx.codePageNumber() != null) return new PagedAnsiStringTypeExpr
            {
                CodePage = VisitIntNum(cx.codePageNumber().intNum()),
                Source = Source.Of(cx),
            };
            return new UnpagedAnsiStringTypeExpr();
        }

        return new UnsizedStringTypeExpr();
    }
}

public sealed class UnsizedStringTypeExpr : TypeExpr;

public sealed class SizedStringTypeExpr : TypeExpr, IEquatable<SizedStringTypeExpr>
{
    public required Expr Size { get; init; }
    public override IEnumerable<DelphiNode> Children => [Size];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitSizedStringTypeExpr(this);
    public bool Equals(SizedStringTypeExpr? other)
    {
        if (ReferenceEquals(null, other)) return false;
        if (ReferenceEquals(this, other)) return true;
        return base.Equals(other) && Size.Equals(other.Size);
    }

    public override bool Equals(object? obj)
    {
        return ReferenceEquals(this, obj) || obj is SizedStringTypeExpr other && Equals(other);
    }

    public override int GetHashCode()
    {
        return HashCode.Combine(base.GetHashCode(), Size);
    }

    public static bool operator ==(SizedStringTypeExpr? left, SizedStringTypeExpr? right)
    {
        return Equals(left, right);
    }

    public static bool operator !=(SizedStringTypeExpr? left, SizedStringTypeExpr? right)
    {
        return !Equals(left, right);
    }

}
public sealed class UnpagedAnsiStringTypeExpr : TypeExpr;
public sealed class PagedAnsiStringTypeExpr : TypeExpr, IEquatable<PagedAnsiStringTypeExpr>
{
    public required LitExpr CodePage { get; init; }
    public override IEnumerable<DelphiNode> Children => [CodePage];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitPagedAnsiStringTypeExpr(this);
    public bool Equals(PagedAnsiStringTypeExpr? other)
    {
        if (ReferenceEquals(null, other)) return false;
        if (ReferenceEquals(this, other)) return true;
        return base.Equals(other) && CodePage.Equals(other.CodePage);
    }

    public override bool Equals(object? obj)
    {
        return ReferenceEquals(this, obj) || obj is PagedAnsiStringTypeExpr other && Equals(other);
    }

    public override int GetHashCode()
    {
        return HashCode.Combine(base.GetHashCode(), CodePage);
    }

    public static bool operator ==(PagedAnsiStringTypeExpr? left, PagedAnsiStringTypeExpr? right)
    {
        return Equals(left, right);
    }

    public static bool operator !=(PagedAnsiStringTypeExpr? left, PagedAnsiStringTypeExpr? right)
    {
        return !Equals(left, right);
    }
}
