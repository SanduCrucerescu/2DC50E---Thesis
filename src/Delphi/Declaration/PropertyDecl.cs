using System.Collections.Generic;

namespace DelphiCSharp.Delphi;

public sealed class PropertyDecl : Decl
{
    public required bool IsClass { get; init; }
    public required SimpleIdent Ident { get; init; }
    public required Expr? Index { get; init; }
    public required FormalParamsSection Params { get; init; }
    public required GenericIdent Type { get; init; }
    public required ReadWriteAccessorList ReadWriteList { get; init; }

    public override IEnumerable<DelphiNode> Children =>
        Index is not null
            ? [Ident, Index, Params, Type, ReadWriteList]
            : [Ident, Params, Type, ReadWriteList];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitPropertyDecl(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitPropertyDecl(this);
}

public sealed class PropertyDeclSection() :
    DelphiCollection<
        PropertyDeclSection,
        PropertyDecl
    >(DelphiNodeKind.PropertyDecl)
{
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitPropertyDeclSection(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitPropertyDeclSection(this);
}

public abstract class ReadWriteAccessor() : DelphiNode(DelphiNodeKind.ReadWriteAccessor)
{
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitReadWriteAccessor(this);
}

public sealed class ReadWriteAccessorList() :
    DelphiCollection<
        ReadWriteAccessorList,
        ReadWriteAccessor
    >(DelphiNodeKind.ReadWriteAccessor)
{
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitReadWriteAccessorList(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitReadWriteAccessorList(this);
}

public sealed class ReadProperty : ReadWriteAccessor
{
    public required SimpleIdent Ident { get; init; }
    public required Expr? Expr { get; init; }

    public override IEnumerable<DelphiNode> Children => Expr is not null ? [Ident, Expr] : [Ident];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitReadProperty(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitReadProperty(this);
}

public sealed class WriteProperty() : ReadWriteAccessor
{
    public required Ident Ident { get; init; }
    public required Expr? Expr { get; init; }

    public override IEnumerable<DelphiNode> Children => Expr is not null ? [Ident, Expr] : [Ident];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitWriteProperty(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitWriteProperty(this);
}
