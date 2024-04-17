using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Linq;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override ConstrainedGeneric VisitConstrainedGeneric(DelphiParser.ConstrainedGenericContext cx)
    {
        return new ConstrainedGeneric
        {
            Ident = VisitIdent(cx.ident()),
            Constraints = new GenericConstraintsList
            {
                Items = cx.genericConstraint()
                    .Select(VisitGenericConstraint)
                    .ToList(),
                Source = Source.Of(cx.genericConstraint())
            },
            Source = Source.Of(cx),
        };
    }

    public override GenericConstraint VisitGenericConstraint(DelphiParser.GenericConstraintContext cx)
    {
        if (cx.ident() != null)
        {
            return new GenericIdentConstraint
            {
                Ident = VisitIdent(cx.ident()),
                Source = Source.Of(cx),
            };
        }

        return cx.RECORD() != null
            ? new GenericRecordConstraint { Source = Source.Of(cx) }
            : cx.CONSTRUCTOR() != null
                ? new GenericConstructorConstraint { Source = Source.Of(cx) }
                : new GenericClassConstraint { Source = Source.Of(cx) };
    }
}

public abstract class Generic() : DelphiNode(DelphiNodeKind.Generic);

public sealed class SimpleGeneric : Generic
{
    public required SimpleIdent Ident { get; init; }

    public override IEnumerable<DelphiNode> Children => [Ident];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitSimpleGeneric(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitSimpleGeneric(this);
}


public sealed class ConstrainedGeneric : Generic
{
    public required SimpleIdent Ident { get; init; }
    public required GenericConstraintsList Constraints { get; init; }

    public override IEnumerable<DelphiNode> Children => [Ident, Constraints];

    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitConstrainedGeneric(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitConstrainedGeneric(this);
}

public abstract class GenericConstraint() : DelphiNode(DelphiNodeKind.GenericConstraint)
{
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitGenericConstraint(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitGenericConstraint(this);
    public override IEnumerable<DelphiNode> Children => [];
    protected override bool IsLeaf => true;
}
public sealed class GenericClassConstraint : GenericConstraint;
public sealed class GenericRecordConstraint : GenericConstraint;
public sealed class GenericConstructorConstraint : GenericConstraint;
public sealed class GenericIdentConstraint : GenericConstraint
{
    public required SimpleIdent Ident { get; init; }
}

public sealed class GenericConstraintsList() :
    DelphiCollection<
        GenericConstraintsList,
        GenericConstraint
    >(DelphiNodeKind.GenericConstraint)
{
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitGenericConstraintsList(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitGenericConstraintsList(this);
}
