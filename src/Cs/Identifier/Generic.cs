using System;
using System.Collections.Generic;
using System.Linq;
using DelphiCSharp.Delphi;

namespace DelphiCSharp.Cs;

public partial class DelphiWalker
{
    public override Generic VisitGeneric(Delphi.Generic generic)
    {
        return generic switch
        {
            ConstrainedGeneric c => VisitConstrainedGeneric(c),
            SimpleGeneric s => VisitSimpleGeneric(s),
            _ => throw new ArgumentOutOfRangeException(nameof(generic))
        };
    }

    public override GenericsSeparated VisitGenericsList(Delphi.GenericsList generics)
    {
        var symbols = SimpleSymbolsList.From([]);
        var constraints = GenericsList.From([]);
        foreach (var generic in generics)
        {
            if (generic is ConstrainedGeneric constrained)
            {
                var sym = VisitSimpleIdent(constrained.Ident);
                symbols.Add(sym);
                constraints.Add(new Generic
                {
                    Argument = sym,
                    Constraints = VisitGenericConstraintsList(constrained.Constraints),
                });
            }

            if (generic is SimpleGeneric simple)
            {
                symbols.Add(VisitSimpleIdent(simple.Ident));
            }
        }

        return new GenericsSeparated
        {
            Args = symbols,
            Constraints = constraints,
            Source = generics.Source,
        };
    }

    public override Generic VisitSimpleGeneric(SimpleGeneric simple)
    {
        return new Generic
        {
            Argument = VisitSimpleIdent(simple.Ident),
            Constraints = [],
            Source = simple.Source,
        };
    }

    public override Generic VisitConstrainedGeneric(ConstrainedGeneric constrained)
    {
        return new Generic
        {
            Argument = VisitSimpleIdent(constrained.Ident),
            Constraints = VisitGenericConstraintsList(constrained.Constraints),
            Source = constrained.Source,
        };
    }

    public override GenericConstraint VisitGenericConstraint(Delphi.GenericConstraint genericConstraint)
    {
        return genericConstraint switch
        {
            GenericClassConstraint => new ClassConstraint(),
            GenericRecordConstraint => new StructConstraint(),
            GenericConstructorConstraint => new NewConstraint(),
            GenericIdentConstraint ident => new TypeConstraint
            {
                Generic = new Generic
                {
                    Argument = VisitSimpleIdent(ident.Ident),
                    Constraints = [],
                    Source = ident.Ident.Source
                },
                Source = ident.Source,
            },
            _ => throw new ArgumentOutOfRangeException(nameof(genericConstraint))
        };
    }

    public override GenericConstraintsList VisitGenericConstraintsList(Delphi.GenericConstraintsList constraintsesList)
    {
        return new GenericConstraintsList
        {
            Items = constraintsesList.Items.Select(VisitGenericConstraint).ToList(),
            Source = constraintsesList.Source,
        };
    }
}

public sealed class Generic() : CsNode(CsNodeKind.Generic)
{
     public required SimpleSymbol Argument { get; init; }
     public required GenericConstraintsList Constraints { get; init; }

     public override IEnumerable<CsNode> Children => [Argument, Constraints];
     protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitGeneric(this);
     protected override void Accept(CsVisitor visitor) => visitor.VisitGeneric(this);
}



public sealed class GenericsList() :
    CsCollection<
        GenericsList,
        Generic
    >(CsNodeKind.Generic)
{
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitGenericsList(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitGenericsList(this);
}


public abstract class GenericConstraint() : CsNode(CsNodeKind.Constraint)
{
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitGenericConstraint(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitGenericConstraint(this);
     public override IEnumerable<CsNode> Children => [];
    protected override bool IsLeaf => true;
    
    public static readonly NewConstraint New = new();
    public static readonly ClassConstraint Class = new();
    public static readonly StructConstraint Struct = new();
    public static TypeConstraint Type(Generic generic) => new()
    {
        Generic = generic
    };
}

public sealed class NewConstraint : GenericConstraint;

public sealed class ClassConstraint : GenericConstraint;

public sealed class StructConstraint : GenericConstraint;

public sealed class TypeConstraint : GenericConstraint
{
    public required Generic Generic { get; init; }
}

public sealed class GenericsSeparated() : CsNode(CsNodeKind.GenericSeparated)
{
    public required SimpleSymbolsList Args { get; init; }
    public required GenericsList Constraints { get; init; }

     public override IEnumerable<CsNode> Children => [Args, Constraints];
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitGenericSeparated(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitGenericSeparated(this);
}
    
public sealed class GenericConstraintsList() :
    CsCollection<
        GenericConstraintsList,
        GenericConstraint
    >(CsNodeKind.Constraint)
{
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitGenericConstraintList(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitGenericConstraintList(this);
}