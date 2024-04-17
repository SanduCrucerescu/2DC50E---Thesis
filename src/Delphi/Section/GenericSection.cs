using System.Collections.Immutable;
using System.Linq;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override GenericsList VisitGenericDefinition(DelphiParser.GenericDefinitionContext cx)
    {
        if (cx.simpleGenericDefinition() != null)
        {
            return new GenericsList
            {
                Items = cx.simpleGenericDefinition()
                    .ident()
                    .Select(generic => (Generic)new SimpleGeneric
                    {
                        Ident = VisitIdent(generic),
                    })
                    .ToImmutableArray(),
                Source = Source.Of(cx),
            };
        }
        
        return new GenericsList
        {
            Items = cx.constrainedGenericDefinition()
                .constrainedGeneric()
                .Select(generic => (Generic)VisitConstrainedGeneric(generic))
                .ToImmutableArray(),
            Source = Source.Of(cx)
        };
    }
    
}

public sealed class GenericsList() :
    ImmutableDelphiCollection<
        GenericsList,
        Generic
    >(DelphiNodeKind.Generic)
{
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitGenericsList(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitGenericsList(this);
}


