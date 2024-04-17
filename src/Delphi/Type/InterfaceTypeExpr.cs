using System;
using System.Collections.Generic;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override InterfaceTypeExpr VisitInterfaceTypeDecl(DelphiParser.InterfaceTypeDeclContext cx)
    {
        if (cx.interfaceKey().DISPINTERFACE() is not null)
        {
            throw new NotImplementedException("Not Supported for now");
        }

        var (methods, props) = (MethodDeclSection.Nil, PropertyDeclSection.Nil);

        foreach (var item in cx.interfaceItem())
        {
            var child = VisitInterfaceItem(item);
            if (child is MethodDecl method) methods.Add(method);
            else if (child is PropertyDecl prop) props.Add(prop);
            else throw new ArgumentOutOfRangeException(nameof(child));
        }

        return new InterfaceTypeExpr
        {
            Methods = methods,
            Properties = props,
            Source = Source.Of(cx),
        };
    }

    public override Decl VisitInterfaceItem(DelphiParser.InterfaceItemContext cx)
    {
        if (cx.classMethod() is { } method) return VisitClassMethod(method);
        if (cx.classProperty() is { } prop) return VisitClassProperty(prop);
        throw new ArgumentOutOfRangeException(nameof(cx));
    }
}


public sealed class InterfaceTypeExpr : StructuredTypeExpr
{
    public required MethodDeclSection Methods { get; init; }
    public required PropertyDeclSection Properties { get; init; }

    public override IEnumerable<DelphiNode> Children => [Methods, Properties];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitInterfaceType(this);
}