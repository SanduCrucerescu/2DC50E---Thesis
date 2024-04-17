using System;
using System.Collections.Generic;
using System.Linq;
using Antlr4.Runtime;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override StructuredTypeExpr VisitClassDecl(DelphiParser.ClassDeclContext cx)
    {
        if (cx.classTypeDecl() is { } typeDecl) return VisitClassTypeDecl(typeDecl);
        if (cx.classTypeTypeDecl() is { } type) return VisitClassTypeTypeDecl(type);
        if (cx.interfaceTypeDecl() is { } interfaceDecl) return VisitInterfaceTypeDecl(interfaceDecl);
        if (cx.recordDecl() is { } recordDecl) return VisitRecordDecl(recordDecl);
        if (cx.recordHelperDecl() is { } recordHelper) return VisitRecordHelperDecl(recordHelper);
        if (cx.classHelperDecl() is { } classHelper) return VisitClassHelperDecl(classHelper);
        if (cx.objectDecl() is { } obj) return VisitObjectDecl(obj);

        throw new ArgumentOutOfRangeException(nameof(cx));
    }

    public override ClassOfTypeExpr VisitClassTypeTypeDecl(DelphiParser.ClassTypeTypeDeclContext cx)
    {
        return new ClassOfTypeExpr
        {
            Type = VisitTypeId(cx.typeId()),
            Source = Source.Of(cx),
        };
    }

    public override ClassTypeExpr VisitClassTypeDecl(DelphiParser.ClassTypeDeclContext cx)
    {
        var members = VisibilitySectionList.Nil;

        if (cx.classItem() is not { Length: > 0 } items) 
        {
            return new ClassTypeExpr
            {
                State = cx.classState() is { } state 
                    ? VisitClassState(state)
                    : null,
                Parents = cx.classParent() is { } parents 
                    ? VisitClassParent(parents)
                    : [],
                Members = [],
                Source = Source.Of(cx),
            };
        }
        
        var section = VisibilitySection.Nil(Visibility.Published(Source.Conjured));
        foreach (var item in items)
        {
            var classItem = VisitClassItem(item);
            if (classItem is Visibility vis)
            {
                members.Add(section);
                section = VisibilitySection.Nil(vis);
                section.Visibility = vis;
                continue;
            }

            switch (classItem)
            {
                case MethodDecl method: section.Methods.Add(method); break;
                case PropertyDecl prop: section.Properties.Add(prop); break;
                case ConstDeclSection consts: section.Consts.AddRange(consts); break;
                case TypeDeclSection types: section.Types.AddRange(types); break;
                case ClassFieldDecl field: section.Fields.Add(field); break;
                case VarDeclSection vars: section.Vars.AddRange(vars); break;
                case ClassVarDeclSection classVars: section.ClassVars.AddRange(classVars); break;
                default: 
                    classItem.GetType().Print("CLASS ITEM");
                    throw new ArgumentOutOfRangeException(nameof(classItem), "Should never happen");
            }
        }
        members.Add(section);

        return new ClassTypeExpr
        {
            State = cx.classState() is { } _state
                ? VisitClassState(_state)
                : null,
            Parents = cx.classParent() is { } _parents 
                ? VisitClassParent(_parents)
                : [],
            Members = members,
            Source = Source.Of(cx),
        };
    }

    public override ClassHelperTypeExpr VisitClassHelperDecl(DelphiParser.ClassHelperDeclContext cx)
    {
        return new ClassHelperTypeExpr
        {
            Parents = VisitClassParent(cx.classParent()),
            For = VisitTypeId(cx.typeId()),
            Items = new ClassHelperItemSectionList
            {
                
            },
            Source = Source.Of(cx),
        };
    }

    public override StructuredTypeExpr VisitObjectDecl(DelphiParser.ObjectDeclContext context)
    {
        throw new NotImplementedException("Not Supported for now");
    }

    public override DelphiNode VisitClassItem(DelphiParser.ClassItemContext cx)
    {
        if (cx.visibility() is { } vis) return VisitVisibility(vis);
        if (cx.classMethod() is { } method) return VisitClassMethod(method);
        if (cx.classProperty() is { } prop) return VisitClassProperty(prop);
        if (cx.constSection() is { } consts) return VisitConstSection(consts);
        if (cx.typeSection() is { } types) return VisitTypeSection(types);
        if (cx.classField() is { } field) return VisitClassField(field);
        if (cx.varSection() is { } vars)
        {
            var section = VisitVarSection(vars);
            if (cx.CLASS() is not null)
            {
                return new ClassVarDeclSection
                {
                    Items = section.Items,
                    Source = [Source.Of(cx)],
                };
            }
            
            return section;
        }

        throw new ArgumentOutOfRangeException(nameof(cx));
    }

    public override DelphiNode VisitClassHelperItem(DelphiParser.ClassHelperItemContext cx)
    {
        if (cx.visibility() is { } vis) return VisitVisibility(vis);
        if (cx.classMethod() is { } method) return VisitClassMethod(method);
        if (cx.classProperty() is { } prop) return VisitClassProperty(prop);
        if (cx.varSection() is { } section)
        {
            var vars = VisitVarSection(section);
            if (cx.CLASS() is not null)
            {
                return new ClassVarDeclSection
                {
                    Items = vars.Items,
                    Source = [Source.Of(cx)],
                };
            }

            return vars;
        }

        throw new ArgumentOutOfRangeException(nameof(cx));
    }

    public override ClassState VisitClassState(DelphiParser.ClassStateContext cx)
    {
        if (cx.SEALED() is not null) return new SealedClassState();
        if (cx.ABSTRACT() is not null) return new AbstractClassState();
        throw new ArgumentOutOfRangeException(nameof(cx));
    }

    public override GenericTypeExprList VisitClassParent(DelphiParser.ClassParentContext cx)
    {
        return GenericTypeExprList.From(cx.genericType().Select(VisitGenericType));
    }

    public override MethodDecl VisitClassMethod(DelphiParser.ClassMethodContext cx)
    {
        var kind = cx.FUNCTION() is not null
            ? MethodKind.Function(Source.Of(cx.FUNCTION().Symbol))
            : cx.OPERATOR() is not null
                ? MethodKind.Operator(Source.Of(cx.OPERATOR().Symbol))
                : VisitMethodKey(cx.methodKey());
        
        var directives = kind.Kind is EMethodKind.Operator
            ? MethodDirectiveList.Nil
            : new MethodDirectiveList
            {
                Items = cx.methodDirective().Select(VisitMethodDirective).ToList(),
                Source = Source.Of(cx.methodDirective()),
            };

        var name = VisitGenericTypeIdent(cx.genericTypeIdent());
        return new MethodDecl
        {
            Head = new MethodHead
            {
                Name = new MethodName
                {
                    Segments = [name],
                    Source = name.Source,
                },
                Signature = new MethodSignature
                {
                    Params = cx.formalParameterSection() is { } parms 
                        ? VisitFormalParameterSection(parms)
                        : [],
                    MethodKind = kind,
                    ReturnType = kind.Kind switch
                    {
                        EMethodKind.Constructor or EMethodKind.Operator or EMethodKind.Destructor => null,
                        EMethodKind.Function => VisitTypeExpr(cx.typeExpr()),
                        EMethodKind.Procedure => new VoidTypeExpr(),
                        _ => throw new ArgumentOutOfRangeException(nameof(kind))
                    },
                    Directives = directives,
                    Source = cx.formalParameterSection() is {} fps ? Source.Of(fps) : Source.Nil
                         + kind.Source 
                         + directives.Source 
                         + (cx.typeExpr() is { } t ? Source.Of(t) : Source.Nil),
                },
                Source = Source.Of(cx),
            },
            Block = BlockSection.Nil,
            Source = Source.Of(cx),
        };
    }

    public override PropertyDecl VisitClassProperty(DelphiParser.ClassPropertyContext cx)
    {
        return new PropertyDecl
        {
            IsClass = cx.CLASS() is not null,
            Ident = VisitIdent(cx.ident()),
            Params = cx.classPropertyArray() is { } arr
                ? VisitFormalParameterList(arr.formalParameterList())
                : [],
            Type = VisitGenericTypeIdent(cx.genericTypeIdent()),
            Index = cx.classPropertyIndex() is { } ix 
                ? VisitExpression(ix.expression())
                : null,
            ReadWriteList = new ReadWriteAccessorList
            {
                Items = cx.classPropertyReadWrite().Select(VisitClassPropertyReadWrite).ToList(),
                Source = Source.Of(cx.classPropertyReadWrite()),
            },
            Source = Source.Of(cx)
        };
    }

    public override ReadWriteAccessor VisitClassPropertyReadWrite(DelphiParser.ClassPropertyReadWriteContext cx)
    {
        var ident = VisitIdent(cx.ident());
        var expr = VisitExpression(cx.expression());
        return cx.READ() is not null
            ? new ReadProperty
            {
                Ident = ident,
                Expr = expr,
                Source = Source.Of(cx),
            }
            : new WriteProperty
            {
                Ident = ident,
                Expr = expr,
                Source = Source.Of(cx)
            };
    }
}

public sealed class ClassOfTypeExpr : StructuredTypeExpr
{
    public required TypeId Type { get; init; }

    public override IEnumerable<DelphiNode> Children => [Type];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitClassOfType(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitClassOfType(this);
}

public sealed class ClassTypeExpr : StructuredTypeExpr
{
    public required ClassState? State { get; init; }
    public required GenericTypeExprList Parents { get; init; }
    public required VisibilitySectionList Members { get; init; }

    public override IEnumerable<DelphiNode> Children =>
        State is not null
            ? [State, Parents, Members]
            : [Parents, Members];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitClassType(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitClassType(this);
}

public sealed class ClassHelperTypeExpr : StructuredTypeExpr
{
    public required GenericTypeExprList Parents { get; init; }
    public required TypeId For { get; init; }
    public required ClassHelperItemSectionList Items { get; init; }

    public override IEnumerable<DelphiNode> Children => [Parents, For, Items];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitClassHelperType(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitClassHelperType(this);
}

public sealed class ClassHelperItemSection() : DelphiNode(DelphiNodeKind.ClassHelperItemSection)
{
    public required Visibility Visibility { get; init; }
    public required MethodDeclSection Methods { get; init; }
    public required PropertyDeclSection Properties { get; init; }
    public required VarDeclSection Vars { get; init; }
    public required ClassVarDeclSection ClassVars { get; init; }

    public override IEnumerable<DelphiNode> Children => [Visibility, Methods, Properties, Vars, ClassVars];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitClassHelperItemSection(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitClassHelperItemSection(this);
}

public sealed class ClassHelperItemSectionList() :
    DelphiCollection<
        ClassHelperItemSectionList,
        ClassHelperItemSection
    >(DelphiNodeKind.ClassHelperItemSection)
{
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitClassHelperItemSectionList(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitClassHelperItemSectionList(this);
}

public abstract class ClassState() : DelphiNode(DelphiNodeKind.ClassState)
{
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitClassState(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitClassState(this);
    public override IEnumerable<DelphiNode> Children => [];
    protected override bool IsLeaf => true;
}

public sealed class SealedClassState : ClassState;
public sealed class AbstractClassState : ClassState;
public sealed class NilClassState : ClassState;