
using System;
using System.Collections.Generic;
using System.Linq;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override StructuredTypeExpr VisitRecordDecl(DelphiParser.RecordDeclContext cx)
    {
        if (cx.simpleRecord() is { } simple) return VisitSimpleRecord(simple);
        if (cx.variantRecord() is { } variant) return VisitVariantRecord(variant);
        throw new ArgumentOutOfRangeException(nameof(cx));
    }

    public override RecordHelperTypeExpr VisitRecordHelperDecl(DelphiParser.RecordHelperDeclContext cx)
    {
        var (methods, props) = (MethodDeclSection.Nil, PropertyDeclSection.Nil);

        foreach (var item in cx.recordHelperItem())
        {
            var child = VisitRecordHelperItem(item);
            if (child is MethodDecl method) methods.Add(method);
            else if (child is PropertyDecl prop) props.Add(prop);
            else throw new ArgumentOutOfRangeException(nameof(child));
        }
        
        return new RecordHelperTypeExpr
        {
            For = VisitTypeId(cx.typeId()),
            Items = new RecordHelperItemSection
            {
                Methods = methods,
                Properties = props,
                Source = methods.Source + props.Source,
            },
            Source = Source.Of(cx),
        };
    }
    
    public override VariantRecordTypeExpr VisitVariantRecord(DelphiParser.VariantRecordContext cx)
    {
        throw new NotImplementedException("Not Supported For now");
    }

    public override SimpleRecordTypeExpr VisitSimpleRecord(DelphiParser.SimpleRecordContext cx)
    {
        var section = RecordItemSection.Nil;
        foreach (var recordItem in cx.recordItem())
        {
            var item = VisitRecordItem(recordItem);
            
            if (item is MethodDecl method) section.Methods.Add(method);
            if (item is PropertyDecl prop) section.Properties.Add(prop);
            if (item is ConstDeclSection consts) section.Consts.AddRange(consts);
            if (item is TypeDeclSection types) section.Types.AddRange(types);
            if (item is RecordField field) section.Fields.Add(field);
            if (item is VarDeclSection vars) section.Vars.AddRange(vars);
            if (item is ClassVarDeclSection classVars) section.ClassVars.AddRange(classVars);
        }
        
        return new SimpleRecordTypeExpr
        {
            Fields = new RecordFieldList
            {
                Items = cx.recordField().Select(VisitRecordField).ToList(),
                Source = Source.Of(cx.recordField()),
            },
            Items = section,
            Source = Source.Of(cx),
        };
    }

    public override RecordField VisitRecordField(DelphiParser.RecordFieldContext cx)
    {
        return new RecordField
        {
            Idents = VisitIdentList(cx.identList()),
            Type = VisitTypeExpr(cx.typeExpr()),
            Source = Source.Of(cx),
        };
    }

    public override DelphiNode VisitRecordItem(DelphiParser.RecordItemContext cx)
    {
        if (cx.classMethod() is { } method) return VisitClassMethod(method);
        if (cx.classProperty() is { } prop) return VisitClassProperty(prop);
        if (cx.constSection() is { } consts) return VisitConstSection(consts);
        if (cx.typeSection() is { } types) return VisitTypeSection(types);
        if (cx.recordField() is { } field) return VisitRecordField(field);

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

    public override Decl VisitRecordHelperItem(DelphiParser.RecordHelperItemContext cx)
    {
        if (cx.classMethod() is { } method) return VisitClassMethod(method);
        if (cx.classProperty() is { } prop) return VisitClassProperty(prop);
        throw new ArgumentOutOfRangeException(nameof(cx));
    }
}

public sealed class RecordHelperTypeExpr : StructuredTypeExpr
{
    public required TypeId For { get; init; }
    public required RecordHelperItemSection Items { get; init; }

    public override IEnumerable<DelphiNode> Children => [For, Items];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitRecordHelper(this);
}

public sealed class RecordHelperItemSection() : DelphiNode(DelphiNodeKind.RecordHelperItemSection)
{
    public required MethodDeclSection Methods { get; init; }
    public required PropertyDeclSection Properties { get; init; }

    public override IEnumerable<DelphiNode> Children => [Methods, Properties];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitRecordHelperItemSection(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitRecordHelperItemSection(this);
}

public sealed class SimpleRecordTypeExpr : StructuredTypeExpr
{
    public required RecordFieldList Fields { get; init; }
    public required RecordItemSection Items { get; init; }

    public override IEnumerable<DelphiNode> Children => [Fields, Items];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitSimpleRecord(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitSimpleRecord(this);
}

public sealed class VariantRecordTypeExpr : StructuredTypeExpr
{
    public required RecordFieldList Fields { get; init; }
    public required RecordVariantSection Variants { get; init; }

    public override IEnumerable<DelphiNode> Children => [Fields, Variants];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitVariantRecord(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitVariantRecord(this);
}

public sealed class RecordVariantSection() : DelphiNode(DelphiNodeKind.RecordVariantSection)
{
    
    public required TypeExpr Type { get; init; }

    public override IEnumerable<DelphiNode> Children => [Type];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitRecordVariantSection(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitRecordVariantSection(this);
}

public sealed class RecordField() : DelphiNode(DelphiNodeKind.RecordField)
{
    public required SimpleIdentList Idents { get; init; }
    public required TypeExpr Type { get; init; }

    public override IEnumerable<DelphiNode> Children => [Idents, Type];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitRecordField(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitRecordField(this);
}

public sealed class RecordFieldList() :
    DelphiCollection<
        RecordFieldList,
        RecordField
    >(DelphiNodeKind.RecordField)
{
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitRecordFieldList(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitRecordFieldList(this);
}

public sealed class RecordItemSection() : DelphiNode(DelphiNodeKind.RecordItemSection)
{
    public required MethodDeclSection Methods { get; init; }
    public required PropertyDeclSection Properties { get; init; }
    public required ConstDeclSection Consts { get; init; }
    public required TypeDeclSection Types { get; init; }
    public required RecordFieldList Fields { get; init; }
    public required VarDeclSection Vars { get; init; }
    public required ClassVarDeclSection ClassVars { get; init; }

    public static RecordItemSection Nil => new()
    {
        Methods = MethodDeclSection.Nil,
        Properties = PropertyDeclSection.Nil,
        Fields = RecordFieldList.Nil,
        Types = TypeDeclSection.Nil,
        Consts= ConstDeclSection.Nil,
        Vars= VarDeclSection.Nil,
        ClassVars = ClassVarDeclSection.Nil,
    };

    public override IEnumerable<DelphiNode> Children => [Methods, Properties, Consts, Types, Fields, Vars, ClassVars];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitRecordItemSection(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitRecordItemSection(this);
}
