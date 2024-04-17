using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using DelphiCSharp.Delphi;
using DelphiCSharp.Semantics;

namespace DelphiCSharp.Cs;

public partial class DelphiWalker
{
    public override TypeDeclList VisitTypeDeclSection(TypeDeclSection section)
    {
        return TypeDeclList.From(section.Items.Select(VisitTypeDecl).ToList());
    }

    public override TypeDecl VisitTypeDecl(Delphi.TypeDecl decl)
    {
        return decl.TypeExpr switch
        {
            ClassTypeExpr => VisitClassDecl(decl),
            ClassHelperTypeExpr => throw new NotImplementedException(),
            ClassOfTypeExpr => VisitClassOfDecl(decl),
            InterfaceTypeExpr interfaceTypeExpr => throw new NotImplementedException(),
            EnumTypeExpr enumTypeExpr => throw new NotImplementedException(),
            SimpleRecordTypeExpr simpleRecordTypeExpr => throw new NotImplementedException(),
            VariantRecordTypeExpr variantRecordTypeExpr => throw new NotImplementedException(),
            RecordHelperTypeExpr recordHelperTypeExpr => throw new NotImplementedException(),
            ArrayTypeExpr => throw new NotImplementedException(),
            SetTypeExpr setTypeExpr => throw new NotImplementedException(),
            FileTypeExpr fileTypeExpr => throw new NotImplementedException(),
            
            StructuredTypeExpr structuredTypeExpr => throw new NotImplementedException(),
            
            IdentTypeExpr identTypeExpr => throw new NotImplementedException(),
            VariantTypeExpr variantTypeExpr => throw new NotImplementedException(),
            ConstTypeExpr constTypeExpr => throw new NotImplementedException(),
            
            FuncRefTypeExpr funcRefTypeExpr => throw new NotImplementedException(),
            FuncTypeExpr => VisitFuncTypeDecl(decl),
            GenericTypeExpr genericIdentTypeExpr => throw new NotImplementedException(),
            InferTypeExpr inferTypeExpr => throw new NotImplementedException(),
            MethodTypeExpr methodTypeExpr => throw new NotImplementedException(),
            PagedAnsiStringTypeExpr pagedAnsiStringTypeExpr => throw new NotImplementedException(),
            RangeTypeExpr rangeTypeExpr => throw new NotImplementedException(),
            SizedStringTypeExpr sizedStringTypeExpr => throw new NotImplementedException(),
            TypedPointerTypeExpr typedPointerTypeExpr => throw new NotImplementedException(),
            UnpagedAnsiStringTypeExpr unpagedAnsiStringTypeExpr => throw new NotImplementedException(),
            UnsizedStringTypeExpr unsizedStringTypeExpr => throw new NotImplementedException(),
            UntypedPointerTypeExpr untypedPointerTypeExpr => throw new NotImplementedException(),
            VoidTypeExpr voidTypeExpr => throw new NotImplementedException(),
            _ => throw new ArgumentOutOfRangeException()
        };
    }
    
    private ClassDecl VisitClassDecl(Delphi.TypeDecl decl)
    {
        Debug.Assert(decl.TypeExpr is ClassTypeExpr);
        var helper = VisitClassType((decl.TypeExpr as ClassTypeExpr)!);
        var separated = VisitGenericsList(decl.Generics);

        var ms = helper.Methods.Aggregate(new List<Cs.MethodDecl>(), (agg, m) =>
        {
            agg.AddRange(Root.Resolve<MethodSymbol>(m.Head.Name.Text, decl.Name.Text)!
                .AssociatedNodes
                .Where(met => VisitMethodHead(met.Head).EqualsSignature(m.Head))
                .Select(node => new MethodDecl
                {
                    Head = m.Head,
                    LocalDecls = m.LocalDecls,
                    Body = VisitStatement(node.Block.Body),
                    Source = m.Source,
                })
            );
            return agg;
        });
        
        return new ClassDecl
        {
            Name = VisitSimpleIdent(decl.Name),
            Modifiers = helper.Modifier is not null ? [helper.Modifier] : [],
            TypeArguments = separated.Args,
            TypeConstraints = separated.Constraints,
            Parents = helper.Parents,
            Fields = helper.Fields,
            Properties = helper.Properties,
            Types = helper.Types,
            Methods = [..ms],
            Source = decl.Source,
        };
    }

    private ClassDecl VisitClassOfDecl(Delphi.TypeDecl decl)
    {
        Debug.Assert(decl.TypeExpr is ClassOfTypeExpr);
        var type = VisitClassOfType((decl.TypeExpr as ClassOfTypeExpr)!);
        var separated = VisitGenericsList(decl.Generics);
        return new ClassDecl
        {
            Name = VisitSimpleIdent(decl.Name),
            Modifiers = [],
            TypeArguments = separated.Args,
            TypeConstraints = separated.Constraints,
            Parents = type.Parents,
            Fields = [],
            Properties = [],
            Types = [],
            Methods = [],
            Source = decl.Source,
        };
    }
}

public abstract class TypeDecl : MemberDecl
{
    public abstract required SimpleSymbol Name { get; init; }
    public abstract required TypeModifierList Modifiers { get; init; }
    
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitTypeDecl(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitTypeDecl(this);
}


public sealed class TypeDeclList() :
    CsCollection<
        TypeDeclList,
        TypeDecl
    >(CsNodeKind.TypeDecl)
{
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitTypeDeclList(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitTypeDeclList(this);
}

