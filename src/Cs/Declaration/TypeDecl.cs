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

    public override TypeDecl VisitTypeDecl(Delphi.TypeDecl cx)
    {
        return cx.TypeExpr switch
        {
            ClassTypeExpr => VisitClassDecl(cx),
            ClassHelperTypeExpr => throw new NotImplementedException(),
            ClassOfTypeExpr => VisitClassOfDecl(cx),
            InterfaceTypeExpr => throw new NotImplementedException(),
            EnumTypeExpr => VisitEnumDecl(cx),
            SimpleRecordTypeExpr simpleRecordTypeExpr => throw new NotImplementedException(),
            VariantRecordTypeExpr variantRecordTypeExpr => throw new NotImplementedException(),
            RecordHelperTypeExpr recordHelperTypeExpr => throw new NotImplementedException(),
            ArrayTypeExpr => throw new NotImplementedException(),
            SetTypeExpr setTypeExpr => throw new NotImplementedException(),
            FileTypeExpr fileTypeExpr => throw new NotImplementedException(),
            
            StructuredTypeExpr structuredTypeExpr => throw new NotImplementedException(),
            
            IdentTypeExpr => VisitIdentTypeDecl(cx),
            VariantTypeExpr variantTypeExpr => throw new NotImplementedException(),
            ConstTypeExpr constTypeExpr => throw new NotImplementedException(),
            
            FuncRefTypeExpr funcRefTypeExpr => throw new NotImplementedException(),
            FuncTypeExpr => VisitFuncTypeDecl(cx),
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

    public TypeDecl VisitIdentTypeDecl(Delphi.TypeDecl cx)
    {
        cx.Print("IDENT");
        throw new NotImplementedException();
    }


    private ClassDecl VisitClassDecl(Delphi.TypeDecl cx)
    {
        Debug.Assert(cx.TypeExpr is ClassTypeExpr);
        var helper = VisitClassType((cx.TypeExpr as ClassTypeExpr)!);
        var separated = VisitGenericsList(cx.Generics);
        var className = VisitSimpleIdent(cx.Name);

        var ms = helper.Methods.Aggregate(new List<Cs.MethodDecl>(), (agg, m) =>
        {
            var methodName = m.Head.MethodKind is MethodKind.Constructor or MethodKind.Destructor
                ? className
                : m.Head.Name;
            agg.AddRange(Root.Resolve<MethodSymbol>(methodName.Text, cx.Name.Text)!
                .AssociatedNodes
                .Where(met => VisitMethodHead(met.Head).EqualsSignature(m.Head))
                .Select(node => new MethodDecl
                {
                    Head = new MethodHead
                    {
                        MethodKind = m.Head.MethodKind,
                        Name = methodName,
                        Parent = m.Head.Parent ?? methodName,
                        TypeArguments = m.Head.TypeArguments,
                        TypeConstraints = m.Head.TypeConstraints,
                        ReturnType = m.Head.ReturnType,
                        Params = m.Head.Params,
                        Modifiers = m.Head.Modifiers,
                    },
                    LocalDecls = m.LocalDecls,
                    Body = VisitStatement(node.Block.Body),
                    Source = m.Source,
                }));
            return agg;
        });
        
        return new ClassDecl
        {
            Name = className,
            Modifiers = helper.Modifier is not null ? [helper.Modifier] : [],
            TypeArguments = separated.Args,
            TypeConstraints = separated.Constraints,
            Parents = helper.Parents,
            Fields = helper.Fields,
            Properties = helper.Properties,
            Types = helper.Types,
            Methods = [..ms],
            Source = cx.Source,
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

