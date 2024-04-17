using System.Collections.Generic;
using System.Linq;

namespace DelphiCSharp.Cs;


public partial class DelphiWalker
{
    public override VarDecl VisitVarDecl(Delphi.VarDecl decl)
    {
        return new VarDecl
        {
            Type = VisitTypeExpr(decl.Type),
            Names = VisitSimpleIdentList(decl.Idents),
            Expr = decl.Expr is {} expr ? VisitExpr(expr) : null,
            IsConst = false,
            Source = decl.Source,
        };
    }
    
    public override VarDeclList VisitVarDeclSection(Delphi.VarDeclSection section)
    {
        return VarDeclList.From(section.Items.Select(VisitVarDecl));
    }

    public override VarDecl VisitConstDecl(Delphi.ConstDecl decl)
    {
        return new VarDecl
        {
            Type = VisitTypeExpr(decl.Type),
            Names = [VisitSimpleIdent(decl.Ident)],
            Expr = VisitConstExpr(decl.Expr),
            IsConst = true,
            Source = decl.Source,
        };
    }

    public override VarDeclList VisitConstDeclSection(Delphi.ConstDeclSection section)
    {
        return VarDeclList.From(section.Items.Select(VisitConstDecl));
    }

    public override ClassFieldDeclList VisitClassVarDeclSection(Delphi.ClassVarDeclSection section)
    {
        return ClassFieldDeclList.From(section.Items.Select(var =>
        {
            var vr = VisitVarDecl(var);
            return new ClassFieldDecl
            {
                Modifiers = FieldModifierList.From([FieldModifier.Static(Source.Conjured)]),
                Type = vr.Type,
                Names = vr.Names,
                Expr = vr.Expr,
                Source = vr.Source,
            };
        }));
    }
}

public sealed class VarDecl : Decl
{
    public required Type Type { get; init; }
    public required SimpleSymbolsList Names { get; init; }
    public required Expr? Expr { get; init; }
    public required bool IsConst { get; init; }

    public override IEnumerable<CsNode> Children => 
        Expr is not null ? [Type, Names, Expr] : [Type, Names];
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitVarDecl(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitVarDecl(this);
}


public sealed class VarDeclList() :
    CsCollection<
        VarDeclList,
        VarDecl
    >(CsNodeKind.VarDecl)
{
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitVarDeclList(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitVarDeclList(this);
}