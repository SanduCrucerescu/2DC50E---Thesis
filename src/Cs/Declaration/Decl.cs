using System;
using DelphiCSharp.Delphi;

namespace DelphiCSharp.Cs;

public partial class DelphiWalker
{
    public override Decl VisitDecl(Delphi.Decl decl)
    {
        return decl switch
        {
            Delphi.ClassFieldDecl field => VisitClassFieldDecl(field),
            ConstDecl constDecl => VisitConstDecl(constDecl),
            Delphi.MethodDecl methodDecl => VisitMethodDecl(methodDecl),
            Delphi.PropertyDecl propertyDecl => VisitPropertyDecl(propertyDecl),
            Delphi.TypeDecl typeDecl => VisitTypeDecl(typeDecl),
            Delphi.VarDecl varDecl => VisitVarDecl(varDecl),
            _ => throw new ArgumentOutOfRangeException(nameof(decl))
        };
    }

    public override DeclList VisitDeclSection(Delphi.DeclSection section)
    {
        var res = DeclList.From([]);

        foreach (var var in section.Vars.Items) res.Add(VisitVarDecl(var));
        foreach (var @const in section.Consts.Items) res.Add(VisitConstDecl(@const));
        foreach (var func in section.Funcs.Items) res.Add(VisitFuncDecl(func));
        foreach (var method in section.Methods.Items) res.Add(VisitMethodDecl(method));
        foreach (var type in section.Types.Items) res.Add(VisitTypeDecl(type));
        
        return res;
    }
    
}

public abstract class Decl() : CsNode(CsNodeKind.Decl)
{
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitDecl(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitDecl(this);
}

public sealed class DeclList() :
    CsCollection<
        DeclList,
        Decl
    >(CsNodeKind.Decl)
{
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitDeclList(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitDeclList(this);
}
