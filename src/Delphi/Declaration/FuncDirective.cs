using System;
using System.Collections.Generic;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override FuncDirective VisitFunctionDirective(DelphiParser.FunctionDirectiveContext cx)
    {
        return cx.GetText() switch
        {
            "overload" or "overload;" => new OverrideFuncDirective(),
            "unsafe;" => new UnsafeFuncDirective(),
            "inline;" => new InlineFuncDirective(),
            "decl;" => new CDeclFuncDirective(),
            "pascal;" => new PascalFuncDirective(),
            "register;" => new RegisterFuncDirective(),
            "safecall;" => new SafeCallFuncDirective(),
            "stdcall;" => new StdCallFuncDirective(),
            _ => throw new NotImplementedException(),
        };
    }
}

public abstract class FuncDirective() : DelphiNode(DelphiNodeKind.FuncDirective)
{
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitFuncDirective(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitFuncDirective(this);
    public override IEnumerable<DelphiNode> Children => [];
    protected override bool IsLeaf => true;
}

public sealed class OverrideFuncDirective : FuncDirective;
public sealed class InlineFuncDirective : FuncDirective;
public sealed class UnsafeFuncDirective : FuncDirective;
public sealed class CDeclFuncDirective : FuncDirective;
public sealed class PascalFuncDirective : FuncDirective;
public sealed class RegisterFuncDirective : FuncDirective;
public sealed class SafeCallFuncDirective : FuncDirective;
public sealed class StdCallFuncDirective : FuncDirective;

public sealed class FuncDirectiveList() :
    DelphiCollection<
        FuncDirectiveList,
        FuncDirective
    >(DelphiNodeKind.FuncDirective)
{
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitFuncDirectiveList(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitFuncDirectiveList(this);
}
