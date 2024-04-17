using System;
using System.Collections.Generic;
using DelphiCSharp.Delphi;

namespace DelphiCSharp.Cs;

public partial class DelphiWalker
{
    public override Expr VisitLitExpr(Delphi.LitExpr cx)
    {
        return new LitExpr
        {
            LitKind = cx.LitKind switch
            {
                Delphi.LitKind.U32 => LitKind.U32,
                Delphi.LitKind.U64 => LitKind.U64,
                Delphi.LitKind.I32 => LitKind.I32,
                Delphi.LitKind.I64 => LitKind.I64,
                Delphi.LitKind.F32 => LitKind.F32,
                Delphi.LitKind.F64 => LitKind.F64,
                Delphi.LitKind.Hex => LitKind.Hex,
                Delphi.LitKind.String => LitKind.String,
                Delphi.LitKind.True => LitKind.True,
                Delphi.LitKind.False => LitKind.False,
                Delphi.LitKind.Nil => LitKind.Null,
                _ => throw new ArgumentOutOfRangeException()
            },
            Value = cx.Value,
            Source = cx.Source,
        };
    }
}

public sealed class LitExpr : PrimaryExpr
{
    public required LitKind LitKind { get; init; }
    public required object? Value { get; init; }

    protected override bool IsLeaf => true;
    public override IEnumerable<CsNode> Children => [];
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitLitExpr(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitLitExpr(this);
}

public enum LitKind
{
    U32,
    U64,
    I32,
    I64,
    F32,
    F64,
    Hex,
    String,
    True,
    False,
    Null,
}