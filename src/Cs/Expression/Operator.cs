using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using DelphiCSharp.Delphi;

namespace DelphiCSharp.Cs;

public partial class DelphiWalker
{
    public override Operator VisitOperator(Delphi.Operator cx)
    {
        return cx.OpKind switch
        {
            Delphi.OpKind.Plus => Operator.Plus(cx.Source),
            Delphi.OpKind.Minus => Operator.Minus(cx.Source),
            Delphi.OpKind.Star => Operator.Star(cx.Source),
            Delphi.OpKind.Slash => Operator.Slash(cx.Source),
            Delphi.OpKind.As => Operator.As(cx.Source),
            Delphi.OpKind.At => Operator.Ref(cx.Source),
            Delphi.OpKind.Div => Operator.Slash(cx.Source),
            Delphi.OpKind.Eq => Operator.Eq(cx.Source),
            Delphi.OpKind.Neq => Operator.Neq(cx.Source),
            Delphi.OpKind.Geq => Operator.Geq(cx.Source),
            Delphi.OpKind.Gt => Operator.Gt(cx.Source),
            Delphi.OpKind.Leq => Operator.Leq(cx.Source),
            Delphi.OpKind.Lt => Operator.Lt(cx.Source),
            Delphi.OpKind.In => Operator.In(cx.Source),
            Delphi.OpKind.Is => Operator.Is(cx.Source),
            Delphi.OpKind.Mod => Operator.Mod(cx.Source),
            Delphi.OpKind.Not => Operator.Not(cx.Source),
            Delphi.OpKind.Or => Operator.LOr(cx.Source),
            Delphi.OpKind.And => Operator.LAnd(cx.Source),
            Delphi.OpKind.Shl => Operator.Shl(cx.Source),
            Delphi.OpKind.Shr => Operator.Shr(cx.Source),
            Delphi.OpKind.XOr => Operator.XOr(cx.Source),
            _ => throw new ArgumentOutOfRangeException(nameof(cx))
        };
    }
}

public sealed class Operator() : CsNode(CsNodeKind.Operator)
{
    public required int Precedence { get; init; }
    public required OpKind OpKind { get; init; }
    
    [SetsRequiredMembers]
    public Operator(int precedence, OpKind opKind, Source source) : this()
    {
        Precedence = precedence;
        OpKind = opKind;
        Source = source;
    }
    
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitOperator(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitOperator(this);
    protected override bool IsLeaf => true;
    public override IEnumerable<CsNode> Children => [];

    public static Operator LOr(Source source) => new(0, OpKind.LOr, source);
    public static Operator LAnd(Source source) => new(1, OpKind.LAnd, source);
    public static Operator BOr(Source source) => new(2, OpKind.BOr, source);
    public static Operator XOr(Source source) => new(2, OpKind.XOr, source);
    public static Operator BAnd(Source source) => new(4, OpKind.BAnd, source);
    public static Operator Eq(Source source) => new(5, OpKind.Eq, source);
    public static Operator Neq(Source source) => new(5, OpKind.Neq, source);
    public static Operator Gt(Source source) => new(5, OpKind.Gt, source);
    public static Operator Geq(Source source) => new(5, OpKind.Geq, source);
    public static Operator Lt(Source source) => new(5, OpKind.Lt, source);
    public static Operator Leq(Source source) => new(5, OpKind.Leq, source);
    public static Operator Is(Source source) => new(5, OpKind.Is, source);
    public static Operator In(Source source) => new(5, OpKind.In, source);
    public static Operator Not(Source source) => new(5, OpKind.Not, source); // TODO: not sure
    public static Operator Or(Source source) => new(5, OpKind.Or, source);   // TODO: not sure
    public static Operator Ref(Source source) => new(5, OpKind.Ref, source);
    public static Operator As(Source source) => new(5, OpKind.As, source);
    public static Operator Shl(Source source) => new(6, OpKind.Shl, source);
    public static Operator Shr(Source source) => new(6, OpKind.Shr, source);
    public static Operator Plus(Source source) => new(7, OpKind.Plus, source);
    public static Operator Minus(Source source) => new(7, OpKind.Minus, source);
    public static Operator Star(Source source) => new(8, OpKind.Star, source);
    public static Operator Slash(Source source) => new(8, OpKind.Slash, source);
    public static Operator Mod(Source source) => new(8, OpKind.Mod, source);
}

public enum OpKind
{
    Eq,
    Neq,
    Gt,
    Geq,
    Lt,
    Leq,
    
    Plus,
    Minus,
    Star,
    Slash,
    Mod,
    
    Shl,
    Shr,
    
    Not,
    Is,
    In,
    As,
    Ref,
    Or,
    
    XOr,
    BOr,
    BAnd,
    
    LAnd,
    LOr,
}