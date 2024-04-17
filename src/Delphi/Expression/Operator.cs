using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using Antlr4.Runtime.Atn;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override Operator VisitRelOp(DelphiParser.RelOpContext cx)
    {
        var src = Source.Of(cx);
        return cx.GetText() switch
        {
            "<" =>  Operator.Lt(src),
            ">" =>  Operator.Gt(src),
            "<=" => Operator.Leq(src),
            ">=" => Operator.Geq(src),
            "<>" => Operator.Neq(src),
            "=" =>  Operator.Eq(src),
            "in" => Operator.In(src),
            "is" => Operator.Is(src),
            _ => throw new ArgumentOutOfRangeException(nameof(cx)),
        };
    }

    public override Operator VisitOperator(DelphiParser.OperatorContext cx)
    {
        var src = Source.Of(cx);
        return cx.GetText() switch
        {
            "+" => Operator.Plus(src),
            "-" => Operator.Minus(src),
            "or" => Operator.Or(src),
            "xor" => Operator.Xor(src),
            "*" => Operator.Star(src),
            "/" => Operator.Slash(src),
            "div" => Operator.Div(src),
            "mod" => Operator.Mod(src),
            "and" => Operator.And(src),
            "shl" => Operator.Shl(src),
            "shr" => Operator.Shr(src),
            "as" => Operator.As(src),
            _ => throw new ArgumentOutOfRangeException(nameof(cx)),
        };
    }
}
public enum Ord
{
    Gt,
    Lt,
    Eq,
}
    
public sealed class Operator() : DelphiNode(DelphiNodeKind.Operator)
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

    public Ord Cmp(Operator other)
    {
        return Precedence < other.Precedence
            ? Ord.Lt
            : Precedence > other.Precedence
                ? Ord.Gt
                : Ord.Eq;
    }

    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitOperator(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitOperator(this);
    protected override bool IsLeaf => true;
    public override IEnumerable<DelphiNode> Children => [];

    public static Operator Eq(Source source) => new(0, OpKind.Eq, source);
    public static Operator Neq(Source source) => new(0, OpKind.Neq, source);
    public static Operator Gt(Source source) => new(0, OpKind.Gt, source);
    public static Operator Geq(Source source) => new(0, OpKind.Geq, source);
    public static Operator Lt(Source source) => new(0, OpKind.Lt, source);
    public static Operator Leq(Source source) => new(0, OpKind.Leq, source);
    public static Operator In(Source source) => new(0, OpKind.In, source);
    public static Operator Is(Source source) => new(0, OpKind.Is, source);
    public static Operator Plus(Source source) => new(1, OpKind.Plus, source);
    public static Operator Minus(Source source) => new(1, OpKind.Minus, source);
    public static Operator Or(Source source) => new(1, OpKind.Or, source);
    public static Operator Xor(Source source) => new(1, OpKind.XOr, source);
    public static Operator Star(Source source) => new(2, OpKind.Star, source);
    public static Operator Slash(Source source) => new(2, OpKind.Slash, source);
    public static Operator Div(Source source) => new(2, OpKind.Div, source);
    public static Operator Mod(Source source) => new(2, OpKind.Mod, source);
    public static Operator And(Source source) => new(2, OpKind.And, source);
    public static Operator Shl(Source source) => new(2, OpKind.Shl, source);
    public static Operator Shr(Source source) => new(2, OpKind.Shr, source);
    public static Operator As(Source source) => new(2, OpKind.As, source);
    public static Operator Not(Source source) => new(3, OpKind.Not, source);
    public static Operator At(Source source) => new(3, OpKind.At, source);
    public static Operator Deref(Source source) => new(3, OpKind.Deref, source);

}

public enum OpKind
{
    Eq,
    Neq,
    Gt,
    Geq,
    Lt,
    Leq,
    In,
    Is,
    Plus,
    Minus,
    Or,
    XOr,
    Star,
    Slash,
    Div,
    Mod,
    And,
    Shl,
    Shr,
    As,
    Not,
    At,
    Deref,
}

