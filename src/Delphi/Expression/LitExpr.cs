using System;
using System.Collections.Generic;
using System.Globalization;
using Antlr4.Runtime.Tree;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override LitExpr VisitIntNum(DelphiParser.IntNumContext cx)
    {
        try
        {
            return new LitExpr
            {
                LitKind = LitKind.I32,
                Value = int.Parse(cx.GetText()),
                Source = Source.Of(cx),
            };
        }
        catch (OverflowException oex)
        {
            Console.WriteLine(oex);
            throw;
        }
    }

    private LitExpr VisitTkIntNum(ITerminalNode node)
    {
        if (int.TryParse(node.GetText(), out var res))
        {
            return new LitExpr
            {
                LitKind = LitKind.I32,
                Value = res,
                Source = Source.Of(node.Symbol)
            };
        }

        throw new ArgumentException("Do proper error handling later", nameof(node));
    }

    private LitExpr VisitTkHexNum(ITerminalNode node)
    {
        if (ulong.TryParse(
                node.GetText().Replace('$', ' '), 
                NumberStyles.HexNumber, 
                CultureInfo.InvariantCulture, 
                out var res))
        {
            return new LitExpr
            {
                LitKind = LitKind.Hex,
                Value = res,
                Source = Source.Of(node.Symbol)
            };
        }
        
        throw new ArgumentException("Do proper error handling later", nameof(node));
    }
}
    
public sealed class LitExpr : Expr
{
    public required LitKind LitKind { get; init; }
    public required object? Value { get; init; }
    
    protected override bool IsLeaf => true;
    public override IEnumerable<DelphiNode> Children => [];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitLitExpr(this);
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
    Nil,
}
