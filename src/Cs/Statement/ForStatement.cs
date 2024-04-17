using System.Collections.Generic;
using System.Diagnostics;
using DelphiCSharp.Delphi;

namespace DelphiCSharp.Cs;

public partial class DelphiWalker
{
    public override ForStatement VisitForToStatement(ForToStatement cx)
    {
        return new ForStatement
        {
            VariableName = VisitSimpleIdent(cx.Ident),
            InitialValue = VisitExpr(cx.From),
            EndValue = VisitExpr(cx.UpTo),
            Statement = VisitStatement(cx.Statement),
            Source = cx.Source,
        };
    }
}

public sealed class ForStatement : Statement
{
    public required SimpleSymbol VariableName { get; init; }
    public required Expr InitialValue { get; init; }
    public required Expr EndValue { get; init; }
    public required Statement Statement { get; init; }
    
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitForStatement(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitForStatement(this);
    public override IEnumerable<CsNode> Children => [VariableName, InitialValue, EndValue, Statement];
}