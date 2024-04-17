using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override Statement VisitForStatement(DelphiParser.ForStatementContext cx)
    {
        if (cx.TO() is not null)
        {
            return new ForToStatement
            {
                Ident = VisitIdent(cx.ident()),
                From = VisitExpression(cx.expression().First()),
                UpTo = VisitExpression(cx.expression().Last()),
                Statement = VisitStatement(cx.statement()),
                Source = Source.Of(cx),
            };
        }

        if (cx.DOWNTO() is not null)
        {
            return new ForDownToStatement
            {
                Ident = VisitIdent(cx.ident()),
                From = VisitExpression(cx.expression().First()),
                DownTo = VisitExpression(cx.expression().Last()),
                Statement = VisitStatement(cx.statement()),
                Source = Source.Of(cx),
            };
        }
        
        Debug.Assert(cx.IN() is not null);
        
        return new ForInStatement
        {
            Ident = VisitIdent(cx.ident()),
            Iter = VisitExpression(cx.expression().First()),
            Statement = VisitStatement(cx.statement()),
            Source = Source.Of(cx),
        };
    }
}

public sealed class ForInStatement : Statement
{
    public required SimpleIdent Ident { get; init; }
    public required Expr Iter { get; init; }
    public required Statement Statement { get; init; }

    public override IEnumerable<DelphiNode> Children => [Ident, Iter, Statement];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitForInStatement(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitForInStatement(this);
}

public sealed class ForToStatement : Statement
{
    public required SimpleIdent Ident { get; init; }
    public required Expr From { get; init; }
    public required Expr UpTo { get; init; }
    public required Statement Statement { get; init; }

    public override IEnumerable<DelphiNode> Children => [Ident, From, UpTo, Statement];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitForToStatement(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitForToStatement(this);
}

public sealed class ForDownToStatement : Statement
{
    public required SimpleIdent Ident { get; init; }
    public required Expr From { get; init; }
    public required Expr DownTo { get; init; }
    public required Statement Statement { get; init; }

    public override IEnumerable<DelphiNode> Children => [Ident, From, DownTo, Statement];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitForDownToStatement(this);
}