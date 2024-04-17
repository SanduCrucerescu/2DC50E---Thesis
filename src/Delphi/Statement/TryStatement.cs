using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override Statement VisitTryStatement(DelphiParser.TryStatementContext cx)
    {
        if (cx.EXCEPT() is not null)
        {
            return new TryExceptStatement
            {
                Tries = cx.statementList() is { } stmts0
                    ? VisitStatementList(stmts0.First())
                    : [],
                Handlers = new TryHandlerList
                {
                    Items = cx.handlerList().handler().Select(VisitHandler).ToImmutableArray(),
                    Source = Source.Of(cx.handlerList()),
                },
                Else = cx.handlerList().statementList() is { } stmts1
                    ? VisitStatementList(stmts1)
                    : [],
                Source = Source.Of(cx),
            };
        }
        
        Debug.Assert(cx.FINALLY() is not null);
        var src = Source.Of(cx);
        return cx.statementList() switch
        {
            [var tries, var final] => new TryFinallyStatement
            {
                Tries = VisitStatementList(tries),
                Finally = VisitStatementList(final),
                Source = src,
            },
            [var stmts] => cx.GetChild(1).GetText() == "finally"
                ? new TryFinallyStatement
                {
                    Tries = [],
                    Finally = VisitStatementList(stmts),
                    Source = src,
                } 
                : new TryFinallyStatement
                {
                    Tries = VisitStatementList(stmts),
                    Finally = [],
                    Source = src,
                },
            _ => new TryFinallyStatement
            {
                Tries = [],
                Finally = [],
                Source = src,
            },
        };
    }

    public override TryHandler VisitHandler(DelphiParser.HandlerContext cx)
    {
        return new TryHandler
        {
            Alias = cx.handlerIdent() is { } ident 
                ? VisitIdent(cx.handlerIdent().ident())
                : null,
            Type = VisitTypeId(cx.typeId()),
            Statement = cx.handlerStatement().statement() is { } stmt
                ? VisitStatement(stmt)
                : null,
            Source = Source.Of(cx),
        };
    }
}

public sealed class TryFinallyStatement : Statement
{
    public required StatementSection Tries { get; init; }
    public required StatementSection Finally { get; init; }

    public override IEnumerable<DelphiNode> Children => [Tries, Finally];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitTryFinallyStatement(this);
}

public sealed class TryExceptStatement : Statement
{
    public required StatementSection Tries { get; init; }
    public required TryHandlerList Handlers { get; init; }
    public required StatementSection Else { get; init; }

    public override IEnumerable<DelphiNode> Children => [Tries, Handlers, Else,];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitTryExceptStatement(this);
}

public sealed class TryHandler() : DelphiNode(DelphiNodeKind.TryHandler)
{
    public required SimpleIdent? Alias { get; init; }
    public required TypeId Type { get; init; }
    public required Statement? Statement { get; init; }

    public override IEnumerable<DelphiNode> Children =>
        Alias is not null 
            ? Statement is not null 
                ? [Alias, Type, Statement]
                : [Alias, Type]
            : [Type];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitTryHandler(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitTryHandler(this);
}

public sealed class TryHandlerList() :
    ImmutableDelphiCollection<
        TryHandlerList,
        TryHandler
    >(DelphiNodeKind.TryHandler)
{
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitTryHandlerList(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitTryHandlerList(this);
}