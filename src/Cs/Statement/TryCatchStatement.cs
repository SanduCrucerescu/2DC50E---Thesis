using System;
using System.Collections.Generic;
using System.Linq;
using DelphiCSharp.Delphi;

namespace DelphiCSharp.Cs;

public partial class DelphiWalker
{
    public override TryCatchStatement VisitTryFinallyStatement(TryFinallyStatement cx)
    {
        return new TryCatchStatement
        {
            Statements = VisitStatementSection(cx.Tries),
            Handlers = [],
            Finally = VisitStatementSection(cx.Finally),
            Source = cx.Source,
        };
    }

    public override TryCatchStatement VisitTryExceptStatement(TryExceptStatement cx)
    {
        var elseHandler = cx.Else.Items.Length > 0 
            ? new CatchHandler
            {
                Type = new SimpleSymbol("Exception", cx.Else.Source),
                Alias = null,
                Statement = new CompoundStatement
                {
                    Statements = VisitStatementSection(cx.Else),
                    Source = cx.Else.Source
                },
                Source = cx.Else.Source,
            } : null;

        var handlers = VisitTryHandlerList(cx.Handlers);
        if (elseHandler is not null)
        {
            handlers.Add(elseHandler);
        }
        return new TryCatchStatement
        {
            Statements = VisitStatementSection(cx.Tries),
            Handlers = handlers,
            Finally = [],
            Source = cx.Source,
        };
    }

    public override CatchHandler VisitTryHandler(TryHandler cx)
    {
        return new CatchHandler
        {
            Type = VisitTypeId(cx.Type),
            Alias = cx.Alias is not null ? VisitSimpleIdent(cx.Alias) : null,
            Statement = cx.Statement is not null ? VisitStatement(cx.Statement) : new CompoundStatement
            {
                Statements = [],
                Source = cx.Source,
            },
            Source = cx.Source,
        };
    }

    public override CatchHandlerList VisitTryHandlerList(TryHandlerList cx)
    {
        return CatchHandlerList.From(cx.Items.Select(VisitTryHandler));
    }
}

public sealed class TryCatchStatement : Statement
{
    public required StatementList Statements { get; init; }
    public required CatchHandlerList Handlers { get; init; }
    public required StatementList Finally { get; init; }

    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitTryCatchStatement(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitTryCatchStatement(this);
    public override IEnumerable<CsNode> Children => [Statements, Handlers, Finally];
}

public sealed class CatchHandler() : CsNode(CsNodeKind.CatchHandler)
{
    public required Type Type { get; init; }
    public required SimpleSymbol? Alias { get; init; }
    public required Statement Statement { get; init; }
        
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitCatchHandler(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitCatchHandler(this);
    public override IEnumerable<CsNode> Children => Alias is not null ? [Type, Alias, Statement] : [Type, Statement];
}

public sealed class CatchHandlerList() :
    CsCollection<
        CatchHandlerList,
        CatchHandler
    >(CsNodeKind.CatchHandler)
{
    protected override T Accept<T>(CsVisitor<T> visitor) => visitor.VisitCatchHandlerList(this);
    protected override void Accept(CsVisitor visitor) => visitor.VisitCatchHandlerList(this);
}