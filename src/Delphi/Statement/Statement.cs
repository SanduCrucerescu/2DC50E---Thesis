using System;
using System.Collections.Generic;
using Antlr4.Runtime.Misc;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override Statement VisitStatement(DelphiParser.StatementContext cx)
    {
        if (cx.ifStatement() is { } ifSt) return VisitIfStatement(ifSt);
        if (cx.caseStatement() is { } caseSt) return VisitCaseStatement(caseSt);
        if (cx.repeatStatement() is { } repeat) return VisitRepeatStatement(repeat);
        if (cx.whileStatement() is { } whileSt) return VisitWhileStatement(whileSt);
        if (cx.forStatement() is { } forSt) return VisitForStatement(forSt);
        if (cx.withStatement() is { } with) return VisitWithStatement(with);
        if (cx.tryStatement() is { } trySt) return VisitTryStatement(trySt);
        if (cx.raiseStatement() is { } raiseSt) return VisitRaiseStatement(raiseSt);
        if (cx.compoundStatement() is { } compoundSt) return VisitCompoundStatement(compoundSt);
        if (cx.label() is not null) return VisitLabelledStatement(cx);
        if (cx.simpleStatement() is { } simple) return VisitSimpleStatement(simple);
        
        throw new NotImplementedException();
    }
}

public abstract class Statement() : DelphiNode(DelphiNodeKind.Statement)
{
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitStatement(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitStatement(this);
}


