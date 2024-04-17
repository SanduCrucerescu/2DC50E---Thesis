using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using DelphiCSharp.Cs;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override MethodDecl VisitMethodDecl(DelphiParser.MethodDeclContext cx)
    {
        return new MethodDecl
        {
            Head = VisitMethodDeclHeading(cx.methodDeclHeading()),
            Block = cx.methodBody() is { } body 
                ? VisitMethodBody(body)
                : BlockSection.Nil,
            Source = Source.Of(cx),
        };
    }

    public override BlockSection VisitMethodBody(DelphiParser.MethodBodyContext cx)
    {
        return VisitBlock(cx.block());
    }

    public override MethodHead VisitMethodDeclHeading(DelphiParser.MethodDeclHeadingContext cx)
    {
        var kind = cx.FUNCTION() is not null
            ? MethodKind.Function(Source.Of(cx))
            : cx.OPERATOR() is not null
                ? MethodKind.Operator(Source.Of(cx))
                : VisitMethodKey(cx.methodKey());

        var directives = kind.Kind is EMethodKind.Operator
            ? MethodDirectiveList.Nil
            : MethodDirectiveList.From(cx.methodDirective().Select(VisitMethodDirective));

        if (cx.CLASS() is not null)
        {
            directives.Add(MethodDirective.Class(Source.Of(cx)));
        }

        return new MethodHead
        {
            Name = VisitMethodName(cx.methodName()),
            Signature = new MethodSignature
            {
                Params = cx.formalParameterSection() is { } parms
                    ? VisitFormalParameterSection(parms)
                    : [],
                MethodKind = kind,
                ReturnType = kind.Kind switch
                {
                    EMethodKind.Operator or EMethodKind.Constructor or EMethodKind.Destructor => null,
                    EMethodKind.Function => VisitTypeExpr(cx.typeExpr()),
                    EMethodKind.Procedure => new VoidTypeExpr(),
                    _ => throw new ArgumentOutOfRangeException(nameof(kind))
                },
                Directives = directives,
                Source = cx.formalParameterSection() is { } fps ? Source.Of(fps) : Source.Nil
                     + kind.Source
                     + directives.Source
                     + (cx.typeExpr() is { } t ? Source.Of(t) : Source.Nil),
            },
            Source = Source.Of(cx),
        };
    }

    public override MethodName VisitMethodName(DelphiParser.MethodNameContext cx)
    {
        Debug.Assert(cx.genericTypeIdent().Length > 0);
        return new MethodName
        {
            Segments = new GenericIdentList
            {
                Items = cx.genericTypeIdent().Select(VisitGenericTypeIdent).ToList(),
                Source = Source.Of(cx.genericTypeIdent()),
            },
            Source = Source.Of(cx)
        };
    }

    public override MethodDirective VisitMethodDirective(DelphiParser.MethodDirectiveContext cx)
    {
        return cx.GetText() switch
        {
            "reintroduce;" => MethodDirective.Reintroduce(Source.Of(cx)),
            "overload" or "overload;" => MethodDirective.Overload(Source.Of(cx)),
            "static;" => MethodDirective.Static(Source.Of(cx)),
            "dynamic;" => MethodDirective.Dynamic(Source.Of(cx)),
            "override;" => MethodDirective.Override(Source.Of(cx)),
            "virtual;" => MethodDirective.Virtual(Source.Of(cx)),
            "abstract;" => MethodDirective.Abstract(Source.Of(cx)),
            "final;" => MethodDirective.Final(Source.Of(cx)),
            "inline;" => MethodDirective.Inline(Source.Of(cx)),
            "cdecl;" or "pascal;" or "register;" or "safecall;" or "stdcall;" => throw new NotImplementedException(),
            _ => throw new NotImplementedException(),
        };
    }

    public override MethodKind VisitMethodKey(DelphiParser.MethodKeyContext cx)
    {
        if (cx.PROCEDURE() is not null) return MethodKind.Procedure(Source.Of(cx));
        if (cx.CONSTRUCTOR() is not null) return MethodKind.Constructor(Source.Of(cx));
        if (cx.DESTRUCTOR() is not null) return MethodKind.Destructor(Source.Of(cx));
        throw new ArgumentOutOfRangeException(nameof(cx));
    }
}

public sealed class MethodDecl : Decl
{
    public required MethodHead Head { get; init; }
    public required BlockSection Block { get; init; }

    public override IEnumerable<DelphiNode> Children => [Head, Block];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitMethodDecl(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitMethodDecl(this);
}

public sealed class MethodDeclSection() :
    DelphiCollection<
        MethodDeclSection,
        MethodDecl
    >(DelphiNodeKind.MethodDecl)
{
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitMethodDeclSection(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitMethodDeclSection(this);
}

public sealed class MethodHead() : DelphiNode(DelphiNodeKind.MethodHead)
{
    public required MethodName Name { get; init; }
    public required MethodSignature Signature { get; init; }

    public override IEnumerable<DelphiNode> Children => [Name, Signature];
            
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitMethodHead(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitMethodHead(this);

}

public sealed class MethodSignature() : DelphiNode(DelphiNodeKind.FuncHead), IEquatable<MethodSignature>
{
    public required MethodKind MethodKind { get; init; }
    public required FormalParamsSection Params { get; init; }
    public required TypeExpr? ReturnType { get; init; }
    public required MethodDirectiveList Directives { get; init; }

    public override IEnumerable<DelphiNode> Children => ReturnType is not null
        ? [MethodKind, Params, ReturnType, Directives]
        : [MethodKind, Params, Directives];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitMethodSignature(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitMethodSignature(this);
    
    public bool Equals(MethodSignature? other)
    {
        if (ReferenceEquals(null, other)) return false;
        if (ReferenceEquals(this, other)) return true;
        return base.Equals(other) 
               && MethodKind.Equals(other.MethodKind) 
               && Params.Equals(other.Params) 
               && Equals(ReturnType, other.ReturnType);
    }

    public override bool Equals(object? obj)
    {
        return ReferenceEquals(this, obj) || obj is MethodSignature other && Equals(other);
    }

    public override int GetHashCode()
    {
        return HashCode.Combine(base.GetHashCode(), MethodKind, Params, ReturnType, Directives);
    }

    public static bool operator ==(MethodSignature? left, MethodSignature? right)
    {
        return Equals(left, right);
    }

    public static bool operator !=(MethodSignature? left, MethodSignature? right)
    {
        return !Equals(left, right);
    }
}

public sealed class MethodName : Ident
{
    public required GenericIdentList Segments { get; init; }

    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitMethodName(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitMethodName(this);
    public override IEnumerable<DelphiNode> Children => [Segments];
}

public sealed class MethodDirective() : DelphiNode(DelphiNodeKind.MethodDirective)
{
    public required MethodDirectiveKind DirectiveKind { get; init; }

    [SetsRequiredMembers]
    public MethodDirective(MethodDirectiveKind kind, Source source) : this()
    {
        DirectiveKind = kind;
        Source = source;
    }
    
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitMethodDirective(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitMethodDirective(this);
    public override IEnumerable<DelphiNode> Children => [];
    protected override bool IsLeaf => true;

    public static MethodDirective Overload(Source source) => new(MethodDirectiveKind.Overload, source);
    public static MethodDirective Inline(Source source) => new(MethodDirectiveKind.Inline, source);
    public static MethodDirective Class(Source source) => new(MethodDirectiveKind.Class, source);
    public static MethodDirective Static(Source source) => new(MethodDirectiveKind.Static, source);
    public static MethodDirective Dynamic(Source source) => new(MethodDirectiveKind.Dynamic, source);
    public static MethodDirective Override(Source source) => new(MethodDirectiveKind.Override, source);
    public static MethodDirective Virtual(Source source) => new(MethodDirectiveKind.Virtual, source);
    public static MethodDirective Abstract(Source source) => new(MethodDirectiveKind.Abstract, source);
    public static MethodDirective Final(Source source) => new(MethodDirectiveKind.Final, source);
    public static MethodDirective Reintroduce(Source source) => new(MethodDirectiveKind.Reintroduce, source);
    public static MethodDirective Unsafe(Source source) => new(MethodDirectiveKind.Unsafe, source);
    
}

public enum MethodDirectiveKind
{
    Overload,
    Inline,
    Class,
    Static,
    Dynamic,
    Override,
    Virtual,
    Abstract,
    Final,
    Reintroduce,
    Unsafe,
} 

// public sealed class CDeclMethodDirective : MethodDirective;
// public sealed class PascalMethodDirective : MethodDirective;
// public sealed class RegisterMethodDirective : MethodDirective;
// public sealed class SafeCallMethodDirective : MethodDirective;
// public sealed class StdCallMethodDirective : MethodDirective;

public sealed class MethodDirectiveList() :
    DelphiCollection<
        MethodDirectiveList,
        MethodDirective
    >(DelphiNodeKind.MethodDirective)
{
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitMethodDirectiveList(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitMethodDirectiveList(this);
}

public sealed class MethodKind() : DelphiNode(DelphiNodeKind.MethodKind), IEquatable<MethodKind>
{
    public bool Equals(MethodKind? other)
    {
        return other != null && Kind == other.Kind;
    }

    public override bool Equals(object? obj)
    {
        return ReferenceEquals(this, obj) || obj is MethodKind other && Equals(other);
    }

    public override int GetHashCode()
    {
        return Kind.GetHashCode();
    }

    public static bool operator ==(MethodKind? left, MethodKind? right)
    {
        return Equals(left, right);
    }

    public static bool operator !=(MethodKind? left, MethodKind? right)
    {
        return !Equals(left, right);
    }

    public required EMethodKind Kind { get; init; }
    
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitMethodKind(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitMethodKind(this);
    public override IEnumerable<DelphiNode> Children => [];
    protected override bool IsLeaf => true;

    [SetsRequiredMembers]
    public MethodKind(Source source, EMethodKind kind) : this()
    {
        Source = source;
        Kind = kind;
    }


    public static MethodKind Procedure(Source source) => new(source, EMethodKind.Procedure);
    public static MethodKind Function(Source source) => new(source, EMethodKind.Function);
    public static MethodKind Constructor(Source source) => new(source, EMethodKind.Constructor);
    public static MethodKind Destructor(Source source) => new(source, EMethodKind.Destructor);
    public static MethodKind Operator(Source source) => new(source, EMethodKind.Operator);
}

public enum EMethodKind
{
    Procedure,
    Function,
    Constructor,
    Destructor,
    Operator,
}
