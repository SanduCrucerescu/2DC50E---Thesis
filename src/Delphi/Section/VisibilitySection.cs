using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;

namespace DelphiCSharp.Delphi;

public partial class Visitor
{
    public override Visibility VisitVisibility(DelphiParser.VisibilityContext cx)
    {
        if (cx.STRICT() is not null)
        {
            if (cx.PROTECTED() is not null) return Visibility.StrictProtected(Source.Of(cx));
            if (cx.PRIVATE() is not null) return Visibility.StrictPrivate(Source.Of(cx));
            throw new ArgumentOutOfRangeException(nameof(cx));
        }

        if (cx.PROTECTED() is not null) return Visibility.Protected(Source.Of(cx));
        if (cx.PRIVATE() is not null) return Visibility.Private(Source.Of(cx));
        if (cx.PUBLIC() is not null) return Visibility.Public(Source.Of(cx));
        if (cx.PUBLISHED() is not null) return Visibility.Published(Source.Of(cx));
        if (cx.AUTOMATED() is not null) return Visibility.Automated(Source.Of(cx));
        throw new ArgumentOutOfRangeException(nameof(cx));
    }
}

public sealed class VisibilitySection() : DelphiNode(DelphiNodeKind.VisibilitySection)
{
    public required Visibility Visibility { get; set; }
    public required MethodDeclSection Methods { get; init; }
    public required PropertyDeclSection Properties { get; init; }
    public required ClassFieldDeclSection Fields { get; init; }
    public required TypeDeclSection Types { get; init; }
    public required ConstDeclSection Consts { get; init; }
    public required VarDeclSection Vars { get; init; }
    public required ClassVarDeclSection ClassVars { get; init; }

    public static VisibilitySection Nil(Visibility vis) => new()
    {
        Visibility = vis,
        Methods = MethodDeclSection.Nil,
        Properties = PropertyDeclSection.Nil,
        Fields = ClassFieldDeclSection.Nil,
        Types = TypeDeclSection.Nil,
        Consts= ConstDeclSection.Nil,
        Vars= VarDeclSection.Nil,
        ClassVars = ClassVarDeclSection.Nil,
    };

    public override IEnumerable<DelphiNode> Children =>
        [Visibility, Methods, Properties, Fields, Types, Consts, Vars, ClassVars];
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitVisibilitySection(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitVisibilitySection(this);
}

public sealed class Visibility() : DelphiNode(DelphiNodeKind.Visibility)
{
    public required VisibilityKind VisKind { get; init; }
    
    [SetsRequiredMembers]
    public Visibility(VisibilityKind visKind, Source source) : this()
    {
        VisKind = visKind;
        Source = source;
    }
    
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitVisibility(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitVisibility(this);
    public override IEnumerable<DelphiNode> Children => [];
    protected override bool IsLeaf => true;

    public static Visibility Public(Source source) => new(VisibilityKind.Public, source);
    public static Visibility Private(Source source) => new(VisibilityKind.Private, source);
    public static Visibility StrictPrivate(Source source) => new(VisibilityKind.StrictPrivate, source);
    public static Visibility Protected(Source source) => new(VisibilityKind.Protected, source);
    public static Visibility StrictProtected(Source source) => new(VisibilityKind.StrictProtected, source);
    public static Visibility Published(Source source) => new(VisibilityKind.Published, source);
    public static Visibility Automated(Source source) => new(VisibilityKind.Automated, source);
}

public enum VisibilityKind
{
    Public,
    Private,
    StrictPrivate,
    Protected,
    StrictProtected,
    Published,
    Automated
}

// public sealed class PublicVisibility(Source source) : Visibility(source);
// public sealed class PrivateVisibility(Source source) : Visibility(source);
// public sealed class StrictPrivateVisibility(Source source) : Visibility(source);
// public sealed class ProtectedVisibility(Source source) : Visibility(source);
// public sealed class StrictProtectedVisibility(Source source) : Visibility(source);
// public sealed class PublishedVisibility(Source source) : Visibility(source);
// public sealed class AutomatedVisibility(Source source) : Visibility();
// public sealed class NilVisibility : Visibility;

public sealed class VisibilitySectionList() :
    DelphiCollection<
        VisibilitySectionList,
        VisibilitySection
    >(DelphiNodeKind.VisibilitySection)
{
    protected override T Accept<T>(DelphiVisitor<T> visitor) => visitor.VisitVisibilitySectionList(this);
    protected override void Accept(DelphiVisitor visitor) => visitor.VisitVisibilitySectionList(this);
}
