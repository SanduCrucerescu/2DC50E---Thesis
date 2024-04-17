using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using DelphiCSharp.Cs;

namespace DelphiCSharp;

public interface ICollectionKind<out TKind>
{
    public static abstract TKind CollectionKind { get; }
}

public abstract class Node<TNode, TKind>(TKind kind)
{
    public readonly TKind SyntaxKind = kind;
    public Source Source { get; init; } = Source.Nil;
    public virtual IEnumerable<Node<TNode, TKind>> Children => [];
    protected virtual bool IsLeaf => Source is Leaf;

    public abstract T Accept<T>(Visitor<TNode, TKind, T> visitor);
    public abstract void Accept(Visitor<TNode, TKind> visitor);

    
    public override string ToString()
    {
        return ToString(Source);
    }

    private string ToString(Source source)
    {
        if (Source is Span span)
        {
            return $"{span.SourceName}:{span.Line}:{span.Col} {SyntaxKind} {span.Text}";
        }
        
        if (Source is SpanList spans)
        {
            var res = "";
            foreach (var s in spans)
            {
                if (s is Span _span)
                {
                    res +=$"\n    {_span.SourceName}:{_span.Line}:{_span.Col} {SyntaxKind} {_span.Text}"; 
                }
            }

            return res;
        }
        
        return $"{SyntaxKind}: {Source.Text}";
    }

    public string ToPrettyString()
    {
        return ToPrettyString(this);
    }

    private static string ToPrettyString(
        Node<TNode, TKind> node, 
        string indent = "", 
        bool isLast = true)
    {
        var builder = new StringBuilder();
        var marker = isLast ? "└──" : "├──";

        builder.Append($"{indent}{marker}{node.GetType().Name}");
        // $"{node.SyntaxKind}: {node.Source.Text}".Print("NODE");
        if (node is { IsLeaf: true, Source: not Nil }) builder.Append($" {node.Source.Text}");
        builder.Append('\n');
        indent += isLast ? "    " : "│   ";

        var last = node.Children.LastOrDefault();
        foreach (var child in node.Children)
        {
            builder.Append(ToPrettyString(child, indent, child == last));
        }

        return builder.ToString();
    }
    public override int GetHashCode()
    {
        return HashCode.Combine(SyntaxKind, Source);
    }

    public static bool operator ==(Node<TNode, TKind>? left, Node<TNode, TKind>? right)
    {
        return Equals(left, right);
    }

    public static bool operator !=(Node<TNode, TKind>? left, Node<TNode, TKind>? right)
    {
        return !Equals(left, right);
    }
    
    private bool Equals(Node<TNode, TKind> other)
    {
        return EqualityComparer<TKind>.Default.Equals(SyntaxKind, other.SyntaxKind);
    }

    public override bool Equals(object? obj)
    {
        if (ReferenceEquals(null, obj)) return false;
        if (ReferenceEquals(this, obj)) return true;
        if (obj.GetType() != GetType()) return false;
        return Equals((Node<TNode, TKind>)obj);
    }

}
