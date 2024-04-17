using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using Antlr4.Runtime;
using Antlr4.Runtime.Misc;
using Antlr4.Runtime.Tree;
using Antlr4.Runtime.Tree.Pattern;

namespace DelphiCSharp;


public abstract class Source
{
    public virtual string Text => "";
    public abstract SpanList With(Source other);

    public static Leaf Of(IToken token)
    {
        return new Leaf
        {
            Token = token,
        };
    }
    
    public static Span Of(ParserRuleContext cx)
    {
        return new Span
        {
            Start = cx.Start,
            Stop = cx.Stop,
        };
    }

    public static SpanList Of(IEnumerable<ParserRuleContext> cxs)
    {
        return new SpanList
        {
            Sources = cxs.Select(cx => (Source)Of(cx)).ToList(),
        };
    }

    public static Nil Nil => new();
    public static Conjured Conjured => new();
    
    public static Source operator +(Source first, Source second)
    {
        return first.With(second);
    }
}

public sealed class Nil : Source
{
    public override string Text => "No Source";
    public override SpanList With(Source other)
    {
        return [other];
    }
}

public sealed class Conjured : Source
{
    public override string Text => "Generated";
    public override SpanList With(Source other)
    {
        return [this];
    }
}

public sealed class Leaf : Source
{
    public required IToken Token { get; init; }

    public override SpanList With(Source other)
    {
        if (other is Nil)
        {
            return [this];
        }
        
        if (other is SpanList l)
        {
            return l.With(this);
        }

        return [this, other];
    }

}

public sealed class Span : Source
{
    public required IToken Start { get; init; }
    public required IToken Stop { get; init; }
    private Interval Interval => Interval.Of(Start.StartIndex, Stop.StopIndex);
    public override string Text => Start.InputStream.GetText(Interval);
    
    public int Line => Start.Line;
    public int Col => Start.Column;
    public int Length => Stop.StopIndex - Start.StartIndex;
    public string SourceName => Start.InputStream.SourceName;
    public ITokenSource TokenSource => Start.TokenSource;
    public ICharStream CharStream => Start.InputStream;

    public override SpanList With(Source other)
    {
        if (other is Nil)
        {
            return [this];
        }
        
        if (other is SpanList l)
        {
            return l.With(this);
        }
        return [this, other];
    }
}

public sealed class SpanList : Source, IEnumerable<Source>
{
    public List<Source> Sources { get; init; } = [];


    public override string Text => ToString(", ");
    public string ToString(string sep) => string.Join(sep, Sources.Select(t => t.Text));
    
    public static SpanList From(IEnumerable<Source> spans)
    {
        return new SpanList
        {
            Sources = spans.ToList()
        };
    }
    
    public IEnumerator<Source> GetEnumerator()
    {
        return Sources.GetEnumerator();
    }

    IEnumerator IEnumerable.GetEnumerator()
    {
        return GetEnumerator();
    }

    public void Add(Source other)
    {
        Sources.Add(other);
    }

    public override SpanList With(Source other)
    {
        if (other is Nil)
        {
            return this;
        }
        Add(other);
        return this;
    }
}


// public sealed record Source
// {
//     public int Line { get; private init; }
//     public int Col { get; private init; }
//     private IToken? Start { get; set; }
//     private IToken? End { get; init; }
//     public string Text { get; private set; }
//
//     public static Source Of(ParserRuleContext cx)
//     {
//         return new Source
//         {
//             Line = cx.Start.Line,
//             Col = cx.Start.Column,
//             Start = cx.Start,
//             End = cx.Stop,
//             Text = cx.GetText(),
//         };
//     }
//
//     public static Source Of(IToken token)
//     {
//         return new Source
//         {
//             Line = token.Line,
//             Col = token.Column,
//             Start = token,
//             End = token,
//             Text = token.Text,
//         };
//     }
//
//     public static Source Of(IEnumerable<ParserRuleContext> cxs)
//     {
//         var ruleContexts = cxs as ParserRuleContext[] ?? cxs.ToArray();
//         var ls = ruleContexts.ToList();
//         if (ls.Count != 0) return Nil;
//         return new Source
//         {
//             Line = ls.First().Start.Line,
//             Col = ls.First().Start.Column,
//             Start = ls.First().Start,
//             End = ls.First().Stop,
//             Text = string.Join('\n', ruleContexts.Select(cx => cx.GetText()))
//         };
//     }
//
//     public static readonly Source Nil = new()
//     {
//         Line = -1, Col = -1,
//         Start = null, End = null,
//         Text = ""
//     };
//
//     public bool IsNil()
//     {
//         return Line == -1 && Col == -1 && Text == "";
//     }
//
//     public static Source operator +(Source first, Source second)
//     {
//         $"{first.Text} + {second.Text}".Cout("ADDING");
//         return first with
//         {
//             End = second.End,
//             Text = first.Text + second.Text,
//         };
//     }
//     
//     // public void Merge(Source other)
//     // {
//     //     if (IsNil())
//     //     {
//     //         Line = other.Line;
//     //         Col = other.Col;
//     //         Start = other.Start;
//     //         End = other.End;
//     //         Text = other.Text;
//     //         return;
//     //     }
//     //     Line = Math.Min(Line, other.Line);
//     //     Col = Math.Min(Col, other.Col);
//     //     Start = Math.Min(Start, other.Start);
//     //     End = Math.Max(End, other.End);
//     //     Text = Start < other.Start ? Text + other.Text : other.Text + Text;
//     // }
// } 
