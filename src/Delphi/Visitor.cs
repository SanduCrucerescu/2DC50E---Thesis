using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using System.Threading.Channels;
using Antlr4.Runtime;
using Antlr4.Runtime.Misc;

namespace DelphiCSharp.Delphi;


public partial class Visitor : DelphiBaseVisitor<Node<DelphiNode, DelphiNodeKind>>
{
    public required DelphiLexer Lexer { get; init; }
    public required DelphiParser Parser { get; init; }
    public required LineAwareInputStream Stream { get; init; }
    public required Diagnostics Diagnostics { get; init; }

    [SetsRequiredMembers]
    public Visitor(string path)
    {
        var sourceText = System.IO.File.ReadAllText(path);
        var stream = new LineAwareInputStream(sourceText, path);
        
        var lexer = new DelphiLexer(stream);
        var tokenStream = new CommonTokenStream(lexer);
        var parser = new DelphiParser(tokenStream);

        var errorChannel = Channel.CreateUnbounded<Error>(
            new UnboundedChannelOptions {
                SingleReader = true,
                SingleWriter = false,
                AllowSynchronousContinuations = false,
            });
        
        lexer.RemoveErrorListeners();
        lexer.AddErrorListener(new ErrorListener<int>(errorChannel.Writer));
        parser.RemoveErrorListeners();
        parser.AddErrorListener(new ErrorListener<IToken>(errorChannel.Writer));

        Lexer = lexer;
        Parser = parser;
        Stream = stream;
        
        Diagnostics = new Diagnostics(errorChannel.Reader);
    }
}


public sealed class LineAwareInputStream : AntlrInputStream
{
    private string[] Lines { get; init; }

    [SetsRequiredMembers]
    public LineAwareInputStream(string input, string sourceName) : base(input)
    {
        name = sourceName;
        Lines = input.Replace("\r\n", "\n").Split('\n');
    }

    public string? GetLine(int line)
    {
        if (line < 0 || line >= Lines.Length) return null;
        return Lines[line];

    }
}