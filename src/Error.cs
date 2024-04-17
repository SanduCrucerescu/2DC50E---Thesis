#nullable enable
using System;
using System.Diagnostics.CodeAnalysis;
using System.IO;
using System.Threading;
using System.Threading.Channels;
using System.Threading.Tasks;
using Antlr4.Runtime;

namespace DelphiCSharp.Delphi;

public sealed class Diagnostics(ChannelReader<Error> reader)
{
    public bool HasError()
    {
        return reader.TryPeek(out _);
    }

    public async void Dump()
    {
        await foreach (var err in reader.ReadAllAsync())
        {
            err.Dump();
        }
    }
    
    public async Task<Error> ReadOne()
    { 
        return await reader.ReadAsync();
    }

    public async Task<Error?> TryReadOne()
    { 
        try
        {
            var error = await reader.ReadAsync();
            return error;
        }
        catch (ChannelClosedException)
        {
            Console.WriteLine("[ERROR] Found Nothing.");
            return null;
        }
    }
}

public sealed record Error(
    int Line,
    int Col,
    string Message,
    RecognitionException Exception,
    ErrorKind Kind)
{

    public override string ToString()
    {
        return $"[{Kind} Error] {Line}:{Col} - {Message}";
    }

    public void Dump()
    {
        Console.WriteLine(ToString());
    }
}

public enum ErrorKind
{
    Syntax
}

public class ErrorListener<TSymbol>(ChannelWriter<Error> writer): IAntlrErrorListener<TSymbol>
{
    public void SyntaxError(
        TextWriter output, 
        IRecognizer recognizer, 
        TSymbol offendingSymbol, 
        int line, 
        int charPositionInLine,
        string msg, 
        RecognitionException e)
    {
        if (!writer.TryWrite(new Error(
            Line: line,
            Col: charPositionInLine,
            Message: msg,
            Exception: e,
            Kind: ErrorKind.Syntax)))
        {
            Console.WriteLine("[ERROR] Writing To Error Channel.");
        }
    }
}