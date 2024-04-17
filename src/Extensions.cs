using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using Microsoft.CodeAnalysis;

namespace DelphiCSharp;
    
public static class DumpExtension
{
    public static void Panic(this object o)
    {
        o.Print();
        throw new Exception(o.ToString());
    }
    
    public static void Panic(this object o, string msg)
    {
        o.Print(msg);
        throw new Exception(o.ToString());
    }
    
    public static void Print(this object o) => o.Print(o.GetType());

    public static void Print(this object o, params object[] args)
    {
        var caller = new System.Diagnostics.StackFrame(1, true);
        var fileParts = caller.GetFileName()!.Split('/').ToList();
        var file = string.Join('/',
            fileParts.Count > 3 
                ? fileParts.GetRange(fileParts.Count - 3, 3) 
                : fileParts
        );
        var columnNumber = caller.GetFileColumnNumber();
        var callingMethod = caller.GetMethod()!.Name.Normalize();
        var lineNumber = caller.GetFileLineNumber();
        
        Console.Write($"[{file}:{lineNumber}:{columnNumber}] {callingMethod} ");
        foreach (var arg in args) Console.Write("[" + arg + "]");
        Console.WriteLine(" " + o);
    }

    public static void Write(this object o, string path)
    {
        using (StreamWriter file = new StreamWriter(path + ".txt"))
        {
            file.Write(o);
        }
    }
    
    public static void Write(this object o, string path, string ending)
    {
        using (StreamWriter file = new StreamWriter(path + "." + ending))
        {
            file.Write(o);
        }
    }
}

public static class ListExtension
{
    public static (List<T>, T) Pop<T>(this List<T> list)
    {
        var last = list[^1];
        list.RemoveAt(list.Count-1);
        return (list, last);
    }

    public static (List<T>, T) Pop<T>(this List<T> list, int ix)
    {
        var elem = list[ix];
        list.RemoveAt(ix);
        return (list, elem);
    }
}

public static class ImmutableArrayExtension
{
    public static (T[], T) Last<T>(this ImmutableArray<T> arr)
    {
        T[] subArr = new T[arr.Length - 1];
        Array.Copy(arr.ToArray(), 0, subArr, 0, arr.Length-1);
        return (subArr, arr[^1]);
    }

    public static (T, T[]) First<T>(this ImmutableArray<T> arr)
    {
        T[] subArr = new T[arr.Length - 1];
        Array.Copy(arr.ToArray(), 1, subArr, 0, arr.Length-1);
        return (arr[0], subArr);
    }
}