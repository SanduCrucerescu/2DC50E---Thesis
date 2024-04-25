// Template generated code from Antlr4BuildTasks.Template v 8.17

using System;
using System.Diagnostics;
using System.Linq;
using System.Text;
using DelphiCSharp;
using DelphiCSharp.Cs;
using DelphiCSharp.Delphi;
using DelphiCSharp.Gen;
using DelphiCSharp.Semantics;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Visitor = DelphiCSharp.Delphi.Visitor;

namespace DelphiCSharp
{
    public static class Program
    {
        public static void Main(string[] args)
        {
            Try("class2");
            // if (args.Length != 1)
            // {
            //     "program <filename>".Print("USAGE");
            //     return;
            // }
            
            // Compile(args[0]);
        }

        private static void Compile(string file)
        {
            var name = file.Split('.')[0];
            var vis = new Visitor($"{name}.pas");
            var cx = vis.Parser.file();
            if (vis.Diagnostics.HasError())
            {
                vis.Diagnostics.Dump();
            }
            var delphiAst = vis.Visit(cx);
            var sem = new SemanticsVisitor();
            sem.Visit(delphiAst);
            var delphiWalker = new DelphiWalker(sem.Root);
            var csAst = delphiWalker.Visit(delphiAst);
            var csWalker = new CsWalker();
            var csCode = csWalker.Visit(csAst);
            csCode.NormalizeWhitespace().ToFullString().Write(name, "cs");
        }

        private static void Try(string fn)
        {
            var vis = new Visitor($"./examples/{fn}.pas");
            var cx = vis.Parser.file();

            if (vis.Diagnostics.HasError())
            {
                vis.Diagnostics.Dump();
            }

            var t = vis.Visit(cx);
            // t.ToPrettyString().Write("delphi");
            // t.ToPrettyString().Print("DELPHI SYNTAX");

            var sem = new SemanticsVisitor();
            sem.Visit(t);
            string.Join("", sem.Root.Symbols.Select(kvp => $"\n\t{kvp.Key.Name}: {kvp.Value.Parent!.Name}")).Print("Symbols");
            
            var delphiWalker = new DelphiWalker(sem.Root);
            var res = delphiWalker.Visit(t);
            // res.ToPrettyString().Write("csharp");
            // res.ToPrettyString().Print("CS SYNTAX");
            
            var csWalker = new CsWalker();
            var csCode = csWalker.Visit(res);
            // csCode.NormalizeWhitespace().ToFullString().Write("code", "cs");
            csCode.NormalizeWhitespace().ToFullString().Print();
        }
    }

}
