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

// MethodDeclarationSyntax methodDeclaration = SyntaxFactory
            //     .MethodDeclaration(SyntaxFactory.ParseTypeName("void"), "MyMethod")
            //     .AddModifiers(SyntaxFactory.Token(SyntaxKind.PublicKeyword))
            //     .WithBody(SyntaxFactory.Block(SyntaxFactory.LocalDeclarationStatement(
            //         SyntaxFactory.VariableDeclaration(SyntaxFactory.ParseTypeName("int"))
            //             .AddVariables(SyntaxFactory.VariableDeclarator(SyntaxFactory.Identifier("age"))
            //                 .WithInitializer(SyntaxFactory.EqualsValueClause(
            //                         SyntaxFactory.ArrayCreationExpression(
            //                             SyntaxFactory.ArrayType(
            //                                 SyntaxFactory.PredefinedType(SyntaxFactory.Token(SyntaxKind.IntKeyword)))
            //                         )
            //                     )
            //                 )
            //             )
            //     )));
            //
            // Console.WriteLine(methodDeclaration.NormalizeWhitespace().ToFullString());

namespace DelphiCSharp
{
    public static class Program
    {
        public static void Main(string[] args)
        {
            Try("big0");
        }

        private static string ReadAllInput(string fn)
        {
            return System.IO.File.ReadAllText(fn);
        }

        private static void Try(string fn)
        {
            // var input = ReadAllInput($"./examples/{fn}.pas");
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
