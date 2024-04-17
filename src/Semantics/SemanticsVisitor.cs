using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using DelphiCSharp.Cs;
using DelphiCSharp.Delphi;
using MethodDecl = DelphiCSharp.Delphi.MethodDecl;
using MethodHead = DelphiCSharp.Delphi.MethodHead;
using TypeDecl = DelphiCSharp.Delphi.TypeDecl;

namespace DelphiCSharp.Semantics;

public sealed class SemanticsVisitor : DelphiVisitor
{
    public Scope Root = new("Global") { Symbols = [] };
    public Stack<Scope> Scopes { get; init; } = [];
    

    // public Symbol? ResolveSymbol(string sym)
    // {
    //     foreach (var scope in Scopes)
    //     {
    //         if (scope.Symbols.TryGetValue(new Key(sym, scope.Key), out var symbol))
    //         {
    //             return symbol;
    //         }
    //     }
    //
    //     return null;
    // }

    public Symbol? ResolveSymbol(Key key)
    {
        foreach (var scope in Scopes)
        {
            if (scope.Symbols.TryGetValue(key, out var symbol))
            {
                return symbol;
            }
        }

        return null;
    }

    public void EnterScope(string name) => Scopes.Push(new Scope(name));
    public Scope ExitScope() => Scopes.Pop();

    private class Scoper : IDisposable
    {
        private Stack<Scope> _scopes;
        public Key? Parent { get; init; }

        public Scoper(Stack<Scope> scopes, string name)
        {
            if (scopes.TryPeek(out var parent)) Parent = parent.Key;
            _scopes = scopes;
            _scopes.Push(Parent is not null ? new Scope(name, Parent) : new Scope(name));
        }
        
        public Scoper(Stack<Scope> scopes, string name, DelphiNode associatedNode)
        {
            if (scopes.TryPeek(out var parent)) Parent = parent.Key;
            _scopes = scopes;
            _scopes.Push(Parent is not null ? new Scope(name, Parent)
            {
                AssociatedNode = associatedNode,
            } : new Scope(name)
            {
                AssociatedNode = associatedNode
            });
        }
        
        public Scoper(Stack<Scope> scopes, string name, Key parent)
        {
            Parent = parent;
            _scopes = scopes;
            _scopes.Push(new Scope(name, parent));
        }
        
        public void Dispose()
        {
            _scopes.Pop();
        }
    }

    public override void VisitProgram(Delphi.Program cx)
    {
        foreach (var ns in cx.Namespace.Segments)
        {
            Scopes.Push(Scopes.TryPeek(out var parent) ? new Scope(ns.Text, parent.Key) : new Scope(ns.Text));
        }
        
        base.VisitProgram(cx);
       
        foreach (var _ in cx.Namespace.Segments)
        {
            Scopes.Pop();
        }
    }

    public override void VisitUnit(Unit cx)
    {
        foreach (var ns in cx.Head.Namespace.Segments)
        {
            Scopes.Push(Scopes.TryPeek(out var parent) ? new Scope(ns.Text, parent.Key) : new Scope(ns.Text));
        }
        
        base.VisitUnit(cx);
        
        foreach (var ns in cx.Head.Namespace.Segments)
        {
            Scopes.Pop();
        }
    }

    public override void VisitTypeDecl(TypeDecl cx)
    {
        using (var scope = new Scoper(Scopes, cx.Name.Text))
        {
            var key = new Key(cx.Name.Text, scope.Parent!);
            Root.Symbols.Add(key, new TypeSymbol(key)
            {
                AssociatedNode = cx
            });
            VisitTypeExpr(cx.TypeExpr);
        }
    }

    public override void VisitFuncHead(FuncHead cx)
    {
        using (var scope = new Scoper(Scopes, cx.Name.Text))
        {
            if (Root.Symbols.TryGetValue(new Key(cx.Name.Text, scope.Parent!), out var sym))
            {
                if (sym is FuncSymbol func)
                {
                    func.FuncSignatures.Add(cx.Signature);
                }

                return;
            }
            Root.Symbols.Add(new Key(cx.Name.Text), new FuncSymbol(cx.Name.Text, scope.Parent!));
        }
    }

    public override void VisitMethodDecl(MethodDecl cx)
    {
        var names = new Queue<string>(cx.Head.Name.Segments.Aggregate(new List<string>(), (ls, seg) =>
        {
            ls.AddRange([..seg.QualifiedIdent.Segments.Select(s => s.Text), seg.QualifiedIdent.Ident.Text]);
            return ls;
        }));
        
        var method = names.Dequeue();
        var methodKey = new Key(method, Scopes.Peek().Key);
        
        using (new Scoper(Scopes, method))
        {
            if (names.TryPeek(out _))
            {
                while (names.TryDequeue(out var name))
                {
                    methodKey = new Key(name, methodKey);
                }

                var hasDecl = Root.Symbols.TryGetValue(methodKey, out var declaration);

                if (!hasDecl || hasDecl && declaration is not MethodSymbol)
                {
                    $"Unknown Method, {method} was never declared".Panic("ERROR");
                    return;
                }

                var methodDecl = (MethodSymbol)declaration!;
                
                for (var ix = 0; ix < methodDecl.AssociatedNodes.Count; ix++)
                {
                    if (cx.Head.Signature == methodDecl.AssociatedNodes[ix].Head.Signature)
                    {
                        methodDecl.AssociatedNodes[ix] = cx;
                        return;
                    }
                }
                
                $"Unknown signature {cx.Head.Source.Text}".Panic("ERROR");
                return;
            }
            
            if (Root.Symbols.TryGetValue(methodKey, out var sym))
            {
                if (sym is MethodSymbol methodSymbol)
                {
                    if (methodSymbol.AssociatedNodes.Any(met => met.Head.Signature == cx.Head.Signature))
                    {
                        "Function already exists".Panic("ERROR");
                    }
                    methodSymbol.AssociatedNodes.Add(cx);

                    return;
                }
            }
            
            Root.Symbols.Add(methodKey, new MethodSymbol(methodKey, cx));
            VisitFormalParamsSection(cx.Head.Signature.Params);
        }
    }
}

public sealed class Scope()
{
    public required Key Key { get; init; }
    public required Dictionary<Key, Symbol> Symbols { get; init; } = [];
    public required DelphiNode? AssociatedNode { get; init; }

    [SetsRequiredMembers]
    public Scope(string name, Key parent) : this()
    {
        Key = new Key(name, parent);
        AssociatedNode = null;
    }
    
    [SetsRequiredMembers]
    public Scope(string name) : this()
    {
        Key = new Key(name);
        AssociatedNode = null;
    }

    public Symbol? Resolve(Key key)
    {
        return Symbols.GetValueOrDefault(key);
    }

    public TSymbol? Resolve<TSymbol>(string name, string parent) where TSymbol : Symbol
    {
        foreach (var kvp in Symbols)
        {
            if (kvp.Key.Name == name && kvp.Key.Parent is { } par && par.Name == parent)
            {
                return kvp.Value as TSymbol;
            }
        }

        return default;
    }

    public Symbol? Resolve(string name, string parent)
    {
        foreach (var kvp in Symbols)
        {
            if (kvp.Key.Name == name && kvp.Key.Parent is { } par && par.Name == parent)
            {
                // ((MethodSymbol)kvp.Value).AssociatedNodes.ForEach(m => m.Print("METHOD"));
                return kvp.Value;
            }
        }

        return default;
    }
}

public sealed class Key
{
    public required string Name { get; init; }
    public required Key? Parent { get; init; }

    [SetsRequiredMembers]
    public Key(string name)
    {
        Name = name;
        Parent = null;
    }

    [SetsRequiredMembers]
    public Key(string name, Key parent)
    {
        Name = name;
        Parent = parent;
    }
    
    private bool Equals(Key other)
    {
        return Name == other.Name && Equals(Parent, other.Parent);
    }

    public override bool Equals(object? obj)
    {
        return ReferenceEquals(this, obj) || obj is Key other && Equals(other);
    }

    public override int GetHashCode()
    {
        return HashCode.Combine(Name, Parent);
    }

    public static bool operator ==(Key? left, Key? right)
    {
        return Equals(left, right);
    }

    public static bool operator !=(Key? left, Key? right)
    {
        return !Equals(left, right);
    }

    public override string ToString()
    {
        return Parent is not null ? $"{Name} <- {Parent}" : Name;
    }
}

public abstract class Symbol
{
    public required string Name { get; init; }
    public required Key? Parent { get; init; }
    public required List<Key> Usages { get; init; } = [];

    [SetsRequiredMembers]
    public Symbol(Key key)
    {
        Name = key.Name;
        Parent = key.Parent;
    }
    
    [SetsRequiredMembers]
    public Symbol(string name)
    {
        Name = name;
        Parent = null;
    }

    [SetsRequiredMembers]
    public Symbol(string name, Key parent) : this(name)
    {
        Parent = parent;
    }
}

[method: SetsRequiredMembers]
public sealed class TypeSymbol(Key key) : Symbol(key)
{
    public required TypeDecl? AssociatedNode { get; init; }
}

[method: SetsRequiredMembers]
public sealed class NamespaceSymbol(string name) : Symbol(name);
[method: SetsRequiredMembers]
public sealed class VariableSymbol(string name) : Symbol(name);

public sealed class FuncSymbol : Symbol
{
    public required List<FuncSignature> FuncSignatures { get; init; } = [];

    [SetsRequiredMembers]
    public FuncSymbol(string name, Key parent) : base(name, parent)
    {
    }
}

public sealed class MethodSymbol : Symbol
{
    public required List<Delphi.MethodDecl> AssociatedNodes { get; init; } = [];

    [SetsRequiredMembers]
    public MethodSymbol(Key key, MethodDecl associatedNode) : base(key)
    {
        AssociatedNodes = [associatedNode];
    }
    
    [SetsRequiredMembers]
    public MethodSymbol(string name, Key parent, MethodDecl associatedNode) : base(name, parent)
    {
        AssociatedNodes = [associatedNode];
    }
}

