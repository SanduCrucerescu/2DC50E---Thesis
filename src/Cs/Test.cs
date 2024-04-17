namespace Foo
{
    internal class Context
    {
        internal int I = 0;
    }

    public class FooBaz
    {
        private readonly Context _local = new();

        public void Foo()
        {
            _local.I = 1;
            System.Console.Write(_local.I);
        }
    }
}

namespace Bar
{
    using Foo;
    
    public static class Program
    {
        public static void Foo()
        {
            var bar = new Space.Scope.Foo(); // Ok, exposed type
            Space.Scope.A = 10; // Ok, public non-constant static variable
            // Space.Scope.B = 10; // Error, although public, constant variable
            // Space.Scope.I // Error, private variable

        }
    }
}

namespace Space 
{
    public static class Scope
    {
        public static int A = 0;
        public const int B = 0;
        
        private static int I = 0;
        private const int Z = 0;
            
        public class Foo
        {
            public void Bar()
            {
                I = 1;
                System.Console.WriteLine(Z);
            }
        }
    }
}
