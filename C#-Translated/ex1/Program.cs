using System;
using NUnit.Framework;
using NUnit.Framework.Legacy;

namespace ex1
{
    class Program
    {
        public static void Main()
        {
            Console.WriteLine("Hello World from Pascal!");
            Console.ReadKey();
        }

        public static int AddInts(int x, int y)
        {
            return x + y;
        }
    }

    [TestFixture]
    class ProgramTests
    {
        private const string Expected = "Hello World from Pascal!";

        [Test]
        public void TestMainOutput()
        {
            // Redirect standard output to a string
            using (var sw = new StringWriter())
            {
                Console.SetOut(sw);

                Program.Main();

                var result = sw.ToString().Trim();
                ClassicAssert.AreEqual(Expected, result);
            }
        }
    }
}