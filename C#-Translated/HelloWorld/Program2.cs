using NUnit.Framework;
using NUnit.Framework.Legacy;

namespace HelloWorld2;

public class Program
{
    public static void Main()
    {
        Console.WriteLine("Hello World from Pascal!");
        Console.ReadKey();
    }

    [TestFixture]
    public class HelloWorldTests
    {
        private const string Expected = "Hello World from Pascal!";

        [Test]
        public void Should_Print_Hello_World()
        {
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
