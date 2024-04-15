namespace TestFixtures;

using System.Reflection;
using NUnit.Framework.Legacy;
using NUnit.Framework.Internal;

[Category("Template")]
[TestFixture]
public class HelloWorldTests
{
    private const string Expected = "Hello World from Pascal!";

    [Test]
    public void Should_Print_Hello_World(string p)
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