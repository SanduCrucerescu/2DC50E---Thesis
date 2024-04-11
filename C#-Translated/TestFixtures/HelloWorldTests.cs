namespace TestFixtures;

using System.Reflection;
using NUnit.Framework.Legacy;


public class HelloWorld
{
    private const string Expected = "Hello World from Pascal!";

    [TestCase("HelloWorld")]
    public void Should_Print_Hello_World(string np)
    {
        string fullyQualifiedClassName = String.Format("{0}.Program, {0}, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null", np);

        using (var runner = new DisposableRunner(fullyQualifiedClassName, "Main"))
        {
            var res = runner.GetOutput();
            ClassicAssert.AreEqual(Expected, res);
        }
    }
}