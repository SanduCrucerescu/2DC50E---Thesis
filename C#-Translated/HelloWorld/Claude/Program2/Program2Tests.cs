using System.Reflection;
using NUnit.Framework.Legacy;
using NUnit.Framework.Internal;
using NUnit.Framework;

namespace HelloWorld2.Tests
{
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

                HelloWorld.Main();

                var result = sw.ToString().Trim();
                ClassicAssert.AreEqual(Expected, result);
            }
        }
    }
}