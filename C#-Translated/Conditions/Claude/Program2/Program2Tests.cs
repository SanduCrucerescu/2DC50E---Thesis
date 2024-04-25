using NUnit.Framework;
using NUnit.Framework.Legacy;

namespace Conditions2.Tests
{
    [TestFixture]
    public class ConditionsTests
    {
        private const string exptTrue = "The person is eligible for this position.";
        private const string exptFalse = "The person is underage!";

        [TestCase(89)]
        [TestCase(19)]
        public void Should_Return_True_If_Over_18(int x)
        {
            using (var sw = new StringWriter())
            {
                Console.SetOut(sw);

                Conditions.StudentAge(x);

                var result = sw.ToString().Trim();
                ClassicAssert.AreEqual(exptTrue, result);
            }
        }

        [TestCase(17)]
        [TestCase(2)]
        public void Should_Return_False_If_Under_18(int x)
        {
            using (var sw = new StringWriter())
            {
                Console.SetOut(sw);

                Conditions.StudentAge(x);

                var result = sw.ToString().Trim();
                ClassicAssert.AreEqual(exptFalse, result);
            }
        }
    }
}
