namespace TestFixtures;

using NUnit.Framework.Legacy;

public class ConditionsTests
{
    private const string exptTrue = "The person is eligible for this position.";
    private const string exptFalse = "The person is underage!";

    [TestCase(89, "Conditions")]
    [TestCase(19, "Conditions")]
    public void Should_Return_True_If_Over_18(int x, string np)
    {
        string fullyQualifiedClassName = String.Format("{0}.Program, {0}, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null", np);
        using (var runner = new DisposableRunner(fullyQualifiedClassName, "StudentAge", [x]))
        {
            var res = runner.GetOutput();
            ClassicAssert.AreEqual(exptTrue, res);
        }
    }

    [TestCase(17, "Conditions")]
    [TestCase(2, "Conditions")]
    public void Should_Return_False_If_Under_18(int x, string np)
    {
        string fullyQualifiedClassName = String.Format("{0}.Program, {0}, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null", np);
        using (var runner = new DisposableRunner(fullyQualifiedClassName, "StudentAge", [x]))
        {
            var res = runner.GetOutput();
            ClassicAssert.AreEqual(exptFalse, res);
        }
    }
}
