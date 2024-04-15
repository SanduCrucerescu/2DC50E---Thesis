namespace TestFixtures;

using System.IO.Pipes;
using System.Runtime.Intrinsics.Arm;
using NUnit.Framework.Legacy;

[Category("Template")]
[TestFixture]
public class ClassesTests
{
    private static double width = 10.2;
    private static double height = 43.1;
    private static TriangleObj TObj;

    [SetUp]
    public void SetUp()
    {
        TObj = new TriangleObj(ShapeType.Triangle, TriangleType.Obtuse, width, height);
    }

    [TearDown]
    public void TearDown()
    {
        TObj = null;
    }

    [Test]
    public void Should_Check_If_The_Object_Is_Created_Correctly()
    {
        ClassicAssert.AreEqual(width, TObj.GetWidth());
        ClassicAssert.AreEqual(height, TObj.GetHeight());
    }

    [TestCase(12.2)]
    [TestCase(64.34)]
    [TestCase(543.43)]
    public void Should_Check_If_The_Height_Is_Set_Correctly(double newHeight)
    {
        TObj.SetHeight(newHeight);
        ClassicAssert.AreEqual(newHeight, TObj.GetHeight());
    }

    [TestCase(45.54)]
    [TestCase(65.434)]
    [TestCase(761.66)]
    public void Should_Check_If_The_Width_Is_Set_Correctly(double newWidth)
    {
        TObj.SetWidth(newWidth);
        ClassicAssert.AreEqual(newWidth, TObj.GetWidth());
    }
}


// public static IEnumerable<TestCaseData> TestCases()
// {
//     var source = new[] {
//     (ITriangleObj)new Classes1.TriangleObj(Classes1.ShapeType.Triangle, Classes1.TriangleType.Obtuse, width, height),
//     (ITriangleObj)new Classes2.TriangleObj(Classes2.ShapeType.Triangle, Classes2.TriangleType.Obtuse, width, height),
// };

//     foreach (var y in source)
//     {
//         yield return new TestCaseData(y, 23.0, 75.2);
//         yield return new TestCaseData(y, 64.54, 1110.543);
//         yield return new TestCaseData(y, 256.74, 876.8);
//     }
// }
