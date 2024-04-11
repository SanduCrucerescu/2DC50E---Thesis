namespace TestFixtures;
using Classes;
using NUnit.Framework.Legacy;

public class ClassesTests
{
    [TestCase(23.0, 75.2)]
    [TestCase(64.54, 1110.543)]
    [TestCase(256.74, 876.8)]
    public void Should_Check_If_The_Data_Is_Set_Correctly(double newWidth, double newHeight)
    {
        double width = 10.2, height = 43.1;
        TriangleObj TObj1 = new TriangleObj(ShapeType.Triangle, TriangleType.Obtuse, width, height);
        ClassicAssert.AreEqual(width, TObj1.GetWidth());
        ClassicAssert.AreEqual(height, TObj1.GetHeight());

        TObj1.SetWidth(newWidth);
        ClassicAssert.AreEqual(newWidth, TObj1.GetWidth());

        TObj1.SetWidth(newWidth);
        ClassicAssert.AreEqual(newWidth, TObj1.GetWidth());
    }
}
