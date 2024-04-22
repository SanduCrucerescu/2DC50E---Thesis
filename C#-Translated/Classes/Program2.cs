// using System;
// using NUnit.Framework;
// using NUnit.Framework.Legacy;

// namespace Classes2
// {
//     public enum ShapeType { Triangle, Square, Rectangle, Cube }
//     public enum TriangleType { Acute, Obtuse, Right, Isosceles, Equilateral, Scalene }

//     // Main Class
//     public class Shape
//     {
//         /*
//             No errors found.
//         */
//         protected double Width, Height;
//         protected ShapeType SType;

//         public Shape(double W, double H, ShapeType T)
//         {
//             Width = W;
//             Height = H;
//             SType = T;
//         }

//         public void SetWidth(double W)
//         {
//             Width = W;
//         }

//         public double GetWidth()
//         {
//             return Width;
//         }

//         public void SetHeight(double H)
//         {
//             Height = H;
//         }

//         public double GetHeight()
//         {
//             return Height;
//         }

//         public void SetType(ShapeType T)
//         {
//             SType = T;
//         }

//         public ShapeType GetType()
//         {
//             return SType;
//         }

//         public virtual void Display()
//         {
//             Console.WriteLine($"Shape type: {SType}");
//             Console.WriteLine($"Shape width: {Width}");
//             Console.WriteLine($"Shape height: {Height}");
//         }
//     }

//     // Derived Class
//     public class TriangleObj : Shape
//     {
//         private TriangleType TType;

//         public TriangleObj(ShapeType T) : base(0.0, 0.0, T) { }

//         public TriangleObj(ShapeType T, TriangleType TT, double W, double H) : base(W, H, T)
//         {
//             TType = TT;
//         }

//         public void SetTType(TriangleType TT)
//         {
//             TType = TT;
//         }

//         public TriangleType GetTType()
//         {
//             return TType;
//         }

//         public override void Display()
//         {
//             Console.WriteLine($"Triangle type: {TType}");
//             Console.WriteLine($"Shape width: {Width}");
//             Console.WriteLine($"Shape height: {Height}");
//         }
//     }

//     class Program
//     {
//         static void Main(string[] args)
//         {
//             TriangleObj TObj = new TriangleObj(ShapeType.Triangle, TriangleType.Obtuse, 20.0, 35.9);
//             TObj.Display();
//         }
//     }

//     [TestFixture]
//     public class ClassesTests
//     {
//         private static double width = 10.2;
//         private static double height = 43.1;
//         private static TriangleObj TObj;

//         [SetUp]
//         public void SetUp()
//         {
//             TObj = new TriangleObj(ShapeType.Triangle, TriangleType.Obtuse, width, height);
//         }

//         [TearDown]
//         public void TearDown()
//         {
//             TObj = null;
//         }

//         [Test]
//         public void Should_Check_If_The_Object_Is_Created_Correctly()
//         {
//             ClassicAssert.AreEqual(width, TObj.GetWidth());
//             ClassicAssert.AreEqual(height, TObj.GetHeight());
//         }

//         [TestCase(12.2)]
//         [TestCase(64.34)]
//         [TestCase(543.43)]
//         public void Should_Check_If_The_Height_Is_Set_Correctly(double newHeight)
//         {
//             TObj.SetHeight(newHeight);
//             ClassicAssert.AreEqual(newHeight, TObj.GetHeight());
//         }

//         [TestCase(45.54)]
//         [TestCase(65.434)]
//         [TestCase(761.66)]
//         public void Should_Check_If_The_Width_Is_Set_Correctly(double newWidth)
//         {
//             TObj.SetWidth(newWidth);
//             ClassicAssert.AreEqual(newWidth, TObj.GetWidth());
//         }
//     }
// }