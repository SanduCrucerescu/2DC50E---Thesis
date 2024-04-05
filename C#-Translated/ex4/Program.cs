using System;
using NUnit.Framework;
using NUnit.Framework.Legacy;

public enum ShapeType { Triangle, Square, Rectangle, Cube }
public enum TriangleType { Acute, Obtuse, Right, Isosceles, Equilateral, Scalene }

// Main Class
public class Shape
{
    /*
        No errors found.
    */
    protected double Width, Height;
    protected ShapeType SType;

    public Shape(double W, double H, ShapeType T)
    {
        Width = W;
        Height = H;
        SType = T;
    }

    public void SetWidth(double W)
    {
        Width = W;
    }

    public double GetWidth()
    {
        return Width;
    }

    public void SetHeight(double H)
    {
        Height = H;
    }

    public double GetHeight()
    {
        return Height;
    }

    public void SetType(ShapeType T)
    {
        SType = T;
    }

    public ShapeType GetType()
    {
        return SType;
    }

    public virtual void Display()
    {
        Console.WriteLine($"Shape type: {SType}");
        Console.WriteLine($"Shape width: {Width}");
        Console.WriteLine($"Shape height: {Height}");
    }
}

// Derived Class
public class TriangleObj : Shape
{
    private TriangleType TType;

    public TriangleObj(ShapeType T) : base(0.0, 0.0, T) { }

    public TriangleObj(ShapeType T, TriangleType TT, double W, double H) : base(W, H, T)
    {
        TType = TT;
    }

    public void SetTType(TriangleType TT)
    {
        TType = TT;
    }

    public TriangleType GetTType()
    {
        return TType;
    }

    public override void Display()
    {
        Console.WriteLine($"Triangle type: {TType}");
        Console.WriteLine($"Shape width: {Width}");
        Console.WriteLine($"Shape height: {Height}");
    }
}

class Program
{
    static void Main(string[] args)
    {
        TriangleObj TObj = new TriangleObj(ShapeType.Triangle, TriangleType.Obtuse, 20.0, 35.9);
        TObj.Display();
    }
}


[TestFixture]
class ProgramTests
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