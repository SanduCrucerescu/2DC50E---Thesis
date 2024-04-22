﻿using System;

namespace Classes8
{
    public enum ShapeType { Triangle, Square, Rectangle, Cube };
    public enum TriangleType { Acute, Optuse, Right, Isosceles, Equilateral, Scalene };

    public class Shape
    {
        protected double Width, Height;
        protected ShapeType SType;

        public Shape(double W, double H, ShapeType T)
        {
            Width = W;
            Height = H;
            SType = T;
        }

        public void SetWidth(double W) => Width = W;
        public double GetWidth() => Width;

        public void SetHeight(double H) => Height = H;
        public double GetHeight() => Height;

        public void SetType(ShapeType T) => SType = T;
        public ShapeType GetType() => SType;

        public virtual void Display()
        {
            Console.WriteLine($"Shape type: {SType}");
            Console.WriteLine($"Shape width: {Width}");
            Console.WriteLine($"Shape height: {Height}");
        }
    }

    public class TriangleObj : Shape
    {
        private TriangleType TType;

        public TriangleObj(ShapeType T) : base(0.0, 0.0, T)
        {
        }

        public TriangleObj(ShapeType T, TriangleType TT, double W, double H) : base(W, H, T)
        {
            TType = TT;
        }

        public void SetTType(TriangleType TT) => TType = TT;
        public TriangleType GetTType() => TType;

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
            TriangleObj TObj = new TriangleObj(ShapeType.Triangle, TriangleType.Optuse, 20.0, 35.9);
            TObj.Display();
        }
    }
}