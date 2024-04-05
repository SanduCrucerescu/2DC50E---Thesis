using System;
using System.Collections.Generic;
using NUnit.Framework;
using NUnit.Framework.Legacy;
class Program
{

    /**
        The code had a List but since we need a fixed array the variable needed to be changed to array
        Time: <1min;
    */

    class Student
    {
        public string FirstName { get; set; }
        public string LastName { get; set; }
        public int AverageGrade { get; set; }
    }

    static int CheckIfInteger(string Input)
    {
        int Response;
        while (true)
        {
            if (int.TryParse(Input, out Response))
            {
                if (Response > 0)
                    return Response;
            }
            Console.WriteLine("Please write an integer, or press ENTER to exit.");
            Input = Console.ReadLine();
            if (string.IsNullOrEmpty(Input))
                Environment.Exit(0);
        }
    }

    static void InitArray(out Student[] Students)
    {
        Console.WriteLine("How many students are in the class?");
        string Input = Console.ReadLine();
        int StudentNumber = CheckIfInteger(Input);
        Students = new Student[StudentNumber];
    }

    static void AddStudentInformation(Student[] Students)
    {
        for (int i = 0; i < Students.Length; i++)
        {
            Console.WriteLine($"Student {i + 1} first name:");
            string FirstName = Console.ReadLine();
            Console.WriteLine($"Student {i + 1} last name:");
            string LastName = Console.ReadLine();
            Console.WriteLine($"Student {i + 1} average grade:");
            string AverageGrade = Console.ReadLine();
            Students[i] = new Student
            {
                FirstName = FirstName,
                LastName = LastName,
                AverageGrade = CheckIfInteger(AverageGrade)
            };
        }
    }

    static void ListStudents(Student[] Students)
    {
        Console.WriteLine("First Name  |  Last Name  |  Average Grade");
        double AverageGrade = 0;
        foreach (var student in Students)
        {
            Console.WriteLine($"{student.FirstName.PadRight(12)} | {student.LastName.PadRight(12)} | {student.AverageGrade.ToString("D3")}");
            AverageGrade += student.AverageGrade;
        }
        AverageGrade /= Students.Length;
        Console.WriteLine($"Average class grade: {AverageGrade}");
    }

    static void Main(string[] args)
    {
        Student[] Students;
        InitArray(out Students);
        AddStudentInformation(Students);
        ListStudents(Students);
    }

    [TestFixture]
    class ProgramTests
    {

        [TestCase("2")]
        [TestCase("10")]
        public void Should_Check_If_The_Array_Has_The_Right_Size(string x)
        {
            // Redirect standard output to a string
            using (var sw = new StringWriter())
            {
                Student[] students;

                StringReader sr = new StringReader(x);
                Console.SetIn(sr);

                InitArray(out students);

                ClassicAssert.IsNotNull(students);
                ClassicAssert.AreEqual(Int32.Parse(x), students.Length);

                sr.Dispose();
            }
        }

        [Test]
        public void Should_Test_If_Students_Are_Added()
        {

            Student[] students = new Student[2];

            string[] simulatedInput = { "John", "Doe", "90", "Jane", "Smith", "85" };
            StringReader sr = new StringReader(string.Join(Environment.NewLine, simulatedInput));
            Console.SetIn(sr);

            AddStudentInformation(students);

            ClassicAssert.IsNotNull(students);
            ClassicAssert.AreEqual(2, students.Length);


            ClassicAssert.AreEqual("John", students[0].FirstName);
            ClassicAssert.AreEqual("Doe", students[0].LastName);
            ClassicAssert.AreEqual(90, students[0].AverageGrade);


            ClassicAssert.AreEqual("Jane", students[1].FirstName);
            ClassicAssert.AreEqual("Smith", students[1].LastName);
            ClassicAssert.AreEqual(85, students[1].AverageGrade);

            sr.Dispose();
        }
    }
}