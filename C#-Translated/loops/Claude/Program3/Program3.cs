namespace loops3; // was not created I did it manually

using System;
using System.Collections.Generic;
class Program
{
    internal struct Student
    {
        public string FirstName;
        public string LastName;
        public int AverageGrade;
    }

    internal static List<Student> Students = new List<Student>();

    internal static int CheckIfInteger(string Input)
    {
        int Response = 0;
        while (true)
        {
            if (!int.TryParse(Input, out Response))
            {
                Console.WriteLine("Please write an integer, or press ENTER to exit.");
                Input = Console.ReadLine();
                if (string.IsNullOrEmpty(Input))
                {
                    Environment.Exit(0);
                }
            }
            else
            {
                break;
            }
        }

        return Response;
    }

    internal static void InitArray()
    {
        Console.WriteLine("How many students are in the class?");
        string Input = Console.ReadLine();
        int StudentNumber = CheckIfInteger(Input);
        Students = new List<Student>(StudentNumber - 1);
    }

    internal static void AddStudentInformation()
    {
        for (int i = 0; i < Students.Capacity; i++)
        {
            Student StudentTemp;

            Console.WriteLine($"Student {i + 1} first name:");
            string FirstName = Console.ReadLine();
            StudentTemp.FirstName = FirstName;

            Console.WriteLine($"Student {i + 1} last name:");
            string LastName = Console.ReadLine();
            StudentTemp.LastName = LastName;

            Console.WriteLine($"Student {i + 1} average grade:");
            string AverageGrade = Console.ReadLine();
            StudentTemp.AverageGrade = CheckIfInteger(AverageGrade);

            Students.Add(StudentTemp);
        }
    }

    internal static void ListStudents()
    {
        Console.WriteLine("First Name  |  Last Name  |  Average Grade");
        double AverageGrade = 0;

        foreach (Student student in Students)
        {
            Console.Write($"{student.FirstName,12} | ");
            Console.Write($"{student.LastName,12} | ");
            Console.WriteLine($"{student.AverageGrade,3}");
            AverageGrade += student.AverageGrade;
        }

        AverageGrade /= Students.Count;
        Console.WriteLine($"Average class grade: {AverageGrade}");
    }

    static void Main(string[] args)
    {
        InitArray();
        AddStudentInformation();
        ListStudents();
    }
}