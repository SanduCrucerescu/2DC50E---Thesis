namespace loops2; // was not created I did it manually

using System;
using System.Collections.Generic;
class Program
{
    struct Student
    {
        public string FirstName;
        public string LastName;
        public int AverageGrade;
    }
    static List<Student> Students;

    static int CheckIfInteger(string Input)
    {
        int Response = 0;
        do
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
        } while (Response <= 0);

        return Response;
    }

    static void InitArray()
    {
        Console.WriteLine("How many students are in the class?");
        string Input = Console.ReadLine();
        int StudentNumber = CheckIfInteger(Input);
        Students = new List<Student>(StudentNumber);
    }

    static void AddStudentInformation()
    {
        for (int i = 0; i < Students.Capacity; i++)
        {
            Student StudentTemp = new Student();

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

    static void ListStudents()
    {
        Console.WriteLine("First Name  |  Last Name  |  Average Grade");
        double AverageGrade = 0;
        foreach (Student s in Students)
        {
            Console.Write($"{s.FirstName,-12} | {s.LastName,-12} | {s.AverageGrade,3}");
            Console.WriteLine();
            AverageGrade += s.AverageGrade;
        }
        AverageGrade /= Students.Count;
        Console.WriteLine($"Average class grade: {AverageGrade}");
    }

    static void Main()
    {
        InitArray();
        AddStudentInformation();
        ListStudents();
    }
}