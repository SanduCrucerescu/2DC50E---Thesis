namespace loops9; // was not created I did it manually

using System;

class Program
{
    static void Main()
    {
        InitArray();
        AddStudentInformation();
        ListStudents();
    }

    public struct Student
    {
        public string FirstName;
        public string LastName;
        public int AverageGrade;
    }

    internal static Student[] Students;

    internal static int CheckIfInteger(string input)
    {
        int response;
        while (!int.TryParse(input, out response) || response <= 0)
        {
            Console.WriteLine("Please write an integer, or press ENTER to exit.");
            input = Console.ReadLine();
            if (input == "")
            {
                Environment.Exit(0);
            }
        }
        return response;
    }

    internal static void InitArray()
    {
        Console.WriteLine("How many students are in the class?");
        string input = Console.ReadLine();
        int studentNumber = CheckIfInteger(input);
        Students = new Student[studentNumber];
    }

    internal static void AddStudentInformation()
    {
        for (int i = 0; i < Students.Length; i++)
        {
            Console.WriteLine($"Student {i + 1} first name:");
            Students[i].FirstName = Console.ReadLine();

            Console.WriteLine($"Student {i + 1} last name:");
            Students[i].LastName = Console.ReadLine();

            Console.WriteLine($"Student {i + 1} average grade:");
            Students[i].AverageGrade = CheckIfInteger(Console.ReadLine());
        }
    }

    internal static void ListStudents()
    {
        double averageGrade = 0;
        Console.WriteLine("First Name  |  Last Name  |  Average Grade");
        foreach (var student in Students)
        {
            Console.WriteLine($"{student.FirstName,12} | {student.LastName,12} | {student.AverageGrade,3}");
            averageGrade += student.AverageGrade;
        }
        averageGrade /= Students.Length;
        Console.WriteLine($"Average class grade: {averageGrade}");
    }
}