namespace loops6; // was not created I did it manually

using System;

class Program
{
    internal class Student
    {
        public string FirstName { get; set; }
        public string LastName { get; set; }
        public int AverageGrade { get; set; }
    }

    internal static Student[] Students;

    internal static void Main(string[] args)
    {
        InitArray();
        AddStudentInformation();
        ListStudents();
    }

    internal static int CheckIfInteger(string input)
    {
        int response = 0;
        do
        {
            if (!int.TryParse(input, out response) || response <= 0)
            {
                Console.WriteLine("Please write an integer, or press ENTER to exit.");
                input = Console.ReadLine();
                if (input == "")
                {
                    Environment.Exit(0);
                }
            }
        } while (response <= 0);
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
            Students[i] = new Student();

            Console.WriteLine($"Student {i + 1} first name:");
            Students[i].FirstName = Console.ReadLine();

            Console.WriteLine($"Student {i + 1} last name:");
            Students[i].LastName = Console.ReadLine();

            Console.WriteLine($"Student {i + 1} average grade:");
            string averageGrade = Console.ReadLine();
            Students[i].AverageGrade = CheckIfInteger(averageGrade);
        }
    }

    internal static void ListStudents()
    {
        double totalGrade = 0;
        Console.WriteLine("First Name  |  Last Name  |  Average Grade");
        foreach (var student in Students)
        {
            Console.WriteLine($"{student.FirstName,12} | {student.LastName,12} | {student.AverageGrade,3}");
            totalGrade += student.AverageGrade;
        }
        double averageGrade = Students.Length > 0 ? totalGrade / Students.Length : 0;
        Console.WriteLine($"Average class grade: {averageGrade}");
    }
}