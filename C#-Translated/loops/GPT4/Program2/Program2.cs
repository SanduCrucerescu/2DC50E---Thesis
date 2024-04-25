namespace loops7; // was not created I did it manually

using System;

class Program
{
    static void Main()
    {
        InitArray();
        AddStudentInformation();
        ListStudents();
    }

    internal struct Student
    {
        public string FirstName;
        public string LastName;
        public int AverageGrade;
    }

    internal static Student[] Students;

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
            Console.WriteLine($"Student {i + 1} first name:");
            string firstName = Console.ReadLine();
            Console.WriteLine($"Student {i + 1} last name:");
            string lastName = Console.ReadLine();
            Console.WriteLine($"Student {i + 1} average grade:");
            string averageGrade = Console.ReadLine();
            Students[i] = new Student
            {
                FirstName = firstName,
                LastName = lastName,
                AverageGrade = CheckIfInteger(averageGrade)
            };
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