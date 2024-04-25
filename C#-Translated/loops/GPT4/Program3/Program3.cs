namespace loops8; // was not created I did it manually

using System;

class Program
{
    internal static Student[] Students;

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

    internal static int CheckIfInteger(string input)
    {
        int response = 0;
        do
        {
            bool isSuccess = int.TryParse(input, out response);
            if (!isSuccess || response <= 0)
            {
                Console.WriteLine("Please write an integer, or press ENTER to exit.");
                input = Console.ReadLine();
                if (string.IsNullOrEmpty(input))
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
            int parsedGrade = CheckIfInteger(averageGrade);

            Students[i] = new Student
            {
                FirstName = firstName,
                LastName = lastName,
                AverageGrade = parsedGrade
            };
        }
    }

    internal static void ListStudents()
    {
        Console.WriteLine("First Name  |  Last Name  |  Average Grade");
        double averageGrade = 0;
        foreach (var student in Students)
        {
            Console.WriteLine($"{student.FirstName,12} | {student.LastName,12} | {student.AverageGrade,3}");
            averageGrade += student.AverageGrade;
        }
        averageGrade /= Students.Length;
        Console.WriteLine($"Average class grade: {averageGrade}");
    }
}