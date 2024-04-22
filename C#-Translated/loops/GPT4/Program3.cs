namespace loops8; // was not created I did it manually

using System;

class Program
{
    struct Student
    {
        public string FirstName;
        public string LastName;
        public int AverageGrade;
    }

    static Student[] Students;

    static int CheckIfInteger(string input)
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

    static void InitArray()
    {
        Console.WriteLine("How many students are in the class?");
        string input = Console.ReadLine();
        int studentNumber = CheckIfInteger(input);
        Students = new Student[studentNumber];
    }

    static void AddStudentInformation()
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

    static void ListStudents()
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

    static void Main(string[] args)
    {
        InitArray();
        AddStudentInformation();
        ListStudents();
    }
}