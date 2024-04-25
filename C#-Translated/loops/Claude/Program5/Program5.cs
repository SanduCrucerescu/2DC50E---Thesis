namespace loops5; // was not created I did it manually

using System;

class Program
{
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
        while (true)
        {
            if (!int.TryParse(input, out response))
            {
                Console.WriteLine("Please write an integer, or press ENTER to exit.");
                input = Console.ReadLine();
                if (string.IsNullOrEmpty(input))
                {
                    Environment.Exit(0);
                }
            }
            else
            {
                break;
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
            Student studentTemp = new Student();

            Console.WriteLine($"Student {i + 1} first name:");
            string firstName = Console.ReadLine();
            studentTemp.FirstName = firstName;

            Console.WriteLine($"Student {i + 1} last name:");
            string lastName = Console.ReadLine();
            studentTemp.LastName = lastName;

            Console.WriteLine($"Student {i + 1} average grade:");
            string averageGrade = Console.ReadLine();
            studentTemp.AverageGrade = CheckIfInteger(averageGrade);

            Students[i] = studentTemp;
        }
    }

    internal static void ListStudents()
    {
        Console.WriteLine("First Name  |  Last Name  |  Average Grade");
        double averageGrade = 0;
        foreach (Student student in Students)
        {
            Console.Write($"{student.FirstName,-12} | ");
            Console.Write($"{student.LastName,-12} | ");
            Console.WriteLine($"{student.AverageGrade,3}");
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