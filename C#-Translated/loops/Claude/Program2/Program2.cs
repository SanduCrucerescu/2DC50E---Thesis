namespace loops2; // was not created I did it manually

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
        Students = new Student[StudentNumber];
    }

    internal static void AddStudentInformation()
    {
        for (int i = 0; i < Students.Length; i++)
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
            Students[i] = StudentTemp;
        }
    }

    internal static void ListStudents()
    {
        Console.WriteLine("First Name  |  Last Name  |  Average Grade");
        double AverageGrade = 0;
        foreach (Student student in Students)
        {
            Console.Write($"{student.FirstName,-12} | {student.LastName,-12} | {student.AverageGrade,3}");
            Console.WriteLine();
            AverageGrade += student.AverageGrade;
        }
        AverageGrade /= Students.Length;
        Console.WriteLine($"Average class grade: {AverageGrade}");
    }

    static void Main()
    {
        InitArray();
        AddStudentInformation();
        ListStudents();
    }
}