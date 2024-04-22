namespace loops5; // was not created I did it manually

using System;

class loops
{
    struct Student
    {
        public string FirstName;
        public string LastName;
        public int AverageGrade;
    }

    static Student[] Students;

    static int CheckIfInteger(string Input)
    {
        int Response = 0;
        do
        {
            if (!int.TryParse(Input, out Response))
            {
                Console.WriteLine("Please write an integer, or press ENTER to exit.");
                Input = Console.ReadLine();
                if (Input == "")
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
        Students = new Student[StudentNumber];
    }

    static void AddStudentInformation()
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

    static void ListStudents()
    {
        Console.WriteLine("First Name  |  Last Name  |  Average Grade");
        double AverageGrade = 0;
        foreach (var student in Students)
        {
            Console.Write($"{student.FirstName,12} | ");
            Console.Write($"{student.LastName,12} | ");
            Console.WriteLine($"{student.AverageGrade,3}");
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