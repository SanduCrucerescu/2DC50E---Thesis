namespace loops;
public class Program
{

    /**
        The code had a List but since we need a fixed array the variable needed to be changed to array
        Time: <1min;
    */

    public class Student
    {
        public string FirstName { get; set; }
        public string LastName { get; set; }
        public int AverageGrade { get; set; }
    }

    static int CheckIfInteger(string Input)
    {
        int Response;
        while (true)
        {
            if (int.TryParse(Input, out Response))
            {
                if (Response > 0)
                    return Response;
            }
            Console.WriteLine("Please write an integer, or press ENTER to exit.");
            Input = Console.ReadLine();
            if (string.IsNullOrEmpty(Input))
                Environment.Exit(0);
        }
    }

    public static void InitArray(out Student[] Students)
    {
        Console.WriteLine("How many students are in the class?");
        string Input = Console.ReadLine();
        int StudentNumber = CheckIfInteger(Input);
        Students = new Student[StudentNumber];
    }

    public static void AddStudentInformation(Student[] Students)
    {
        for (int i = 0; i < Students.Length; i++)
        {
            Console.WriteLine($"Student {i + 1} first name:");
            string FirstName = Console.ReadLine();
            Console.WriteLine($"Student {i + 1} last name:");
            string LastName = Console.ReadLine();
            Console.WriteLine($"Student {i + 1} average grade:");
            string AverageGrade = Console.ReadLine();
            Students[i] = new Student
            {
                FirstName = FirstName,
                LastName = LastName,
                AverageGrade = CheckIfInteger(AverageGrade)
            };
        }
    }

    static void ListStudents(Student[] Students)
    {
        Console.WriteLine("First Name  |  Last Name  |  Average Grade");
        double AverageGrade = 0;
        foreach (var student in Students)
        {
            Console.WriteLine($"{student.FirstName.PadRight(12)} | {student.LastName.PadRight(12)} | {student.AverageGrade.ToString("D3")}");
            AverageGrade += student.AverageGrade;
        }
        AverageGrade /= Students.Length;
        Console.WriteLine($"Average class grade: {AverageGrade}");
    }

    static void Main(string[] args)
    {
        Student[] Students;
        InitArray(out Students);
        AddStudentInformation(Students);
        ListStudents(Students);
    }
}