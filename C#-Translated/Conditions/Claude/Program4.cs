namespace Conditions4; // was not created I did it manually

using System;
class Program
{
    static void StudentAge(int Num)
    {
        if (Num < 18)
        {
            Console.WriteLine("The person is underage!");
        }
        else
        {
            Console.WriteLine("The person is eligible for this position.");
        }
    }

    static void Main(string[] args)
    {
        Console.WriteLine("What is your age?");
        string Input = Console.ReadLine();

        int Age;
        bool IsInteger = int.TryParse(Input, out Age);

        if (IsInteger)
        {
            StudentAge(Age);
        }
        else
        {
            Console.WriteLine("Please insert an integer.");
            Console.WriteLine("Press any key to exit.");
            Console.ReadKey();
        }
    }
}