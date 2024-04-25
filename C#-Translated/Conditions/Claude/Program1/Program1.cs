namespace Conditions1; // was not created I did it manually

using System;
class conditions
{
    public static void StudentAge(int Num)
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

    public static void Main()
    {
        Console.WriteLine("What is your age?");
        string Input = Console.ReadLine();

        if (int.TryParse(Input, out int Age))
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