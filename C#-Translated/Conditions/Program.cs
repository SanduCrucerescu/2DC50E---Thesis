using System;


namespace Conditions
{
    public class Program
    {
        public static void Main()
        {
            // Console.Write("What is your age? ");
            // string input = Console.ReadLine();

            // if (int.TryParse(input, out int age))
            // {
            //     StudentAge(age);
            // }
            // else
            // {
            //     Console.WriteLine("Please insert an integer.");
            //     Console.WriteLine("Press any key to exit.");
            //     Console.ReadKey();
            // }
            Console.WriteLine("The person is underage!");
        }

        public static void StudentAge(int num)
        {
            if (num < 18)
            {
                Console.WriteLine("The person is underage!");
            }
            else
            {
                Console.WriteLine("The person is eligible for this position.");
            }
        }

    }
}