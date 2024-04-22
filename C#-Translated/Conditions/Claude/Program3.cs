namespace conditions3
{
    class Program
    {
        static void Main()
        {
            string Input;
            int Age;
            int IsInteger;

            Console.WriteLine("What is your age?");
            Input = Console.ReadLine();

            IsInteger = int.TryParse(Input, out Age) ? 0 : 1;

            if (IsInteger == 0)
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
    }
}