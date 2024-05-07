namespace conditions
{
    using crt;

    public static class Program
    {
        private static string Input;
        private static int Age;
        private static int IsInteger;
        public static void Main(string[] args)
        {
            WriteLn("What is your age?");
            ReadLn(Input);
            Val(Input, Age, IsInteger);
            if ((IsInteger == 0))
            {
                StudentAge(Age);
            }
            else
            {
                WriteLn("Please insert an integer.");
                WriteLn("Press any key to exit.");
                ReadKey;
            }
        }

        private static void StudentAge(int Num)
        {
            if ((Num < 18))
            {
                WriteLn("The person is underage!");
            }
            else
            {
                WriteLn("The person is eligible for this position.");
            }
        }
    }
}