namespace BankSystem9
{
    using System;
    using AccountUnit9;
    using TransactionUnit9;
    using ReportUnit9;

    class Program
    {
        static void Main(string[] args)
        {
            TAccountList AccountList = new TAccountList();
            int Choice;

            do
            {
                ShowMenu();
                Choice = int.Parse(Console.ReadLine());

                switch (Choice)
                {
                    case 1: // Create Account
                        Console.Write("Enter account holder name: ");
                        string AccountHolder = Console.ReadLine();
                        Console.Write("Enter account number: ");
                        string AccountNumber = Console.ReadLine();
                        Console.Write("Enter initial balance: ");
                        double InitialBalance = double.Parse(Console.ReadLine());
                        AccountList.AddAccount(new TAccount(AccountHolder, AccountNumber, InitialBalance));
                        Console.WriteLine("Account created successfully.");
                        break;

                    case 2: // Deposit
                        Console.Write("Enter account number: ");
                        AccountNumber = Console.ReadLine();
                        Console.Write("Enter amount to deposit: ");
                        double Amount = double.Parse(Console.ReadLine());
                        AccountList.ProcessTransaction(new TTransaction(AccountNumber, Amount, "Deposit"));
                        Console.WriteLine("Deposit successful.");
                        break;

                    case 3: // Withdraw
                        Console.Write("Enter account number: ");
                        AccountNumber = Console.ReadLine();
                        Console.Write("Enter amount to withdraw: ");
                        Amount = double.Parse(Console.ReadLine());
                        try
                        {
                            AccountList.ProcessTransaction(new TTransaction(AccountNumber, Amount, "Withdrawal"));
                            Console.WriteLine("Withdrawal successful.");
                        }
                        catch (Exception e)
                        {
                            Console.WriteLine($"Error: {e.Message}");
                        }
                        break;

                    case 4: // Generate Report
                        ReportGenerator.GenerateReport(AccountList);
                        break;

                    case 5: // Exit
                        Console.WriteLine("Exiting the program.");
                        break;

                    default:
                        Console.WriteLine("Invalid choice. Please try again.");
                        break;
                }

                Console.WriteLine();
            } while (Choice != 5);
        }

        static void ShowMenu()
        {
            Console.WriteLine("Bank Account Management System");
            Console.WriteLine("1. Create Account");
            Console.WriteLine("2. Deposit");
            Console.WriteLine("3. Withdraw");
            Console.WriteLine("4. Generate Report");
            Console.WriteLine("5. Exit");
            Console.Write("Enter your choice: ");
        }
    }
}