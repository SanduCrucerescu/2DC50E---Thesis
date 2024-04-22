using System;
using AccountUnit6;
using TransactionUnit6;
using ReportUnit6;

namespace BankAccountSystem6
{
    class Program
    {
        static void Main(string[] args)
        {
            TAccountList AccountList = new TAccountList();
            int Choice;
            string AccountNumber, AccountHolder;
            double InitialBalance, Amount;

            Action ShowMenu = () =>
            {
                Console.WriteLine("Bank Account Management System");
                Console.WriteLine("1. Create Account");
                Console.WriteLine("2. Deposit");
                Console.WriteLine("3. Withdraw");
                Console.WriteLine("4. Generate Report");
                Console.WriteLine("5. Exit");
                Console.Write("Enter your choice: ");
            };

            do
            {
                ShowMenu();
                Choice = Convert.ToInt32(Console.ReadLine());

                switch (Choice)
                {
                    case 1:
                        Console.Write("Enter account holder name: ");
                        AccountHolder = Console.ReadLine();
                        Console.Write("Enter account number: ");
                        AccountNumber = Console.ReadLine();
                        Console.Write("Enter initial balance: ");
                        InitialBalance = Convert.ToDouble(Console.ReadLine());
                        AccountList.AddAccount(new TAccount(AccountHolder, AccountNumber, InitialBalance));
                        Console.WriteLine("Account created successfully.");
                        break;

                    case 2:
                        Console.Write("Enter account number: ");
                        AccountNumber = Console.ReadLine();
                        Console.Write("Enter amount to deposit: ");
                        Amount = Convert.ToDouble(Console.ReadLine());
                        AccountList.ProcessTransaction(new TTransaction(AccountNumber, Amount, "Deposit"));
                        Console.WriteLine("Deposit successful.");
                        break;

                    case 3:
                        Console.Write("Enter account number: ");
                        AccountNumber = Console.ReadLine();
                        Console.Write("Enter amount to withdraw: ");
                        Amount = Convert.ToDouble(Console.ReadLine());
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

                    case 4:
                        ReportGenerator.GenerateReport(AccountList);
                        break;

                    case 5:
                        Console.WriteLine("Exiting the program.");
                        break;

                    default:
                        Console.WriteLine("Invalid choice. Please try again.");
                        break;
                }

                Console.WriteLine();
            } while (Choice != 5);
        }
    }
}