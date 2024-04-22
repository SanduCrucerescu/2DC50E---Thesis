using System;
namespace BankAccountSystem4
{
    class Program
    {
        static void Main(string[] args)
        {
            AccountList accountList = new AccountList();
            int choice;
            string accountNumber;
            string accountHolder;
            double initialBalance;
            double amount;

            void ShowMenu()
            {
                Console.WriteLine("Bank Account Management System");
                Console.WriteLine("1. Create Account");
                Console.WriteLine("2. Deposit");
                Console.WriteLine("3. Withdraw");
                Console.WriteLine("4. Generate Report");
                Console.WriteLine("5. Exit");
                Console.Write("Enter your choice: ");
            }

            do
            {
                ShowMenu();
                choice = Convert.ToInt32(Console.ReadLine());

                switch (choice)
                {
                    case 1: // Create Account
                        Console.Write("Enter account holder name: ");
                        accountHolder = Console.ReadLine();
                        Console.Write("Enter account number: ");
                        accountNumber = Console.ReadLine();
                        Console.Write("Enter initial balance: ");
                        initialBalance = Convert.ToDouble(Console.ReadLine());
                        accountList.AddAccount(new Account(accountHolder, accountNumber, initialBalance));
                        Console.WriteLine("Account created successfully.");
                        break;

                    case 2: // Deposit
                        Console.Write("Enter account number: ");
                        accountNumber = Console.ReadLine();
                        Console.Write("Enter amount to deposit: ");
                        amount = Convert.ToDouble(Console.ReadLine());
                        accountList.ProcessTransaction(new Transaction(accountNumber, amount, "Deposit"));
                        Console.WriteLine("Deposit successful.");
                        break;

                    case 3: // Withdraw
                        Console.Write("Enter account number: ");
                        accountNumber = Console.ReadLine();
                        Console.Write("Enter amount to withdraw: ");
                        amount = Convert.ToDouble(Console.ReadLine());
                        try
                        {
                            accountList.ProcessTransaction(new Transaction(accountNumber, amount, "Withdrawal"));
                            Console.WriteLine("Withdrawal successful.");
                        }
                        catch (Exception e)
                        {
                            Console.WriteLine("Error: " + e.Message);
                        }
                        break;

                    case 4: // Generate Report
                        ReportUnit.GenerateReport(accountList);
                        break;

                    case 5: // Exit
                        Console.WriteLine("Exiting the program.");
                        break;

                    default:
                        Console.WriteLine("Invalid choice. Please try again.");
                        break;
                }

                Console.WriteLine();
            } while (choice != 5);
        }
    }
}