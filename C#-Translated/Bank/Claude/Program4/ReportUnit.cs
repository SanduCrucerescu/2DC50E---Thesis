using System;
namespace BankAccountSystem4
{
    public static class ReportUnit
    {
        public static void GenerateReport(AccountList accountList)
        {
            double totalBalance = 0;
            Console.WriteLine("Account Report:");
            for (int i = 0; i < accountList.GetCount(); i++)
            {
                Account account = accountList.GetAccount(i);
                Console.WriteLine("Account Number: " + account.GetAccountNumber());
                Console.WriteLine("Account Holder: " + account.GetAccountHolder());
                Console.WriteLine("Balance: " + account.GetBalance().ToString("C2"));
                Console.WriteLine();
                totalBalance += account.GetBalance();
            }

            Console.WriteLine("Total Balance: " + totalBalance.ToString("C2"));
        }
    }
}