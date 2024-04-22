using System;

namespace BankAccountSystem1
{
    public class ReportUnit
    {
        public static void GenerateReport(TAccountList AccountList)
        {
            double TotalBalance = 0;
            Console.WriteLine("Account Report:");
            for (int I = 0; I < AccountList.GetCount(); I++)
            {
                TAccount Account = AccountList.GetAccount(I);
                Console.WriteLine("Account Number: " + Account.GetAccountNumber());
                Console.WriteLine("Account Holder: " + Account.GetAccountHolder());
                Console.WriteLine("Balance: " + Account.GetBalance().ToString("C2"));
                Console.WriteLine();
                TotalBalance += Account.GetBalance();
            }

            Console.WriteLine("Total Balance: " + TotalBalance.ToString("C2"));
        }
    }
}