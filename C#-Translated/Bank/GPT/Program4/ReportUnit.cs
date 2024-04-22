namespace ReportUnit9
{
    using AccountUnit9;
    using System;

    public static class ReportGenerator
    {
        public static void GenerateReport(TAccountList AccountList)
        {
            double TotalBalance = 0;
            Console.WriteLine("Account Report:");
            for (int i = 0; i < AccountList.GetCount(); i++)
            {
                TAccount Account = AccountList.GetAccount(i);
                Console.WriteLine($"Account Number: {Account.GetAccountNumber()}");
                Console.WriteLine($"Account Holder: {Account.GetAccountHolder()}");
                Console.WriteLine($"Balance: ${Account.GetBalance():N2}");
                Console.WriteLine();
                TotalBalance += Account.GetBalance();
            }

            Console.WriteLine($"Total Balance: ${TotalBalance:N2}");
        }
    }
}