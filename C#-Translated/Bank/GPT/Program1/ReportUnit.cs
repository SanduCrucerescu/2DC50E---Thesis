namespace ReportUnit6
{
    using AccountUnit6;
    using System;

    public static class ReportGenerator
    {
        public static void GenerateReport(TAccountList AccountList)
        {
            double TotalBalance = 0;
            Console.WriteLine("Account Report:");
            for (int i = 0; i < AccountList.GetCount(); i++)
            {
                var Account = AccountList.GetAccount(i);
                Console.WriteLine($"Account Number: {Account.GetAccountNumber()}");
                Console.WriteLine($"Account Holder: {Account.GetAccountHolder()}");
                Console.WriteLine($"Balance: {Account.GetBalance():C2}");
                Console.WriteLine();
                TotalBalance += Account.GetBalance();
            }

            Console.WriteLine($"Total Balance: {TotalBalance:C2}");
        }
    }
}