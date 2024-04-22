namespace ReportUnit10
{
    using AccountUnit10;
    using System;

    public static class ReportGenerator
    {
        public static void GenerateReport(TAccountList AccountList)
        {
            double TotalBalance = 0;
            Console.WriteLine("Account Report:");
            for (int I = 0; I < AccountList.GetCount(); I++)
            {
                var Account = AccountList.GetAccount(I);
                Console.WriteLine($"Account Number: {Account.GetAccountNumber()}");
                Console.WriteLine($"Account Holder: {Account.GetAccountHolder()}");
                Console.WriteLine($"Balance: {Account.GetBalance():C}");
                Console.WriteLine();
                TotalBalance += Account.GetBalance();
            }

            Console.WriteLine($"Total Balance: {TotalBalance:C}");
        }
    }
}