namespace ReportUnit8
{
    using AccountUnit8;
    using System;

    public static class ReportModule
    {
        public static void GenerateReport(TAccountList accountList)
        {
            double totalBalance = 0;
            Console.WriteLine("Account Report:");
            for (int i = 0; i < accountList.GetCount(); i++)
            {
                TAccount account = accountList.GetAccount(i);
                Console.WriteLine($"Account Number: {account.GetAccountNumber()}");
                Console.WriteLine($"Account Holder: {account.GetAccountHolder()}");
                Console.WriteLine($"Balance: {account.GetBalance():C}");

                totalBalance += account.GetBalance();
                Console.WriteLine();
            }

            Console.WriteLine($"Total Balance: {totalBalance:C}");
        }
    }
}