namespace ReportUnit
{
    using SysUtils;

    public static class Scope
    {
        private static void GenerateReport(TAccountList AccountList)
        {
            TotalBalance = 0;
            WriteLn("Account Report:");
            for (int i = 0; i < AccountList.GetCount - 1; i++)
            {
                Account = AccountList.GetAccount(I);
                WriteLn("Account Number: ", Account.GetAccountNumber);
                WriteLn("Account Holder: ", Account.GetAccountHolder);
                WriteLn("Balance: ", FormatFloat("$#,##0.00", Account.GetBalance));
                WriteLn;
                TotalBalance = TotalBalance + Account.GetBalance;
            }

            WriteLn("Total Balance: ", FormatFloat("$#,##0.00", TotalBalance));
        }
    }
}