namespace AccountUnit
{
    using SysUtils;

    public static class Scope
    {
        public class TAccount
        {
            private string FAccountHolder;
            private string FAccountNumber;
            private double FBalance;
            public TAccount(string AccountHolder, string AccountNumber, double Balance)
            {
                FAccountHolder = AccountHolder;
                FAccountNumber = AccountNumber;
                FBalance = Balance;
            }

            public string GetAccountHolder()
            {
                string Result;
                Result = FAccountHolder;
                return Result;
            }

            public string GetAccountNumber()
            {
                string Result;
                Result = FAccountNumber;
                return Result;
            }

            public double GetBalance()
            {
                double Result;
                Result = FBalance;
                return Result;
            }

            public void Deposit(double Amount)
            {
                FBalance = FBalance + Amount;
            }

            public void Withdraw(double Amount)
            {
                if (Amount <= FBalance)
                    FBalance = FBalance - Amount;
                else
                    throw Exception.Create("Insufficient funds.");
            }
        }

        public class TAccountList
        {
            private TAccount[] FAccounts;
            private int FCount;
            public TAccountList()
            {
                FCount = 0;
            }

            public void AddAccount(TAccount Account)
            {
                SetLength(FAccounts, FCount + 1);
                FAccounts[FCount] = Account;
                Inc(FCount);
            }

            public TAccount GetAccount(int Index)
            {
                TAccount Result;
                Result = FAccounts[Index];
                return Result;
            }

            public int GetCount()
            {
                int Result;
                Result = FCount;
                return Result;
            }

            public void ProcessTransaction(TTransaction Transaction)
            {
                for (int i = 0; i < FCount - 1; i++)
                {
                    Account = FAccounts[I];
                    if (Account.GetAccountNumber == Transaction.GetAccountNumber)
                    {
                        if (Transaction.GetTransactionType == "Deposit")
                            Account.Deposit(Transaction.GetAmount);
                        else if (Transaction.GetTransactionType == "Withdrawal")
                            Account.Withdraw(Transaction.GetAmount);
                        Break;
                    }
                }
            }
        }
    }
}