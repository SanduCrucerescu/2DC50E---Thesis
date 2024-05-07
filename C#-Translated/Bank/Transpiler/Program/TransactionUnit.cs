namespace TransactionUnit
{
    public static class Scope
    {
        public class TTransaction
        {
            private string FAccountNumber;
            private double FAmount;
            private string FTransactionType;
            public TTransaction(string AccountNumber, double Amount, string TransactionType)
            {
                FAccountNumber = AccountNumber;
                FAmount = Amount;
                FTransactionType = TransactionType;
            }

            public string GetAccountNumber()
            {
                string Result;
                Result = FAccountNumber;
                return Result;
            }

            public double GetAmount()
            {
                double Result;
                Result = FAmount;
                return Result;
            }

            public string GetTransactionType()
            {
                string Result;
                Result = FTransactionType;
                return Result;
            }
        }
    }
}