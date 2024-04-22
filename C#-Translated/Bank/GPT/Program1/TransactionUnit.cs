namespace TransactionUnit6
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
            return FAccountNumber;
        }

        public double GetAmount()
        {
            return FAmount;
        }

        public string GetTransactionType()
        {
            return FTransactionType;
        }
    }
}