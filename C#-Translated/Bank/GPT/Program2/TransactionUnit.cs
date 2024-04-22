namespace TransactionUnit7
{
    public class TTransaction
    {
        private string FAccountNumber;
        private double FAmount;
        private string FTransactionType;

        public TTransaction(string AccountNumber, double Amount, string TransactionType)
        {
            this.FAccountNumber = AccountNumber;
            this.FAmount = Amount;
            this.FTransactionType = TransactionType;
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