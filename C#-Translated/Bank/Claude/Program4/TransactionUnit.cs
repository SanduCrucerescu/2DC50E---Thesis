using System;
namespace BankAccountSystem4
{
    public class Transaction
    {
        private string accountNumber;
        private double amount;
        private string transactionType;

        public Transaction(string accountNumber, double amount, string transactionType)
        {
            this.accountNumber = accountNumber;
            this.amount = amount;
            this.transactionType = transactionType;
        }

        public string GetAccountNumber()
        {
            return accountNumber;
        }

        public double GetAmount()
        {
            return amount;
        }

        public string GetTransactionType()
        {
            return transactionType;
        }
    }
}