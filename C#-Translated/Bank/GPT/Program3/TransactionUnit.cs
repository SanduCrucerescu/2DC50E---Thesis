using System;
using System.Collections.Generic;

namespace TransactionUnit8
{
    public class TTransaction
    {
        private string FAccountNumber;
        private double FAmount;
        private string FTransactionType;

        public TTransaction(string accountNumber, double amount, string transactionType)
        {
            FAccountNumber = accountNumber;
            FAmount = amount;
            FTransactionType = transactionType;
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