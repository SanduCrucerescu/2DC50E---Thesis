using System;
using System.Collections.Generic;

namespace AccountUnit8
{
    using TransactionUnit8;

    public class TAccount
    {
        private string FAccountHolder;
        private string FAccountNumber;
        private double FBalance;

        public TAccount(string accountHolder, string accountNumber, double balance)
        {
            FAccountHolder = accountHolder;
            FAccountNumber = accountNumber;
            FBalance = balance;
        }

        public string GetAccountHolder()
        {
            return FAccountHolder;
        }

        public string GetAccountNumber()
        {
            return FAccountNumber;
        }

        public double GetBalance()
        {
            return FBalance;
        }

        public void Deposit(double amount)
        {
            FBalance += amount;
        }

        public void Withdraw(double amount)
        {
            if (amount <= FBalance)
            {
                FBalance -= amount;
            }
            else
            {
                throw new InvalidOperationException("Insufficient funds.");
            }
        }
    }

    public class TAccountList
    {
        private List<TAccount> FAccounts = new List<TAccount>();

        public void AddAccount(TAccount account)
        {
            FAccounts.Add(account);
        }

        public TAccount GetAccount(int index)
        {
            return FAccounts[index];
        }

        public int GetCount()
        {
            return FAccounts.Count;
        }

        public void ProcessTransaction(TTransaction transaction)
        {
            foreach (var account in FAccounts)
            {
                if (account.GetAccountNumber() == transaction.GetAccountNumber())
                {
                    if (transaction.GetTransactionType() == "Deposit")
                    {
                        account.Deposit(transaction.GetAmount());
                    }
                    else if (transaction.GetTransactionType() == "Withdrawal")
                    {
                        account.Withdraw(transaction.GetAmount());
                    }
                    break;
                }
            }
        }
    }
}