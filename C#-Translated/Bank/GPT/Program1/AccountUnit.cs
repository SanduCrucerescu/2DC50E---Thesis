using System;
using System.Collections.Generic;

namespace AccountUnit6
{
    public class TAccount
    {
        private string FAccountHolder;
        private string FAccountNumber;
        private double FBalance;

        public TAccount(string AccountHolder, string AccountNumber, double Balance)
        {
            this.FAccountHolder = AccountHolder;
            this.FAccountNumber = AccountNumber;
            this.FBalance = Balance;
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

        public void Deposit(double Amount)
        {
            FBalance += Amount;
        }

        public void Withdraw(double Amount)
        {
            if (Amount <= FBalance)
                FBalance -= Amount;
            else
                throw new InvalidOperationException("Insufficient funds.");
        }
    }

    public class TAccountList
    {
        private List<TAccount> FAccounts = new List<TAccount>();
        private int FCount;

        public TAccountList()
        {
            FCount = 0;
        }

        public void AddAccount(TAccount Account)
        {
            FAccounts.Add(Account);
            FCount++;
        }

        public TAccount GetAccount(int Index)
        {
            return FAccounts[Index];
        }

        public int GetCount()
        {
            return FCount;
        }

        public void ProcessTransaction(TransactionUnit6.TTransaction Transaction)
        {
            foreach (var Account in FAccounts)
            {
                if (Account.GetAccountNumber() == Transaction.GetAccountNumber())
                {
                    if (Transaction.GetTransactionType() == "Deposit")
                        Account.Deposit(Transaction.GetAmount());
                    else if (Transaction.GetTransactionType() == "Withdrawal")
                        Account.Withdraw(Transaction.GetAmount());

                    break;
                }
            }
        }
    }
}