using System;

namespace BankAccountSystem3
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
                throw new Exception("Insufficient funds.");
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
            Array.Resize(ref FAccounts, FCount + 1);
            FAccounts[FCount] = Account;
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

        public void ProcessTransaction(TTransaction Transaction)
        {
            for (int I = 0; I < FCount; I++)
            {
                TAccount Account = FAccounts[I];
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