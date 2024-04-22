using NUnit.Framework;
using System;

namespace BankAccountSystem.Tests
{
    [TestFixture]
    public class BankAccountSystemTests
    {
        private TAccountList accountList;

        [SetUp]
        public void Setup()
        {
            accountList = new TAccountList();
        }

        // Smoke Tests
        [Test]
        public void SmokeTest_CreateAccount_AccountCreated()
        {
            // Arrange
            string accountHolder = "John Doe";
            string accountNumber = "1234567890";
            double initialBalance = 1000;

            // Act
            accountList.AddAccount(new TAccount(accountHolder, accountNumber, initialBalance));

            // Assert
            Assert.AreEqual(1, accountList.GetCount());
        }

        [Test]
        public void SmokeTest_Deposit_BalanceUpdated()
        {
            // Arrange
            TAccount account = new TAccount("John Doe", "1234567890", 1000);
            accountList.AddAccount(account);
            double depositAmount = 500;

            // Act
            accountList.ProcessTransaction(new TTransaction(account.GetAccountNumber(), depositAmount, "Deposit"));

            // Assert
            Assert.AreEqual(1500, account.GetBalance());
        }

        [Test]
        public void SmokeTest_Withdraw_BalanceUpdated()
        {
            // Arrange
            TAccount account = new TAccount("John Doe", "1234567890", 1000);
            accountList.AddAccount(account);
            double withdrawalAmount = 500;

            // Act
            accountList.ProcessTransaction(new TTransaction(account.GetAccountNumber(), withdrawalAmount, "Withdrawal"));

            // Assert
            Assert.AreEqual(500, account.GetBalance());
        }

        // Unit Tests
        [Test]
        public void UnitTest_GetAccountHolder_ReturnsAccountHolder()
        {
            // Arrange
            string accountHolder = "John Doe";
            TAccount account = new TAccount(accountHolder, "1234567890", 1000);

            // Act
            string result = account.GetAccountHolder();

            // Assert
            Assert.AreEqual(accountHolder, result);
        }

        [Test]
        public void UnitTest_GetAccountNumber_ReturnsAccountNumber()
        {
            // Arrange
            string accountNumber = "1234567890";
            TAccount account = new TAccount("John Doe", accountNumber, 1000);

            // Act
            string result = account.GetAccountNumber();

            // Assert
            Assert.AreEqual(accountNumber, result);
        }

        [Test]
        public void UnitTest_GetBalance_ReturnsBalance()
        {
            // Arrange
            double balance = 1000;
            TAccount account = new TAccount("John Doe", "1234567890", balance);

            // Act
            double result = account.GetBalance();

            // Assert
            Assert.AreEqual(balance, result);
        }

        [Test]
        public void UnitTest_Deposit_UpdatesBalance()
        {
            // Arrange
            TAccount account = new TAccount("John Doe", "1234567890", 1000);
            double depositAmount = 500;

            // Act
            account.Deposit(depositAmount);

            // Assert
            Assert.AreEqual(1500, account.GetBalance());
        }

        [Test]
        public void UnitTest_Withdraw_UpdatesBalance()
        {
            // Arrange
            TAccount account = new TAccount("John Doe", "1234567890", 1000);
            double withdrawalAmount = 500;

            // Act
            account.Withdraw(withdrawalAmount);

            // Assert
            Assert.AreEqual(500, account.GetBalance());
        }

        [Test]
        public void UnitTest_Withdraw_InsufficientFunds_ThrowsException()
        {
            // Arrange
            TAccount account = new TAccount("John Doe", "1234567890", 1000);
            double withdrawalAmount = 1500;

            // Act & Assert
            Assert.Throws<Exception>(() => account.Withdraw(withdrawalAmount));
        }

        // Class Name Tests
        [Test]
        public void ClassNameTest_TAccount_Exists()
        {
            // Act
            var account = new TAccount("John Doe", "1234567890", 1000);

            // Assert
            Assert.IsInstanceOf<TAccount>(account);
        }

        [Test]
        public void ClassNameTest_TAccountList_Exists()
        {
            // Act
            var accountList = new TAccountList();

            // Assert
            Assert.IsInstanceOf<TAccountList>(accountList);
        }

        [Test]
        public void ClassNameTest_TTransaction_Exists()
        {
            // Act
            var transaction = new TTransaction("1234567890", 1000, "Deposit");

            // Assert
            Assert.IsInstanceOf<TTransaction>(transaction);
        }

        // Integration Tests
        [Test]
        public void IntegrationTest_AddAccount_AccountAdded()
        {
            // Arrange
            string accountHolder = "John Doe";
            string accountNumber = "1234567890";
            double initialBalance = 1000;

            // Act
            accountList.AddAccount(new TAccount(accountHolder, accountNumber, initialBalance));

            // Assert
            Assert.AreEqual(1, accountList.GetCount());
            TAccount addedAccount = accountList.GetAccount(0);
            Assert.AreEqual(accountHolder, addedAccount.GetAccountHolder());
            Assert.AreEqual(accountNumber, addedAccount.GetAccountNumber());
            Assert.AreEqual(initialBalance, addedAccount.GetBalance());
        }

        [Test]
        public void IntegrationTest_ProcessTransaction_Deposit_BalanceUpdated()
        {
            // Arrange
            TAccount account = new TAccount("John Doe", "1234567890", 1000);
            accountList.AddAccount(account);
            double depositAmount = 500;

            // Act
            accountList.ProcessTransaction(new TTransaction(account.GetAccountNumber(), depositAmount, "Deposit"));

            // Assert
            TAccount updatedAccount = accountList.GetAccount(0);
            Assert.AreEqual(1500, updatedAccount.GetBalance());
        }

        [Test]
        public void IntegrationTest_ProcessTransaction_Withdraw_BalanceUpdated()
        {
            // Arrange
            TAccount account = new TAccount("John Doe", "1234567890", 1000);
            accountList.AddAccount(account);
            double withdrawalAmount = 500;

            // Act
            accountList.ProcessTransaction(new TTransaction(account.GetAccountNumber(), withdrawalAmount, "Withdrawal"));

            // Assert
            TAccount updatedAccount = accountList.GetAccount(0);
            Assert.AreEqual(500, updatedAccount.GetBalance());
        }

    }
}