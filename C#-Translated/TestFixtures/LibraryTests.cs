using NUnit.Framework;
using System;

namespace LibraryManagementSystemTests
{
    [TestFixture]
    public class LibraryTests
    {
        private TLibrary library;

        [SetUp]
        public void Setup()
        {
            library = new TLibrary(100);
        }

        // Smoke Test
        [Test]
        public void SmokeTest_LibraryCreation_Success()
        {
            Assert.IsNotNull(library);
        }

        // Unit Tests
        [Test]
        public void AddBook_ValidBook_Success()
        {
            TBook book = new TBook
            {
                Title = "Book1",
                Author = "Author1",
                ISBN = "ISBN1",
                Available = true
            };

            library.AddBook(book);

            Assert.AreEqual(1, library.GetBookCount());
        }

        [Test]
        public void RemoveBook_ExistingBook_Success()
        {
            TBook book = new TBook
            {
                Title = "Book1",
                Author = "Author1",
                ISBN = "ISBN1",
                Available = true
            };

            library.AddBook(book);
            library.RemoveBook("ISBN1");

            Assert.AreEqual(0, library.GetBookCount());
        }

        [Test]
        public void FindBook_ExistingBook_ReturnsIndex()
        {
            TBook book = new TBook
            {
                Title = "Book1",
                Author = "Author1",
                ISBN = "ISBN1",
                Available = true
            };

            library.AddBook(book);

            int index = library.FindBook("ISBN1");

            Assert.AreEqual(0, index);
        }

        [Test]
        public void BorrowBook_AvailableBook_Success()
        {
            TBook book = new TBook
            {
                Title = "Book1",
                Author = "Author1",
                ISBN = "ISBN1",
                Available = true
            };

            library.AddBook(book);
            library.BorrowBook("ISBN1");

            int index = library.FindBook("ISBN1");
            Assert.IsFalse(library.FBooks[index].Available);
        }

        [Test]
        public void ReturnBook_BorrowedBook_Success()
        {
            TBook book = new TBook
            {
                Title = "Book1",
                Author = "Author1",
                ISBN = "ISBN1",
                Available = false
            };

            library.AddBook(book);
            library.ReturnBook("ISBN1");

            int index = library.FindBook("ISBN1");
            Assert.IsTrue(library.FBooks[index].Available);
        }

        // Functional Tests
        [Test]
        public void AddBook_LibraryFull_Failure()
        {
            TLibrary smallLibrary = new TLibrary(1);

            TBook book1 = new TBook
            {
                Title = "Book1",
                Author = "Author1",
                ISBN = "ISBN1",
                Available = true
            };

            TBook book2 = new TBook
            {
                Title = "Book2",
                Author = "Author2",
                ISBN = "ISBN2",
                Available = true
            };

            smallLibrary.AddBook(book1);
            smallLibrary.AddBook(book2);

            Assert.AreEqual(1, smallLibrary.GetBookCount());
        }

        [Test]
        public void RemoveBook_NonExistingBook_Failure()
        {
            library.RemoveBook("NonExistingISBN");

            Assert.AreEqual(0, library.GetBookCount());
        }

        [Test]
        public void BorrowBook_NonExistingBook_Failure()
        {
            library.BorrowBook("NonExistingISBN");

            Assert.AreEqual(0, library.GetBookCount());
        }

        [Test]
        public void ReturnBook_NonExistingBook_Failure()
        {
            library.ReturnBook("NonExistingISBN");

            Assert.AreEqual(0, library.GetBookCount());
        }

        // Class Name Tests
        [Test]
        public void ClassName_TBook_IsCorrect()
        {
            TBook book = new TBook();
            Assert.AreEqual("TBook", book.GetType().Name);
        }

        [Test]
        public void ClassName_TLibrary_IsCorrect()
        {
            Assert.AreEqual("TLibrary", library.GetType().Name);
        }
    }
}