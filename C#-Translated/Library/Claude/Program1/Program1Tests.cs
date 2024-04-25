using NUnit.Framework;
using NUnit.Framework.Legacy;
using System;

namespace Library1.Test
{
    [TestFixture]
    public class LibraryTests
    {
        private LibraryManagementSystem.TLibrary library;

        [SetUp]
        public void Setup()
        {
            library = new LibraryManagementSystem.TLibrary(100);
        }

        // Smoke Test
        [Test]
        public void SmokeTest_LibraryCreation_Success()
        {
            ClassicAssert.IsNotNull(library);
        }

        // Unit Tests
        [Test]
        public void AddBook_ValidBook_Success()
        {
            LibraryManagementSystem.TBook book = new LibraryManagementSystem.TBook
            {
                Title = "Book1",
                Author = "Author1",
                ISBN = "ISBN1",
                Available = true
            };

            library.AddBook(book);

            ClassicAssert.AreEqual(1, library.GetBookCount());
        }

        [Test]
        public void RemoveBook_ExistingBook_Success()
        {
            LibraryManagementSystem.TBook book = new LibraryManagementSystem.TBook
            {
                Title = "Book1",
                Author = "Author1",
                ISBN = "ISBN1",
                Available = true
            };

            library.AddBook(book);
            library.RemoveBook("ISBN1");

            ClassicAssert.AreEqual(0, library.GetBookCount());
        }

        [Test]
        public void FindBook_ExistingBook_ReturnsIndex()
        {
            LibraryManagementSystem.TBook book = new LibraryManagementSystem.TBook
            {
                Title = "Book1",
                Author = "Author1",
                ISBN = "ISBN1",
                Available = true
            };

            library.AddBook(book);

            int index = library.FindBook("ISBN1");

            ClassicAssert.AreEqual(0, index);
        }

        [Test]
        public void BorrowBook_AvailableBook_Success()
        {
            LibraryManagementSystem.TBook book = new LibraryManagementSystem.TBook
            {
                Title = "Book1",
                Author = "Author1",
                ISBN = "ISBN1",
                Available = true
            };

            library.AddBook(book);
            library.BorrowBook("ISBN1");

            int index = library.FindBook("ISBN1");
            ClassicAssert.IsFalse(library.FBooks[index].Available);
        }

        [Test]
        public void ReturnBook_BorrowedBook_Success()
        {
            LibraryManagementSystem.TBook book = new LibraryManagementSystem.TBook
            {
                Title = "Book1",
                Author = "Author1",
                ISBN = "ISBN1",
                Available = false
            };

            library.AddBook(book);
            library.ReturnBook("ISBN1");

            int index = library.FindBook("ISBN1");
            ClassicAssert.IsTrue(library.FBooks[index].Available);
        }

        // Functional Tests
        [Test]
        public void AddBook_LibraryFull_Failure()
        {
            LibraryManagementSystem.TLibrary smallLibrary = new LibraryManagementSystem.TLibrary(1);

            LibraryManagementSystem.TBook book1 = new LibraryManagementSystem.TBook
            {
                Title = "Book1",
                Author = "Author1",
                ISBN = "ISBN1",
                Available = true
            };

            LibraryManagementSystem.TBook book2 = new LibraryManagementSystem.TBook
            {
                Title = "Book2",
                Author = "Author2",
                ISBN = "ISBN2",
                Available = true
            };

            smallLibrary.AddBook(book1);
            smallLibrary.AddBook(book2);

            ClassicAssert.AreEqual(1, smallLibrary.GetBookCount());
        }

        [Test]
        public void RemoveBook_NonExistingBook_Failure()
        {
            library.RemoveBook("NonExistingISBN");

            ClassicAssert.AreEqual(0, library.GetBookCount());
        }

        [Test]
        public void BorrowBook_NonExistingBook_Failure()
        {
            library.BorrowBook("NonExistingISBN");

            ClassicAssert.AreEqual(0, library.GetBookCount());
        }

        [Test]
        public void ReturnBook_NonExistingBook_Failure()
        {
            library.ReturnBook("NonExistingISBN");

            ClassicAssert.AreEqual(0, library.GetBookCount());
        }

        // Class Name Tests
        [Test]
        public void ClassName_TBook_IsCorrect()
        {
            LibraryManagementSystem.TBook book = new LibraryManagementSystem.TBook();
            ClassicAssert.AreEqual("TBook", book.GetType().Name);
        }

        [Test]
        public void ClassName_TLibrary_IsCorrect()
        {
            ClassicAssert.AreEqual("TLibrary", library.GetType().Name);
        }
    }
}