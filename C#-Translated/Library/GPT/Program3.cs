namespace Library8; // was not created I did it manually

using System;
using System.Collections.Generic;

class Program
{
    static void Main(string[] args)
    {
        Library libraryObj = new Library(100);
        int choice;
        string isbn;

        do
        {
            Console.WriteLine("Library Management System");
            Console.WriteLine("1. Add Book");
            Console.WriteLine("2. Remove Book");
            Console.WriteLine("3. Borrow Book");
            Console.WriteLine("4. Return Book");
            Console.WriteLine("5. Display Books");
            Console.WriteLine("6. Exit");
            Console.Write("Enter your choice: ");
            choice = int.Parse(Console.ReadLine());

            switch (choice)
            {
                case 1:
                    Book book = new Book();
                    Console.Write("Enter book title: ");
                    book.Title = Console.ReadLine();
                    Console.Write("Enter book author: ");
                    book.Author = Console.ReadLine();
                    Console.Write("Enter book ISBN: ");
                    book.ISBN = Console.ReadLine();
                    book.Available = true;
                    libraryObj.AddBook(book);
                    break;
                case 2:
                    Console.Write("Enter book ISBN: ");
                    isbn = Console.ReadLine();
                    libraryObj.RemoveBook(isbn);
                    break;
                case 3:
                    Console.Write("Enter book ISBN: ");
                    isbn = Console.ReadLine();
                    libraryObj.BorrowBook(isbn);
                    break;
                case 4:
                    Console.Write("Enter book ISBN: ");
                    isbn = Console.ReadLine();
                    libraryObj.ReturnBook(isbn);
                    break;
                case 5:
                    libraryObj.DisplayBooks();
                    break;
                case 6:
                    Console.WriteLine("Exiting the program.");
                    break;
                default:
                    Console.WriteLine("Invalid choice. Please try again.");
                    break;
            }

            Console.WriteLine();
        } while (choice != 6);

        Console.WriteLine("Total books in the library: " + libraryObj.GetBookCount());
        Console.ReadLine();
    }
}

public class Book
{
    public string Title { get; set; }
    public string Author { get; set; }
    public string ISBN { get; set; }
    public bool Available { get; set; }
}

public class Library
{
    private List<Book> books;
    private int capacity;
    private int count;

    public Library(int capacity)
    {
        this.capacity = capacity;
        books = new List<Book>(capacity);
    }

    public void AddBook(Book book)
    {
        if (count < capacity)
        {
            books.Add(book);
            count++;
            Console.WriteLine("Book added successfully.");
        }
        else
        {
            Console.WriteLine("Library is full. Cannot add more books.");
        }
    }

    public void RemoveBook(string isbn)
    {
        var book = books.Find(b => b.ISBN == isbn);
        if (book != null)
        {
            books.Remove(book);
            count--;
            Console.WriteLine("Book removed successfully.");
        }
        else
        {
            Console.WriteLine("Book not found.");
        }
    }

    public void BorrowBook(string isbn)
    {
        var book = books.Find(b => b.ISBN == isbn && b.Available);
        if (book != null)
        {
            book.Available = false;
            Console.WriteLine("Book borrowed successfully.");
        }
        else if (book == null)
        {
            Console.WriteLine("Book not found.");
        }
        else
        {
            Console.WriteLine("Book is not available for borrowing.");
        }
    }

    public void ReturnBook(string isbn)
    {
        var book = books.Find(b => b.ISBN == isbn && !b.Available);
        if (book != null)
        {
            book.Available = true;
            Console.WriteLine("Book returned successfully.");
        }
        else if (book == null)
        {
            Console.WriteLine("Book not found.");
        }
        else
        {
            Console.WriteLine("Book is already available.");
        }
    }

    public void DisplayBooks()
    {
        Console.WriteLine("Library Books:");
        foreach (var book in books)
        {
            Console.WriteLine("Title: " + book.Title);
            Console.WriteLine("Author: " + book.Author);
            Console.WriteLine("ISBN: " + book.ISBN);
            Console.WriteLine("Available: " + (book.Available ? "Yes" : "No"));
            Console.WriteLine();
        }
    }

    public int GetBookCount()
    {
        return count;
    }
}