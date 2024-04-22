namespace Library9; // was not created I did it manually

using System;
using System.Collections.Generic;

class Program
{
    static void Main(string[] args)
    {
        Library libraryObj = new Library(100);
        int choice;
        string ISBN;

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
                    ISBN = Console.ReadLine();
                    libraryObj.RemoveBook(ISBN);
                    break;
                case 3:
                    Console.Write("Enter book ISBN: ");
                    ISBN = Console.ReadLine();
                    libraryObj.BorrowBook(ISBN);
                    break;
                case 4:
                    Console.Write("Enter book ISBN: ");
                    ISBN = Console.ReadLine();
                    libraryObj.ReturnBook(ISBN);
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
    private List<Book> books = new List<Book>();
    private int capacity;
    private int count;

    public Library(int capacity)
    {
        this.capacity = capacity;
        this.count = 0;
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

    public void RemoveBook(string ISBN)
    {
        var index = FindBook(ISBN);
        if (index != -1)
        {
            books.RemoveAt(index);
            count--;
            Console.WriteLine("Book removed successfully.");
        }
        else
        {
            Console.WriteLine("Book not found.");
        }
    }

    public int FindBook(string ISBN)
    {
        for (int i = 0; i < count; i++)
        {
            if (books[i].ISBN == ISBN)
            {
                return i;
            }
        }
        return -1;
    }

    public void BorrowBook(string ISBN)
    {
        var index = FindBook(ISBN);
        if (index != -1 && books[index].Available)
        {
            books[index].Available = false;
            Console.WriteLine("Book borrowed successfully.");
        }
        else if (index == -1)
        {
            Console.WriteLine("Book not found.");
        }
        else
        {
            Console.WriteLine("Book is not available for borrowing.");
        }
    }

    public void ReturnBook(string ISBN)
    {
        var index = FindBook(ISBN);
        if (index != -1 && !books[index].Available)
        {
            books[index].Available = true;
            Console.WriteLine("Book returned successfully.");
        }
        else if (index == -1)
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
            Console.WriteLine($"Title: {book.Title}");
            Console.WriteLine($"Author: {book.Author}");
            Console.WriteLine($"ISBN: {book.ISBN}");
            Console.WriteLine($"Available: {(book.Available ? "Yes" : "No")}");
            Console.WriteLine();
        }
    }

    public int GetBookCount()
    {
        return count;
    }
}