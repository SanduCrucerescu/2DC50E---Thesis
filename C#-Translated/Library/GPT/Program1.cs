namespace Library6; // was not created I did it manually

using System;
using System.Collections.Generic;

public class TBook
{
    public string Title { get; set; }
    public string Author { get; set; }
    public string ISBN { get; set; }
    public bool Available { get; set; }
}

public class TLibrary
{
    private List<TBook> FBooks;
    private int FCapacity;
    private int FCount;

    public TLibrary(int Capacity)
    {
        FCapacity = Capacity;
        FBooks = new List<TBook>(FCapacity);
    }

    public void AddBook(TBook Book)
    {
        if (FCount < FCapacity)
        {
            FBooks.Add(Book);
            FCount++;
            Console.WriteLine("Book added successfully.");
        }
        else
        {
            Console.WriteLine("Library is full. Cannot add more books.");
        }
    }

    public void RemoveBook(string ISBN)
    {
        var Index = FindBook(ISBN);
        if (Index != -1)
        {
            FBooks.RemoveAt(Index);
            FCount--;
            Console.WriteLine("Book removed successfully.");
        }
        else
        {
            Console.WriteLine("Book not found.");
        }
    }

    public int FindBook(string ISBN)
    {
        for (int I = 0; I < FCount; I++)
        {
            if (FBooks[I].ISBN == ISBN)
            {
                return I;
            }
        }
        return -1;
    }

    public void BorrowBook(string ISBN)
    {
        var Index = FindBook(ISBN);
        if (Index != -1 && FBooks[Index].Available)
        {
            FBooks[Index].Available = false;
            Console.WriteLine("Book borrowed successfully.");
        }
        else if (Index == -1)
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
        var Index = FindBook(ISBN);
        if (Index != -1 && !FBooks[Index].Available)
        {
            FBooks[Index].Available = true;
            Console.WriteLine("Book returned successfully.");
        }
        else if (Index == -1)
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
        foreach (var Book in FBooks)
        {
            Console.WriteLine($"Title: {Book.Title}");
            Console.WriteLine($"Author: {Book.Author}");
            Console.WriteLine($"ISBN: {Book.ISBN}");
            Console.WriteLine($"Available: {(Book.Available ? "Yes" : "No")}");
            Console.WriteLine();
        }
    }

    public int GetBookCount()
    {
        return FCount;
    }
}

class Program
{
    static void Main()
    {
        TLibrary LibraryObj = new TLibrary(100);
        TBook Book;
        int Choice;
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
            Choice = int.Parse(Console.ReadLine());

            switch (Choice)
            {
                case 1:
                    Book = new TBook();
                    Console.Write("Enter book title: ");
                    Book.Title = Console.ReadLine();
                    Console.Write("Enter book author: ");
                    Book.Author = Console.ReadLine();
                    Console.Write("Enter book ISBN: ");
                    Book.ISBN = Console.ReadLine();
                    Book.Available = true;
                    LibraryObj.AddBook(Book);
                    break;
                case 2:
                    Console.Write("Enter book ISBN: ");
                    ISBN = Console.ReadLine();
                    LibraryObj.RemoveBook(ISBN);
                    break;
                case 3:
                    Console.Write("Enter book ISBN: ");
                    ISBN = Console.ReadLine();
                    LibraryObj.BorrowBook(ISBN);
                    break;
                case 4:
                    Console.Write("Enter book ISBN: ");
                    ISBN = Console.ReadLine();
                    LibraryObj.ReturnBook(ISBN);
                    break;
                case 5:
                    LibraryObj.DisplayBooks();
                    break;
                case 6:
                    Console.WriteLine("Exiting the program.");
                    break;
                default:
                    Console.WriteLine("Invalid choice. Please try again.");
                    break;
            }

            Console.WriteLine();
        } while (Choice != 6);

        Console.WriteLine($"Total books in the library: {LibraryObj.GetBookCount()}");
        Console.ReadLine();
    }
}
