using System;

namespace LibraryManagementSystem4
{
    class Book
    {
        public string Title { get; set; }
        public string Author { get; set; }
        public string ISBN { get; set; }
        public bool Available { get; set; }
    }

    class Library
    {
        internal Book[] books;
        private int capacity;
        private int count;

        public Library(int capacity)
        {
            this.capacity = capacity;
            count = 0;
            books = new Book[capacity];
        }

        public void AddBook(Book book)
        {
            if (count < capacity)
            {
                books[count] = book;
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
            int index = FindBook(isbn);
            if (index != -1)
            {
                for (int i = index; i < count - 1; i++)
                {
                    books[i] = books[i + 1];
                }
                count--;
                Console.WriteLine("Book removed successfully.");
            }
            else
            {
                Console.WriteLine("Book not found.");
            }
        }

        public int FindBook(string isbn)
        {
            for (int i = 0; i < count; i++)
            {
                if (books[i].ISBN == isbn)
                {
                    return i;
                }
            }
            return -1;
        }

        public void BorrowBook(string isbn)
        {
            int index = FindBook(isbn);
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

        public void ReturnBook(string isbn)
        {
            int index = FindBook(isbn);
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
            for (int i = 0; i < count; i++)
            {
                Console.WriteLine($"Title: {books[i].Title}");
                Console.WriteLine($"Author: {books[i].Author}");
                Console.WriteLine($"ISBN: {books[i].ISBN}");
                Console.WriteLine($"Available: {(books[i].Available ? "Yes" : "No")}");
                Console.WriteLine();
            }
        }

        public int GetBookCount()
        {
            return count;
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            Library library = new Library(100);

            while (true)
            {
                Console.WriteLine("Library Management System");
                Console.WriteLine("1. Add Book");
                Console.WriteLine("2. Remove Book");
                Console.WriteLine("3. Borrow Book");
                Console.WriteLine("4. Return Book");
                Console.WriteLine("5. Display Books");
                Console.WriteLine("6. Exit");
                Console.Write("Enter your choice: ");
                int choice = int.Parse(Console.ReadLine());

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
                        library.AddBook(book);
                        break;
                    case 2:
                        Console.Write("Enter book ISBN: ");
                        string removeIsbn = Console.ReadLine();
                        library.RemoveBook(removeIsbn);
                        break;
                    case 3:
                        Console.Write("Enter book ISBN: ");
                        string borrowIsbn = Console.ReadLine();
                        library.BorrowBook(borrowIsbn);
                        break;
                    case 4:
                        Console.Write("Enter book ISBN: ");
                        string returnIsbn = Console.ReadLine();
                        library.ReturnBook(returnIsbn);
                        break;
                    case 5:
                        library.DisplayBooks();
                        break;
                    case 6:
                        Console.WriteLine("Exiting the program.");
                        return;
                    default:
                        Console.WriteLine("Invalid choice. Please try again.");
                        break;
                }

                Console.WriteLine();
            }

            Console.WriteLine($"Total books in the library: {library.GetBookCount()}");
        }
    }
}