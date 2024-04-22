namespace Pointers7; // was not created I did it manually

using System;
using System.Collections.Generic;

class Program
{
    public class TContact
    {
        public string Name { get; set; }
        public string Email { get; set; }
        public string PhoneNumber { get; set; }
    }

    public class TAddressBook
    {
        public List<TContact> Contacts { get; set; } = new List<TContact>();
        public int Count => Contacts.Count;
    }

    static void InitAddressBook(TAddressBook addressBook)
    {
        addressBook.Contacts.Clear();
    }

    static void FreeAddressBook(TAddressBook addressBook)
    {
        addressBook.Contacts.Clear();
    }

    static void AddContact(TAddressBook addressBook, string name, string email, string phoneNumber)
    {
        TContact newContact = new TContact
        {
            Name = name,
            Email = email,
            PhoneNumber = phoneNumber
        };
        addressBook.Contacts.Add(newContact);
    }

    static TContact SearchContact(TAddressBook addressBook, string name)
    {
        foreach (var contact in addressBook.Contacts)
        {
            if (contact.Name == name)
            {
                return contact;
            }
        }
        return null;
    }

    static void RemoveContact(TAddressBook addressBook, string name)
    {
        TContact contact = SearchContact(addressBook, name);
        if (contact != null)
        {
            addressBook.Contacts.Remove(contact);
        }
    }

    static void PrintContacts(TAddressBook addressBook)
    {
        foreach (var contact in addressBook.Contacts)
        {
            Console.WriteLine($"Name: {contact.Name}");
            Console.WriteLine($"Email: {contact.Email}");
            Console.WriteLine($"Phone Number: {contact.PhoneNumber}");
            Console.WriteLine();
        }
    }

    static void Main(string[] args)
    {
        TAddressBook addressBook = new TAddressBook();
        InitAddressBook(addressBook);

        string name, email, phoneNumber, command;
        do
        {
            Console.Write("Enter command (add/search/remove/print/exit): ");
            command = Console.ReadLine();

            switch (command.ToLower())
            {
                case "add":
                    Console.Write("Enter name: ");
                    name = Console.ReadLine();
                    Console.Write("Enter email: ");
                    email = Console.ReadLine();
                    Console.Write("Enter phone number: ");
                    phoneNumber = Console.ReadLine();
                    AddContact(addressBook, name, email, phoneNumber);
                    break;
                case "search":
                    Console.Write("Enter name to search: ");
                    name = Console.ReadLine();
                    TContact foundContact = SearchContact(addressBook, name);
                    if (foundContact != null)
                    {
                        Console.WriteLine($"Name: {foundContact.Name}");
                        Console.WriteLine($"Email: {foundContact.Email}");
                        Console.WriteLine($"Phone Number: {foundContact.PhoneNumber}");
                    }
                    else
                    {
                        Console.WriteLine("Contact not found");
                    }
                    break;
                case "remove":
                    Console.Write("Enter name to remove: ");
                    name = Console.ReadLine();
                    RemoveContact(addressBook, name);
                    break;
                case "print":
                    PrintContacts(addressBook);
                    break;
            }
        } while (command.ToLower() != "exit");

        FreeAddressBook(addressBook);
    }
}