namespace Pointers3; // was not created I did it manually

using System;
using System.Collections.Generic;

/*
The Contact and AddressBook types are defined as classes.
The AddressBook class uses a List<Contact> to store the contacts instead of an array.
The InitAddressBook and FreeAddressBook procedures are replaced by the constructor of the AddressBook class.
The SearchContact method uses the List<T>.Find method to search for a contact by name.
The RemoveContact method uses the List<T>.Remove method to remove a contact from the list.
The PrintContacts method uses a foreach loop to iterate over the contacts and print their details.
The main program logic remains the same, using a switch statement to handle user commands.
*/

class Program
{
    internal class Contact
    {
        public string Name;
        public string Email;
        public string PhoneNumber;
    }

    internal class AddressBook
    {
        public List<Contact> Contacts;
        public int Count;

        public AddressBook()
        {
            Contacts = new List<Contact>();
            Count = 0;
        }

        public void AddContact(string name, string email, string phoneNumber)
        {
            Contact newContact = new Contact
            {
                Name = name,
                Email = email,
                PhoneNumber = phoneNumber
            };
            Contacts.Add(newContact);
            Count++;
        }

        public Contact SearchContact(string name)
        {
            return Contacts.Find(contact => contact.Name == name);
        }

        public void RemoveContact(string name)
        {
            Contact contact = SearchContact(name);
            if (contact != null)
            {
                Contacts.Remove(contact);
                Count--;
            }
        }

        public void PrintContacts()
        {
            foreach (Contact contact in Contacts)
            {
                Console.WriteLine("Name: " + contact.Name);
                Console.WriteLine("Email: " + contact.Email);
                Console.WriteLine("Phone Number: " + contact.PhoneNumber);
                Console.WriteLine();
            }
        }
    }

    internal static void Main(string[] args)
    {
        AddressBook addressBook = new AddressBook();
        string name, email, phoneNumber, command;

        while (true)
        {
            Console.Write("Enter command (add/search/remove/print/exit): ");
            command = Console.ReadLine();

            switch (command)
            {
                case "add":
                    Console.Write("Enter name: ");
                    name = Console.ReadLine();
                    Console.Write("Enter email: ");
                    email = Console.ReadLine();
                    Console.Write("Enter phone number: ");
                    phoneNumber = Console.ReadLine();
                    addressBook.AddContact(name, email, phoneNumber);
                    break;
                case "search":
                    Console.Write("Enter name to search: ");
                    name = Console.ReadLine();
                    Contact foundContact = addressBook.SearchContact(name);
                    if (foundContact != null)
                    {
                        Console.WriteLine("Name: " + foundContact.Name);
                        Console.WriteLine("Email: " + foundContact.Email);
                        Console.WriteLine("Phone Number: " + foundContact.PhoneNumber);
                    }
                    else
                    {
                        Console.WriteLine("Contact not found");
                    }
                    break;
                case "remove":
                    Console.Write("Enter name to remove: ");
                    name = Console.ReadLine();
                    addressBook.RemoveContact(name);
                    break;
                case "print":
                    addressBook.PrintContacts();
                    break;
                case "exit":
                    return;
                default:
                    Console.WriteLine("Invalid command");
                    break;
            }
        }
    }
}