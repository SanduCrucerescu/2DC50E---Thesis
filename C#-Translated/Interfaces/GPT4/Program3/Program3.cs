using System;
using System.Collections.Generic;

namespace Interfaces8
{
    public interface IProduct
    {
        string Name { get; }
        double Price { get; }
    }

    public class Product : IProduct
    {
        private string name;
        private double price;

        public string Name => name;
        public double Price => price;

        public Product(string name, double price)
        {
            this.name = name;
            this.price = price;
        }
    }

    public class ShoppingCart
    {
        internal List<IProduct> items = new List<IProduct>();

        public int Count => items.Count;

        public void AddItem(IProduct product)
        {
            items.Add(product);
        }

        public double GetTotalCost()
        {
            double totalCost = 0;
            foreach (var item in items)
            {
                totalCost += item.Price;
            }
            return totalCost;
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            ShoppingCart cart = new ShoppingCart();
            string productName;
            double productPrice;
            string choice;

            do
            {
                Console.Write("Enter product name (or \"x\" to finish): ");
                productName = Console.ReadLine();

                if (productName.ToLower() != "x")
                {
                    Console.Write("Enter product price: ");
                    productPrice = double.Parse(Console.ReadLine());

                    cart.AddItem(new Product(productName, productPrice));
                }
            } while (productName.ToLower() != "x");

            Console.WriteLine("Shopping Cart Contents:");
            Console.WriteLine("------------------------");
            foreach (var item in cart.items)
            {
                Console.WriteLine($"{item.Name}: ${item.Price}");
            }

            Console.WriteLine("------------------------");
            Console.WriteLine($"Total Cost: ${cart.GetTotalCost()}");

            Console.Write("Press Enter to exit...");
            choice = Console.ReadLine();
        }
    }
}