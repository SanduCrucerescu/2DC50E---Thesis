using System;
using System.Collections.Generic;

namespace Interfaces4
{
    public interface IProduct
    {
        string Name { get; }
        double Price { get; }
    }

    public class Product : IProduct
    {
        private string _name;
        private double _price;

        public Product(string name, double price)
        {
            _name = name;
            _price = price;
        }

        public string Name => _name;

        public double Price => _price;
    }

    public class ShoppingCart
    {
        private List<IProduct> _items;

        public ShoppingCart()
        {
            _items = new List<IProduct>();
        }

        public int Count => _items.Count;

        public void AddItem(IProduct product)
        {
            _items.Add(product);
        }

        public double GetTotalCost()
        {
            double totalCost = 0;
            foreach (var item in _items)
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
            var cart = new ShoppingCart();

            while (true)
            {
                Console.Write("Enter product name (or 'x' to finish): ");
                string productName = Console.ReadLine();

                if (productName.ToLower() == "x")
                    break;

                Console.Write("Enter product price: ");
                double productPrice = double.Parse(Console.ReadLine());

                cart.AddItem(new Product(productName, productPrice));
            }

            Console.WriteLine("Shopping Cart Contents:");
            Console.WriteLine("------------------------");
            for (int i = 0; i < cart.Count; i++)
            {
                var item = cart._items[i];
                Console.WriteLine($"{item.Name}: ${item.Price}");
            }

            Console.WriteLine("------------------------");
            Console.WriteLine($"Total Cost: ${cart.GetTotalCost()}");

            Console.Write("Press Enter to exit...");
            Console.ReadLine();
        }
    }
}