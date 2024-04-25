using System;
using System.Collections.Generic;

namespace Interfaces9
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

        public string Name
        {
            get { return _name; }
        }

        public double Price
        {
            get { return _price; }
        }
    }

    public class ShoppingCart
    {
        internal List<IProduct> _items;

        public ShoppingCart()
        {
            _items = new List<IProduct>();
        }

        public int Count
        {
            get { return _items.Count; }
        }

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
            ShoppingCart cart = new ShoppingCart();
            try
            {
                string productName;
                double productPrice;
                do
                {
                    Console.Write("Enter product name (or \"x\" to finish): ");
                    productName = Console.ReadLine();

                    if (productName.ToLower() != "x")
                    {
                        Console.Write("Enter product price: ");
                        productPrice = Convert.ToDouble(Console.ReadLine());

                        cart.AddItem(new Product(productName, productPrice));
                    }

                } while (productName.ToLower() != "x");

                Console.WriteLine("Shopping Cart Contents:");
                Console.WriteLine("------------------------");
                foreach (var item in cart._items)
                {
                    Console.WriteLine($"{item.Name}: ${item.Price}");
                }

                Console.WriteLine("------------------------");
                Console.WriteLine($"Total Cost: ${cart.GetTotalCost()}");

                Console.Write("Press Enter to exit...");
                Console.ReadLine();
            }
            finally
            {
                // In C#, the garbage collector handles object cleanup,
                // so explicit freeing of resources is generally not needed unless managing unmanaged resources.
            }
        }
    }
}