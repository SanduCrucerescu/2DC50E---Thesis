using System;
using System.Collections.Generic;

namespace Interfaces5
{
    public interface IProduct
    {
        string Name { get; }
        double Price { get; }
    }

    public class TProduct : IProduct
    {
        private string FName;
        private double FPrice;

        public TProduct(string AName, double APrice)
        {
            FName = AName;
            FPrice = APrice;
        }

        public string Name => FName;
        public double Price => FPrice;
    }

    public class TShoppingCart
    {
        internal List<IProduct> FItems;

        public TShoppingCart()
        {
            FItems = new List<IProduct>();
        }

        public int Count => FItems.Count;

        public void AddItem(IProduct AProduct)
        {
            FItems.Add(AProduct);
        }

        public double GetTotalCost()
        {
            double totalCost = 0;
            foreach (var item in FItems)
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
            var cart = new TShoppingCart();

            try
            {
                while (true)
                {
                    Console.Write("Enter product name (or \"x\" to finish): ");
                    string productName = Console.ReadLine();

                    if (productName.ToLower() == "x")
                        break;

                    Console.Write("Enter product price: ");
                    double productPrice = double.Parse(Console.ReadLine());

                    cart.AddItem(new TProduct(productName, productPrice));
                }

                Console.WriteLine("Shopping Cart Contents:");
                Console.WriteLine("------------------------");
                for (int i = 0; i < cart.Count; i++)
                {
                    Console.WriteLine($"{cart.FItems[i].Name}: ${cart.FItems[i].Price}");
                }

                Console.WriteLine("------------------------");
                Console.WriteLine($"Total Cost: ${cart.GetTotalCost()}");

                Console.Write("Press Enter to exit...");
                Console.ReadLine();
            }
            finally
            {
                cart = null;
            }
        }
    }
}