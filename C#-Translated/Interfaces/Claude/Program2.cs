using System;
using System.Collections.Generic;

namespace Interfaces2
{
    interface IProduct
    {
        string Name { get; }
        double Price { get; }
    }

    class TProduct : IProduct
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

    class TShoppingCart
    {
        private List<IProduct> FItems;

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
            double TotalCost = 0;
            foreach (var item in FItems)
            {
                TotalCost += item.Price;
            }
            return TotalCost;
        }
    }

    class Program
    {
        static void Main()
        {
            TShoppingCart Cart = new TShoppingCart();
            try
            {
                while (true)
                {
                    Console.Write("Enter product name (or \"x\" to finish): ");
                    string ProductName = Console.ReadLine();

                    if (ProductName.ToLower() == "x")
                        break;

                    Console.Write("Enter product price: ");
                    double ProductPrice = double.Parse(Console.ReadLine());

                    Cart.AddItem(new TProduct(ProductName, ProductPrice));
                }

                Console.WriteLine("Shopping Cart Contents:");
                Console.WriteLine("------------------------");
                for (int I = 0; I < Cart.Count; I++)
                {
                    Console.WriteLine($"{Cart.FItems[I].Name}: ${Cart.FItems[I].Price}");
                }

                Console.WriteLine("------------------------");
                Console.WriteLine($"Total Cost: ${Cart.GetTotalCost()}");

                Console.Write("Press Enter to exit...");
                Console.ReadLine();
            }
            finally
            {
                Cart = null;
            }
        }
    }
}