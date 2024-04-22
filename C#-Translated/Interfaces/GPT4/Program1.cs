using System;
using System.Collections.Generic;

namespace Interfaces6
{
    interface IProduct
    {
        string GetName();
        double GetPrice();
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

        public string GetName()
        {
            return FName;
        }

        public double GetPrice()
        {
            return FPrice;
        }

        public string Name
        {
            get { return GetName(); }
        }

        public double Price
        {
            get { return GetPrice(); }
        }
    }

    class TShoppingCart
    {
        private List<IProduct> FItems;

        public TShoppingCart()
        {
            FItems = new List<IProduct>();
        }

        ~TShoppingCart()
        {
            FItems.Clear();
        }

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

        public int Count
        {
            get { return FItems.Count; }
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            TShoppingCart Cart = new TShoppingCart();
            try
            {
                string ProductName;
                double ProductPrice;
                string Choice;

                do
                {
                    Console.Write("Enter product name (or \"x\" to finish): ");
                    ProductName = Console.ReadLine();

                    if (ProductName.ToLower() != "x")
                    {
                        Console.Write("Enter product price: ");
                        ProductPrice = double.Parse(Console.ReadLine());

                        Cart.AddItem(new TProduct(ProductName, ProductPrice));
                    }

                } while (ProductName.ToLower() != "x");

                Console.WriteLine("Shopping Cart Contents:");
                Console.WriteLine("------------------------");
                foreach (IProduct item in Cart.FItems)
                {
                    Console.WriteLine($"{item.Name}: ${item.Price}");
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