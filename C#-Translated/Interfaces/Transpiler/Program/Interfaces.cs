namespace interfaces
{
    using SysUtils;

    public static class Program
    {
        private static TShoppingCart Cart;
        private static string ProductName;
        private static double ProductPrice;
        private static string Choice;
        private static int I;
        public static void Main(string[] args)
        {
            Cart = TShoppingCart.Create;
            try
            {
                do
                {
                    Write("Enter product name (or \"x\" to finish): ");
                    ReadLn(ProductName);
                    if (ProductName.ToLower != "x")
                    {
                        Write("Enter product price: ");
                        ReadLn(ProductPrice);
                        Cart.AddItem(TProduct.Create(ProductName, ProductPrice));
                    }
                }
                while (ProductName.ToLower == "x");
                WriteLn("Shopping Cart Contents:");
                WriteLn("------------------------");
                for (int i = 0; i < Cart.Count - 1; i++)
                    WriteLn(Cart.FItems[I]() + ": $" + Cart.FItems[I]()());
                WriteLn("------------------------");
                WriteLn("Total Cost: $" + Cart.GetTotalCost.ToString);
                Write("Press Enter to exit...");
                ReadLn(Choice);
            }
            finally
            {
                Cart.Free;
            }
        }

        interface IProduct
        {
            string GetName()
            {
                string Result;
                return Result;
            }

            double GetPrice()
            {
                double Result;
                return Result;
            }

            string<> Name double<> Price
        }

        private class TProduct : TInterfacedObject, IProduct
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
                string Result;
                Result = FName;
                return Result;
            }

            public double GetPrice()
            {
                double Result;
                Result = FPrice;
                return Result;
            }
        }

        private class TShoppingCart
        {
            integer<> Count private IProduct[] FItems;
            private int GetCount()
            {
                int Result;
                Result = Length(FItems);
                return Result;
            }

            public TShoppingCart()
            {
                SetLength(FItems, 0);
            }

            override public ~TShoppingCart()
            {
                for (int i = 0; i < High(FItems); i++)
                    FItems[I] = null;
                base;
            }

            public void AddItem(IProduct AProduct)
            {
                SetLength(FItems, Length(FItems) + 1);
                FItems[High(FItems)] = AProduct;
            }

            public double GetTotalCost()
            {
                double Result;
                TotalCost = 0;
                for (int i = 0; i < High(FItems); i++)
                    TotalCost = TotalCost + FItems[I]();
                Result = TotalCost;
                return Result;
            }
        }
    }
}