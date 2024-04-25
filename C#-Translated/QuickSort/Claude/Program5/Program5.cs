namespace QuickSort5; // was not created I did it manually

using System;
using System.Collections.Generic;
class Program
{
    internal class EtdTListException : Exception
    {
        public EtdTListException(string message) : base(message) { }
    }

    internal delegate int TtdCompareFunc(object aData1, object aData2);

    internal static void TDValidateListRange(List<object> aList, int aStart, int aEnd, string aMessage)
    {
        if (aList == null)
            throw new EtdTListException(string.Format("List is nil:: {0}", aMessage));
        if (aStart < 0 || aStart >= aList.Count || aEnd < 0 ||
            aEnd >= aList.Count || aStart > aEnd)
            throw new EtdTListException(string.Format("Invalid range: {0}-{1}: {2}",
                aStart, aEnd, aMessage));
    }

    internal static void QSS(List<object> aList, int aFirst, int aLast, TtdCompareFunc aCompare)
    {
        while (aFirst < aLast)
        {
            object Pivot = aList[(aFirst + aLast) / 2];
            int L = aFirst - 1;
            int R = aLast + 1;
            while (true)
            {
                do
                {
                    R--;
                } while (aCompare(aList[R], Pivot) > 0);
                do
                {
                    L++;
                } while (aCompare(aList[L], Pivot) < 0);
                if (L >= R)
                    break;
                object Temp = aList[L];
                aList[L] = aList[R];
                aList[R] = Temp;
            }
            if (aFirst < R)
                QSS(aList, aFirst, R, aCompare);
            aFirst = R + 1;
        }
    }

    internal static void TDQuickSortStd(List<object> aList, int aFirst, int aLast, TtdCompareFunc aCompare)
    {
        TDValidateListRange(aList, aFirst, aLast, "TDQuickSortStd");
        QSS(aList, aFirst, aLast, aCompare);
    }

    internal static int TDCompareLongint(object aData1, object aData2)
    {
        int L1 = (int)aData1;
        int L2 = (int)aData2;
        if (L1 < L2)
            return -1;
        else if (L1 == L2)
            return 0;
        else
            return 1;
    }

    static void Main(string[] args)
    {
        List<object> arr = new List<object>();

        try
        {
            arr.Add(5);
            arr.Add(876);
            arr.Add(234);
            arr.Add(9);
            arr.Add(12);
            arr.Add(44);
            arr.Add(2);

            TDQuickSortStd(arr, 0, arr.Count - 1, TDCompareLongint);

            for (int i = 0; i < arr.Count; i++)
                Console.WriteLine((int)arr[i]);
        }
        finally
        {
            arr.Clear();
        }
    }
}
