namespace QuickSort4; // was not created I did it manually

/*
Using List<object> instead of TList for the list data structure.
Using delegate for the TtdCompareFunc type.
Adjusting the syntax for exception handling and string interpolation.
Using object type for the list elements and casting them to int when needed.
Replacing WriteLn with Console.WriteLine for output.
*/

using System;
using System.Collections.Generic;

class Program
{
    class EtdTListException : Exception
    {
        public EtdTListException(string message) : base(message) { }
    }

    delegate int TtdCompareFunc(object aData1, object aData2);

    static void TDValidateListRange(List<object> aList, int aStart, int aEnd, string aMessage)
    {
        if (aList == null)
            throw new EtdTListException($"List is nil:: {aMessage}");
        if (aStart < 0 || aStart >= aList.Count || aEnd < 0 ||
            aEnd >= aList.Count || aStart > aEnd)
            throw new EtdTListException($"Invalid range: {aStart}-{aEnd}: {aMessage}");
    }

    static void QSS(List<object> aList, int aFirst, int aLast, TtdCompareFunc aCompare)
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

    static void TDQuickSortStd(List<object> aList, int aFirst, int aLast, TtdCompareFunc aCompare)
    {
        TDValidateListRange(aList, aFirst, aLast, "TDQuickSortStd");
        QSS(aList, aFirst, aLast, aCompare);
    }

    static int TDCompareLongint(object aData1, object aData2)
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

    static void Main()
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