namespace QuickSort7; // was not created I did it manually

using System;
using System.Collections.Generic;

class Program
{
    static void Main(string[] args)
    {
        List<int> arr = new List<int> { 5, 876, 234, 9, 12, 44, 2 };
        TDQuickSortStd(arr, 0, arr.Count - 1, TDCompareLongint);

        foreach (int item in arr)
        {
            Console.WriteLine(item);
        }
    }

    internal static void TDValidateListRange(List<int> aList, int aStart, int aEnd, string aMessage)
    {
        if (aList == null)
        {
            throw new Exception($"List is nil:: {aMessage}");
        }

        if (aStart < 0 || aStart >= aList.Count || aEnd < 0 || aEnd >= aList.Count || aStart > aEnd)
        {
            throw new Exception($"Invalid range: {aStart}-{aEnd}: {aMessage}");
        }
    }

    internal static void QSS(List<int> aList, int aFirst, int aLast, Func<int, int, int> aCompare)
    {
        int L, R, pivot, temp;
        while (aFirst < aLast)
        {
            pivot = aList[(aFirst + aLast) / 2];
            L = aFirst - 1;
            R = aLast + 1;

            while (true)
            {
                do { R--; } while (aCompare(aList[R], pivot) > 0);
                do { L++; } while (aCompare(aList[L], pivot) < 0);

                if (L >= R) break;

                temp = aList[L];
                aList[L] = aList[R];
                aList[R] = temp;
            }

            if (aFirst < R) QSS(aList, aFirst, R, aCompare);
            aFirst = R + 1; // recursion removal step
        }
    }

    internal static void TDQuickSortStd(List<int> aList, int aFirst, int aLast, Func<int, int, int> aCompare)
    {
        TDValidateListRange(aList, aFirst, aLast, "TDQuickSortStd");
        QSS(aList, aFirst, aLast, aCompare);
    }

    internal static int TDCompareLongint(int aData1, int aData2)
    {
        if (aData1 < aData2) return -1;
        else if (aData1 == aData2) return 0;
        else return 1;
    }
}