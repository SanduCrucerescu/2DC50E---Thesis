namespace QuickSort6; // was not created I did it manually

using System;
using System.Collections;

internal class EtdTListException : Exception
{
    public EtdTListException(string message) : base(message) { }
}

internal delegate int TtdCompareFunc(object aData1, object aData2);

class Program
{
    internal static void TDValidateListRange(ArrayList aList, int aStart, int aEnd, string aMessage)
    {
        if (aList == null)
            throw new EtdTListException($"List is nil:: {aMessage}");
        if (aStart < 0 || aStart >= aList.Count || aEnd < 0 || aEnd >= aList.Count || aStart > aEnd)
            throw new EtdTListException($"Invalid range: {aStart}-{aEnd}: {aMessage}");
    }

    internal static void QSS(ArrayList aList, int aFirst, int aLast, TtdCompareFunc aCompare)
    {
        int L, R;
        object Pivot, Temp;
        while (aFirst < aLast)
        {
            Pivot = aList[(aFirst + aLast) / 2];
            L = aFirst - 1;
            R = aLast + 1;
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

                Temp = aList[L];
                aList[L] = aList[R];
                aList[R] = Temp;
            }
            if (aFirst < R)
                QSS(aList, aFirst, R, aCompare);
            aFirst = R + 1;
        }
    }

    internal static void TDQuickSortStd(ArrayList aList, int aFirst, int aLast, TtdCompareFunc aCompare)
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

    static void Main()
    {
        ArrayList arr = new ArrayList();

        try
        {
            arr.Add(5);
            arr.Add(876);
            arr.Add(234);
            arr.Add(9);
            arr.Add(12);
            arr.Add(44);
            arr.Add(2);

            TDQuickSortStd(arr, 0, arr.Count - 1, new TtdCompareFunc(TDCompareLongint));

            foreach (int item in arr)
            {
                Console.WriteLine(item);
            }
        }
        finally
        {
            // ArrayList does not need to be explicitly freed like TList in Delphi
        }
    }
}