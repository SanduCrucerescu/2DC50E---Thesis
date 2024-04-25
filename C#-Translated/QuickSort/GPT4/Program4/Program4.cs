namespace QuickSort9; // was not created I did it manually

using System;
using System.Collections.Generic;

internal class EtdTListException : Exception
{
    public EtdTListException(string message) : base(message)
    {
    }
}

internal delegate int TtdCompareFunc(object data1, object data2);

class Program
{
    internal static void TDValidateListRange(List<object> list, int start, int end, string message)
    {
        if (list == null)
            throw new EtdTListException($"List is nil:: {message}");
        if (start < 0 || start >= list.Count || end < 0 || end >= list.Count || start > end)
            throw new EtdTListException($"Invalid range: {start}-{end}: {message}");
    }

    internal static void QSS(List<object> list, int first, int last, TtdCompareFunc compare)
    {
        int l, r;
        object pivot, temp;

        while (first < last)
        {
            pivot = list[(first + last) / 2];
            l = first - 1;
            r = last + 1;

            while (true)
            {
                do
                {
                    r--;
                } while (compare(list[r], pivot) > 0);

                do
                {
                    l++;
                } while (compare(list[l], pivot) < 0);

                if (l >= r)
                    break;

                temp = list[l];
                list[l] = list[r];
                list[r] = temp;
            }

            if (first < r)
                QSS(list, first, r, compare);
            first = r + 1;
        }
    }

    internal static void TDQuickSortStd(List<object> list, int first, int last, TtdCompareFunc compare)
    {
        TDValidateListRange(list, first, last, "TDQuickSortStd");
        QSS(list, first, last, compare);
    }

    internal static int TDCompareLongint(object data1, object data2)
    {
        int l1 = (int)data1;
        int l2 = (int)data2;

        if (l1 < l2)
            return -1;
        else if (l1 == l2)
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

            foreach (var item in arr)
                Console.WriteLine((int)item);
        }
        finally
        {
            // Cleanup if needed
        }
    }
}