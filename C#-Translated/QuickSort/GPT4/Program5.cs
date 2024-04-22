namespace QuickSort10; // was not created I did it manually

using System;
using System.Collections.Generic;

class Program
{
    public class EtdTListException : Exception
    {
        public EtdTListException(string message) : base(message) { }
    }

    public delegate int TtdCompareFunc(object data1, object data2);

    static void TDValidateListRange(List<object> list, int start, int end, string message)
    {
        if (list == null)
            throw new EtdTListException($"List is nil:: {message}");
        if (start < 0 || start >= list.Count || end < 0 || end >= list.Count || start > end)
            throw new EtdTListException($"Invalid range: {start}-{end}: {message}");
    }

    static void QSS(List<object> list, int first, int last, TtdCompareFunc compare)
    {
        int L, R;
        object pivot, temp;

        while (first < last)
        {
            pivot = list[(first + last) / 2];
            L = first - 1;
            R = last + 1;
            while (true)
            {
                do { R--; } while (compare(list[R], pivot) > 0);
                do { L++; } while (compare(list[L], pivot) < 0);

                if (L >= R) break;

                temp = list[L];
                list[L] = list[R];
                list[R] = temp;
            }

            if (first < R)
                QSS(list, first, R, compare);

            first = R + 1;
        }
    }

    static void TDQuickSortStd(List<object> list, int first, int last, TtdCompareFunc compare)
    {
        TDValidateListRange(list, first, last, "TDQuickSortStd");
        QSS(list, first, last, compare);
    }

    static int TDCompareLongint(object data1, object data2)
    {
        int L1 = (int)data1;
        int L2 = (int)data2;

        return L1.CompareTo(L2);
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

            TDQuickSortStd(arr, 0, arr.Count - 1, new TtdCompareFunc(TDCompareLongint));

            foreach (int item in arr)
                Console.WriteLine(item);
        }
        finally
        {
            // Cleanup if needed
        }
    }
}
