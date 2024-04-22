namespace QuickSort8; // was not created I did it manually

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

    private static void TDValidateListRange(List<int> list, int start, int end, string message)
    {
        if (list == null)
            throw new Exception($"List is nil:: {message}");
        if (start < 0 || start >= list.Count || end < 0 || end >= list.Count || start > end)
            throw new Exception($"Invalid range: {start}-{end}: {message}");
    }

    private static void QSS(List<int> list, int first, int last, Func<int, int, int> compare)
    {
        int l, r;
        int pivot;
        int temp;

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

    private static void TDQuickSortStd(List<int> list, int first, int last, Func<int, int, int> compare)
    {
        TDValidateListRange(list, first, last, "TDQuickSortStd");
        QSS(list, first, last, compare);
    }

    private static int TDCompareLongint(int data1, int data2)
    {
        if (data1 < data2)
            return -1;
        else if (data1 == data2)
            return 0;
        else
            return 1;
    }
}