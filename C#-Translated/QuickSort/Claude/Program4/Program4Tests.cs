using NUnit.Framework;
using System;
using System.Collections.Generic;

namespace QuickSort4.Test
{
    [TestFixture]
    public class QuickSortTests
    {
        [Test]
        public void SmokeTest_TDQuickSortStd_SortsListCorrectly()
        {
            var list = new List<object> { 5, 876, 234, 9, 12, 44, 2 };

            Program.TDQuickSortStd(list, 0, list.Count - 1, Program.TDCompareLongint);

            Assert.That(list, Is.Ordered);
        }

        [Test]
        public void UnitTest_TDValidateListRange_ThrowsExceptionForNullList()
        {
            List<object> list = null;

            Assert.Throws<Program.EtdTListException>(() => Program.TDValidateListRange(list, 0, 0, "Test"));
        }

        [Test]
        public void UnitTest_TDValidateListRange_ThrowsExceptionForInvalidRange()
        {
            var list = new List<object> { 1, 2, 3 };

            Assert.Throws<Program.EtdTListException>(() => Program.TDValidateListRange(list, -1, 2, "Test"));
            Assert.Throws<Program.EtdTListException>(() => Program.TDValidateListRange(list, 0, 3, "Test"));
            Assert.Throws<Program.EtdTListException>(() => Program.TDValidateListRange(list, 2, 1, "Test"));
        }

        [Test]
        public void FunctionalTest_TDQuickSortStd_SortsEmptyList()
        {
            var list = new List<object>();

            Assert.Throws<Program.EtdTListException>(() => Program.TDQuickSortStd(list, 0, list.Count - 1, Program.TDCompareLongint));
        }

        [Test]
        public void FunctionalTest_TDQuickSortStd_SortsSingleElementList()
        {
            var list = new List<object> { 42 };

            Program.TDQuickSortStd(list, 0, list.Count - 1, Program.TDCompareLongint);

            Assert.That(list, Is.Ordered);
            Assert.That(list[0], Is.EqualTo(42));
        }

        [Test]
        public void FunctionalTest_TDQuickSortStd_SortsAlreadySortedList()
        {
            var list = new List<object> { 1, 2, 3, 4, 5 };

            Program.TDQuickSortStd(list, 0, list.Count - 1, Program.TDCompareLongint);

            Assert.That(list, Is.Ordered);
            Assert.That(list, Is.EqualTo(new[] { 1, 2, 3, 4, 5 }));
        }

        [Test]
        public void FunctionalTest_TDQuickSortStd_SortsReverseSortedList()
        {
            var list = new List<object> { 5, 4, 3, 2, 1 };

            Program.TDQuickSortStd(list, 0, list.Count - 1, Program.TDCompareLongint);

            Assert.That(list, Is.Ordered);
            Assert.That(list, Is.EqualTo(new[] { 1, 2, 3, 4, 5 }));
        }

        [Test]
        public void FunctionalTest_TDQuickSortStd_SortsListWithDuplicates()
        {
            var list = new List<object> { 5, 2, 3, 2, 1, 4, 2 };

            Program.TDQuickSortStd(list, 0, list.Count - 1, Program.TDCompareLongint);

            Assert.That(list, Is.Ordered);
            Assert.That(list, Is.EqualTo(new[] { 1, 2, 2, 2, 3, 4, 5 }));
        }

        [Test]
        public void FunctionalTest_TDQuickSortStd_SortsLargeList()
        {
            var random = new Random();
            var list = new List<object>();
            for (int i = 0; i < 10000; i++)
            {
                list.Add(random.Next());
            }

            Program.TDQuickSortStd(list, 0, list.Count - 1, Program.TDCompareLongint);

            Assert.That(list, Is.Ordered);
        }

        [Test]
        public void FunctionalTest_TDQuickSortStd_SortsListWithNegativeNumbers()
        {
            var list = new List<object> { -5, 2, -3, 0, 1, -4, 2 };

            Program.TDQuickSortStd(list, 0, list.Count - 1, Program.TDCompareLongint);

            Assert.That(list, Is.Ordered);
            Assert.That(list, Is.EqualTo(new[] { -5, -4, -3, 0, 1, 2, 2 }));
        }

        [Test]
        public void FunctionalTest_TDQuickSortStd_SortsListWithAllDuplicates()
        {
            var list = new List<object> { 5, 5, 5, 5, 5 };

            Program.TDQuickSortStd(list, 0, list.Count - 1, Program.TDCompareLongint);

            Assert.That(list, Is.Ordered);
            Assert.That(list, Is.EqualTo(new[] { 5, 5, 5, 5, 5 }));
        }

        [Test]
        public void FunctionalTest_TDCompareLongint_ReturnsCorrectComparisonResult()
        {
            int a = 10;
            int b = 20;

            Assert.That(Program.TDCompareLongint(a, b), Is.EqualTo(-1));
            Assert.That(Program.TDCompareLongint(b, a), Is.EqualTo(1));
            Assert.That(Program.TDCompareLongint(a, a), Is.EqualTo(0));
        }

        [Test]
        public void PerformanceTest_TDQuickSortStd_SortsLargeListEfficiently()
        {
            var random = new Random();
            var list = new List<object>();
            for (int i = 0; i < 100000; i++)
            {
                list.Add(random.Next());
            }

            var stopwatch = System.Diagnostics.Stopwatch.StartNew();
            Program.TDQuickSortStd(list, 0, list.Count - 1, Program.TDCompareLongint);
            stopwatch.Stop();

            Assert.That(list, Is.Ordered);
            Assert.That(stopwatch.ElapsedMilliseconds, Is.LessThan(100)); // Adjust the threshold as needed
        }
    }
}