using NUnit.Framework;
using System;
using System.Collections.Generic;

namespace QuickSortTests
{
    [TestFixture]
    public class QuickSortTests
    {
        [Test]
        public void SmokeTest_TDQuickSortStd_SortsListCorrectly()
        {
            // Arrange
            var list = new List<int> { 5, 876, 234, 9, 12, 44, 2 };

            // Act
            TDQuickSortStd(list, 0, list.Count - 1, TDCompareLongint);

            // Assert
            Assert.That(list, Is.Ordered);
        }

        [Test]
        public void UnitTest_TDValidateListRange_ThrowsExceptionForNullList()
        {
            // Arrange
            List<int> list = null;

            // Act & Assert
            Assert.Throws<EtdTListException>(() => TDValidateListRange(list, 0, 0, "Test"));
        }

        [Test]
        public void UnitTest_TDValidateListRange_ThrowsExceptionForInvalidRange()
        {
            // Arrange
            var list = new List<int> { 1, 2, 3 };

            // Act & Assert
            Assert.Throws<EtdTListException>(() => TDValidateListRange(list, -1, 2, "Test"));
            Assert.Throws<EtdTListException>(() => TDValidateListRange(list, 0, 3, "Test"));
            Assert.Throws<EtdTListException>(() => TDValidateListRange(list, 2, 1, "Test"));
        }

        [Test]
        public void FunctionalTest_TDQuickSortStd_SortsEmptyList()
        {
            // Arrange
            var list = new List<int>();

            // Act
            TDQuickSortStd(list, 0, list.Count - 1, TDCompareLongint);

            // Assert
            Assert.That(list, Is.Empty);
        }

        [Test]
        public void FunctionalTest_TDQuickSortStd_SortsSingleElementList()
        {
            // Arrange
            var list = new List<int> { 42 };

            // Act
            TDQuickSortStd(list, 0, list.Count - 1, TDCompareLongint);

            // Assert
            Assert.That(list, Is.Ordered);
            Assert.That(list[0], Is.EqualTo(42));
        }

        [Test]
        public void FunctionalTest_TDQuickSortStd_SortsAlreadySortedList()
        {
            // Arrange
            var list = new List<int> { 1, 2, 3, 4, 5 };

            // Act
            TDQuickSortStd(list, 0, list.Count - 1, TDCompareLongint);

            // Assert
            Assert.That(list, Is.Ordered);
            Assert.That(list, Is.EqualTo(new[] { 1, 2, 3, 4, 5 }));
        }

        [Test]
        public void FunctionalTest_TDQuickSortStd_SortsReverseSortedList()
        {
            // Arrange
            var list = new List<int> { 5, 4, 3, 2, 1 };

            // Act
            TDQuickSortStd(list, 0, list.Count - 1, TDCompareLongint);

            // Assert
            Assert.That(list, Is.Ordered);
            Assert.That(list, Is.EqualTo(new[] { 1, 2, 3, 4, 5 }));
        }

        [Test]
        public void FunctionalTest_TDQuickSortStd_SortsListWithDuplicates()
        {
            // Arrange
            var list = new List<int> { 5, 2, 3, 2, 1, 4, 2 };

            // Act
            TDQuickSortStd(list, 0, list.Count - 1, TDCompareLongint);

            // Assert
            Assert.That(list, Is.Ordered);
            Assert.That(list, Is.EqualTo(new[] { 1, 2, 2, 2, 3, 4, 5 }));
        }

        [Test]
        public void FunctionalTest_TDQuickSortStd_SortsLargeList()
        {
            // Arrange
            var random = new Random();
            var list = new List<int>();
            for (int i = 0; i < 10000; i++)
            {
                list.Add(random.Next());
            }

            // Act
            TDQuickSortStd(list, 0, list.Count - 1, TDCompareLongint);

            // Assert
            Assert.That(list, Is.Ordered);
        }

        [Test]
        public void FunctionalTest_TDQuickSortStd_SortsListWithNegativeNumbers()
        {
            // Arrange
            var list = new List<int> { -5, 2, -3, 0, 1, -4, 2 };

            // Act
            TDQuickSortStd(list, 0, list.Count - 1, TDCompareLongint);

            // Assert
            Assert.That(list, Is.Ordered);
            Assert.That(list, Is.EqualTo(new[] { -5, -4, -3, 0, 1, 2, 2 }));
        }

        [Test]
        public void FunctionalTest_TDQuickSortStd_SortsListWithAllDuplicates()
        {
            // Arrange
            var list = new List<int> { 5, 5, 5, 5, 5 };

            // Act
            TDQuickSortStd(list, 0, list.Count - 1, TDCompareLongint);

            // Assert
            Assert.That(list, Is.Ordered);
            Assert.That(list, Is.EqualTo(new[] { 5, 5, 5, 5, 5 }));
        }

        [Test]
        public void FunctionalTest_TDCompareLongint_ReturnsCorrectComparisonResult()
        {
            // Arrange
            int a = 10;
            int b = 20;

            // Act & Assert
            Assert.That(TDCompareLongint(a, b), Is.EqualTo(-1));
            Assert.That(TDCompareLongint(b, a), Is.EqualTo(1));
            Assert.That(TDCompareLongint(a, a), Is.EqualTo(0));
        }

        [Test]
        public void PerformanceTest_TDQuickSortStd_SortsLargeListEfficiently()
        {
            // Arrange
            var random = new Random();
            var list = new List<int>();
            for (int i = 0; i < 100000; i++)
            {
                list.Add(random.Next());
            }

            // Act
            var stopwatch = System.Diagnostics.Stopwatch.StartNew();
            TDQuickSortStd(list, 0, list.Count - 1, TDCompareLongint);
            stopwatch.Stop();

            // Assert
            Assert.That(list, Is.Ordered);
            Assert.That(stopwatch.ElapsedMilliseconds, Is.LessThan(100)); // Adjust the threshold as needed
        }
    }
}