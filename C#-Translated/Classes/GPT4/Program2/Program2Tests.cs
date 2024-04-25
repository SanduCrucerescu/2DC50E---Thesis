using System;
using System.IO;
using NUnit.Framework;
using NUnit.Framework.Legacy;

namespace Classes7.Test
{

    [TestFixture]
    public class TaskManagerTests
    {
        private TTaskList taskList;

        [SetUp]
        public void Setup()
        {
            taskList = new TTaskList();
        }

        // Smoke Tests
        [Test]
        public void SmokeTest_CreateTask()
        {
            string title = "Test Task";
            string description = "This is a test task";
            DateTime dueDate = DateTime.Now.AddDays(7);
            int priority = 1;

            TTask task = new TTask(title, description, dueDate, priority);

            ClassicAssert.IsNotNull(task);
            ClassicAssert.AreEqual(title, task.Title);
            ClassicAssert.AreEqual(description, task.Description);
            ClassicAssert.AreEqual(dueDate, task.DueDate);
            ClassicAssert.AreEqual(priority, task.Priority);
            ClassicAssert.IsFalse(task.Completed);
        }

        [Test]
        public void SmokeTest_AddTask()
        {
            TTask task = new TTask("Test Task", "This is a test task", DateTime.Now.AddDays(7), 1);

            taskList.AddTask(task);

            ClassicAssert.AreEqual(1, taskList.GetTaskCount());
        }

        // Unit Tests
        [Test]
        public void UnitTest_MarkTaskAsCompleted()
        {
            TTask task = new TTask("Test Task", "This is a test task", DateTime.Now.AddDays(7), 1);

            task.MarkAsCompleted();

            ClassicAssert.IsTrue(task.Completed);
        }

        [Test]
        public void UnitTest_RemoveTask()
        {
            TTask task = new TTask("Test Task", "This is a test task", DateTime.Now.AddDays(7), 1);
            taskList.AddTask(task);

            taskList.RemoveTask(task);

            ClassicAssert.AreEqual(0, taskList.GetTaskCount());
        }

        [Test]
        public void UnitTest_GetTask()
        {
            TTask task1 = new TTask("Task 1", "This is task 1", DateTime.Now.AddDays(7), 1);
            TTask task2 = new TTask("Task 2", "This is task 2", DateTime.Now.AddDays(14), 2);
            taskList.AddTask(task1);
            taskList.AddTask(task2);

            TTask retrievedTask = taskList.GetTask(1);

            ClassicAssert.AreEqual(task2, retrievedTask);
        }

        // Functional Tests
        [Test]
        public void FunctionalTest_SortTasksByPriority()
        {
            TTask task1 = new TTask("Task 1", "This is task 1", DateTime.Now.AddDays(7), 2);
            TTask task2 = new TTask("Task 2", "This is task 2", DateTime.Now.AddDays(14), 1);
            TTask task3 = new TTask("Task 3", "This is task 3", DateTime.Now.AddDays(21), 3);
            taskList.AddTask(task1);
            taskList.AddTask(task2);
            taskList.AddTask(task3);

            taskList.SortTasksByPriority();

            ClassicAssert.AreEqual(task2, taskList.GetTask(0));
            ClassicAssert.AreEqual(task1, taskList.GetTask(1));
            ClassicAssert.AreEqual(task3, taskList.GetTask(2));
        }

        [Test]
        public void FunctionalTest_SaveAndLoadTasks()
        {
            string fileName = "test_tasks.txt";
            TTask task1 = new TTask("Task 1", "This is task 1", DateTime.Now.AddDays(7), 1);
            TTask task2 = new TTask("Task 2", "This is task 2", DateTime.Now.AddDays(14), 2);
            taskList.AddTask(task1);
            taskList.AddTask(task2);

            taskList.SaveTasksToFile(fileName);
            taskList.RemoveTask(task1);
            taskList.RemoveTask(task2);
            taskList.LoadTasksFromFile(fileName);

            ClassicAssert.AreEqual(2, taskList.GetTaskCount());
            ClassicAssert.AreEqual(task1.Title, taskList.GetTask(0).Title);
            ClassicAssert.AreEqual(task2.Title, taskList.GetTask(1).Title);

            File.Delete(fileName);
        }

        // Other Tests
        [Test]
        public void OtherTest_TaskListInitialization()
        {
            ClassicAssert.IsNotNull(taskList);
            ClassicAssert.AreEqual(0, taskList.GetTaskCount());
        }

        [Test]
        public void OtherTest_InvalidTaskIndex()
        {
            ClassicAssert.Throws<IndexOutOfRangeException>(() => taskList.GetTask(-1));
            ClassicAssert.Throws<IndexOutOfRangeException>(() => taskList.GetTask(0));
        }
    }
}