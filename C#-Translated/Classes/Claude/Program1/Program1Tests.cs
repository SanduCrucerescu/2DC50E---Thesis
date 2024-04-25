using System;
using System.IO;
using NUnit.Framework;
using NUnit.Framework.Legacy;

namespace Classes1.Test
{

    [TestFixture]
    public class TaskManagerTests
    {
        private TaskList taskList;

        [SetUp]
        public void Setup()
        {
            taskList = new TaskList();
        }

        // Smoke Tests
        [Test]
        public void SmokeTest_CreateTask()
        {
            string title = "Test Task";
            string description = "This is a test task";
            DateTime dueDate = DateTime.Now.AddDays(7);
            int priority = 1;

            Task task = new Task(title, description, dueDate, priority);

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
            Task task = new Task("Test Task", "This is a test task", DateTime.Now.AddDays(7), 1);

            taskList.AddTask(task);

            ClassicAssert.AreEqual(1, taskList.GetTaskCount());
        }

        // Unit Tests
        [Test]
        public void UnitTest_MarkTaskAsCompleted()
        {
            Task task = new Task("Test Task", "This is a test task", DateTime.Now.AddDays(7), 1);

            task.MarkAsCompleted();

            ClassicAssert.IsTrue(task.Completed);
        }

        [Test]
        public void UnitTest_RemoveTask()
        {
            Task task = new Task("Test Task", "This is a test task", DateTime.Now.AddDays(7), 1);
            taskList.AddTask(task);

            taskList.RemoveTask(task);

            ClassicAssert.AreEqual(0, taskList.GetTaskCount());
        }

        [Test]
        public void UnitTest_GetTask()
        {
            Task task1 = new Task("Task 1", "This is task 1", DateTime.Now.AddDays(7), 1);
            Task task2 = new Task("Task 2", "This is task 2", DateTime.Now.AddDays(14), 2);
            taskList.AddTask(task1);
            taskList.AddTask(task2);

            Task retrievedTask = taskList.GetTask(1);

            ClassicAssert.AreEqual(task2, retrievedTask);
        }

        // Functional Tests
        [Test]
        public void FunctionalTest_SortTasksByPriority()
        {
            Task task1 = new Task("Task 1", "This is task 1", DateTime.Now.AddDays(7), 2);
            Task task2 = new Task("Task 2", "This is task 2", DateTime.Now.AddDays(14), 1);
            Task task3 = new Task("Task 3", "This is task 3", DateTime.Now.AddDays(21), 3);
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
            Task task1 = new Task("Task 1", "This is task 1", DateTime.Now.AddDays(7), 1);
            Task task2 = new Task("Task 2", "This is task 2", DateTime.Now.AddDays(14), 2);
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