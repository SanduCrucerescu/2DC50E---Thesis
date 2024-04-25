namespace Classes10; // manual declaration

using System;
using System.Collections.Generic;
using System.IO;
using System.Globalization;

class Program
{
    static void Main(string[] args)
    {
        TaskList taskList = new TaskList();
        try
        {
            // Load tasks from file (if exists)
            if (File.Exists("tasks.txt"))
                taskList.LoadTasksFromFile("tasks.txt");

            int choice;
            do
            {
                Console.WriteLine("Task Manager");
                Console.WriteLine("1. Add Task");
                Console.WriteLine("2. Remove Task");
                Console.WriteLine("3. Mark Task as Completed");
                Console.WriteLine("4. Display Tasks");
                Console.WriteLine("5. Sort Tasks by Priority");
                Console.WriteLine("6. Save Tasks");
                Console.WriteLine("0. Exit");
                Console.Write("Enter your choice: ");
                choice = int.Parse(Console.ReadLine());

                switch (choice)
                {
                    case 1:
                        {
                            Console.Write("Enter task title: ");
                            string title = Console.ReadLine();
                            Console.Write("Enter task description: ");
                            string description = Console.ReadLine();

                            DateTime dueDate;
                            while (true)
                            {
                                Console.Write("Enter task due date (YYYY-MM-DD): ");
                                string dueDateStr = Console.ReadLine();
                                try
                                {
                                    dueDate = DateTime.ParseExact(dueDateStr, "yyyy-MM-dd", CultureInfo.InvariantCulture);
                                    break;
                                }
                                catch (FormatException)
                                {
                                    Console.WriteLine("Invalid date format. Please enter a valid date (YYYY-MM-DD).");
                                }
                            }

                            Console.Write("Enter task priority (1-5): ");
                            int priority = int.Parse(Console.ReadLine());
                            taskList.AddTask(new Task(title, description, dueDate, priority));
                            Console.WriteLine("Task added successfully.");
                        }
                        break;
                    case 2:
                        {
                            Console.Write("Enter the index of the task to remove: ");
                            int taskIndex = int.Parse(Console.ReadLine());
                            if (taskIndex >= 0 && taskIndex < taskList.GetTaskCount())
                            {
                                taskList.RemoveTask(taskList.GetTask(taskIndex));
                                Console.WriteLine("Task removed successfully.");
                            }
                            else
                                Console.WriteLine("Invalid task index.");
                        }
                        break;
                    case 3:
                        {
                            Console.Write("Enter the index of the task to mark as completed: ");
                            int taskIndex = int.Parse(Console.ReadLine());
                            if (taskIndex >= 0 && taskIndex < taskList.GetTaskCount())
                            {
                                taskList.GetTask(taskIndex).MarkAsCompleted();
                                Console.WriteLine("Task marked as completed.");
                            }
                            else
                                Console.WriteLine("Invalid task index.");
                        }
                        break;
                    case 4:
                        {
                            Console.WriteLine("Tasks:");
                            for (int i = 0; i < taskList.GetTaskCount(); i++)
                            {
                                Task task = taskList.GetTask(i);
                                Console.WriteLine($"Index: {i}, Title: {task.Title}, Priority: {task.Priority}, Due Date: {task.DueDate.ToShortDateString()}, Completed: {task.Completed}");
                            }
                        }
                        break;
                    case 5:
                        {
                            taskList.SortTasksByPriority();
                            Console.WriteLine("Tasks sorted by priority.");
                        }
                        break;
                    case 6:
                        {
                            taskList.SaveTasksToFile("tasks.txt");
                            Console.WriteLine("Tasks saved to file.");
                        }
                        break;
                    case 0:
                        Console.WriteLine("Exiting the program.");
                        break;
                    default:
                        Console.WriteLine("Invalid choice. Please try again.");
                        break;
                }

                Console.WriteLine();
            } while (choice != 0);
        }
        finally
        {
            taskList.Dispose();
        }
    }
}

public class Task
{
    public string Title { get; private set; }
    public string Description { get; private set; }
    public DateTime DueDate { get; private set; }
    public bool Completed { get; private set; }
    public int Priority { get; private set; }

    public Task(string title, string description, DateTime dueDate, int priority)
    {
        Title = title;
        Description = description;
        DueDate = dueDate;
        Completed = false;
        Priority = priority;
    }

    public void MarkAsCompleted()
    {
        Completed = true;
    }
}

public class TaskList : IDisposable
{
    private List<Task> tasks = new List<Task>();

    public void AddTask(Task task)
    {
        tasks.Add(task);
    }

    public void RemoveTask(Task task)
    {
        tasks.Remove(task);
    }

    public int GetTaskCount()
    {
        return tasks.Count;
    }

    public Task GetTask(int index)
    {
        return tasks[index];
    }

    public void SortTasksByPriority()
    {
        tasks.Sort((left, right) => left.Priority.CompareTo(right.Priority));
    }

    public void SaveTasksToFile(string fileName)
    {
        using (StreamWriter file = new StreamWriter(fileName))
        {
            foreach (Task task in tasks)
            {
                file.WriteLine($"{task.Title},{task.Description},{task.DueDate.ToString("yyyy-MM-dd")},{(task.Completed ? 1 : 0)},{task.Priority}");
            }
        }
    }

    public void LoadTasksFromFile(string fileName)
    {
        tasks.Clear();
        using (StreamReader file = new StreamReader(fileName))
        {
            string line;
            while ((line = file.ReadLine()) != null)
            {
                string[] parts = line.Split(',');
                string title = parts[0];
                string description = parts[1];
                DateTime dueDate = DateTime.ParseExact(parts[2], "yyyy-MM-dd", CultureInfo.InvariantCulture);
                bool completed = parts[3] == "1";
                int priority = int.Parse(parts[4]);

                Task newTask = new Task(title, description, dueDate, priority);
                AddTask(newTask);
                if (completed)
                {
                    newTask.MarkAsCompleted();
                }
            }
        }
    }

    public void Dispose()
    {
        // Implement if needed
    }
}