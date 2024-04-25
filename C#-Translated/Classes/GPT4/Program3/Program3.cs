namespace Classes8; // manual declaration

using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Globalization;

public class TTask
{
    private string FTitle;
    private string FDescription;
    private DateTime FDueDate;
    private bool FCompleted;
    private int FPriority;

    public TTask(string Title, string Description, DateTime DueDate, int Priority)
    {
        FTitle = Title;
        FDescription = Description;
        FDueDate = DueDate;
        FCompleted = false;
        FPriority = Priority;
    }

    public void MarkAsCompleted()
    {
        FCompleted = true;
    }

    public string Title => FTitle;
    public string Description => FDescription;
    public DateTime DueDate => FDueDate;
    public bool Completed => FCompleted;
    public int Priority => FPriority;
}

public class TTaskList
{
    internal List<TTask> FTasks = new List<TTask>();

    public void AddTask(TTask task)
    {
        FTasks.Add(task);
    }

    public void RemoveTask(TTask task)
    {
        FTasks.Remove(task);
    }

    public int GetTaskCount()
    {
        return FTasks.Count;
    }

    public TTask GetTask(int index)
    {
        return FTasks[index];
    }

    public void SortTasksByPriority()
    {
        FTasks.Sort((x, y) => x.Priority.CompareTo(y.Priority));
    }

    public void SaveTasksToFile(string fileName)
    {
        using (StreamWriter writer = new StreamWriter(fileName))
        {
            foreach (TTask task in FTasks)
            {
                writer.WriteLine($"{task.Title},{task.Description},{task.DueDate.ToString("yyyy-MM-dd")},{Convert.ToInt32(task.Completed)},{task.Priority}");
            }
        }
    }

    public void LoadTasksFromFile(string fileName)
    {
        FTasks.Clear();
        using (StreamReader reader = new StreamReader(fileName))
        {
            string line;
            while ((line = reader.ReadLine()) != null)
            {
                string[] parts = line.Split(',');
                string title = parts[0];
                string description = parts[1];
                DateTime dueDate = DateTime.ParseExact(parts[2], "yyyy-MM-dd", CultureInfo.InvariantCulture);
                bool completed = Convert.ToBoolean(int.Parse(parts[3]));
                int priority = int.Parse(parts[4]);
                TTask task = new TTask(title, description, dueDate, priority);
                if (completed)
                    task.MarkAsCompleted();
                FTasks.Add(task);
            }
        }
    }
}

public class TaskManagerApp
{
    static void Main(string[] args)
    {
        TTaskList taskList = new TTaskList();
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
                    Console.Write("Enter task title: ");
                    string title = Console.ReadLine();
                    Console.Write("Enter task description: ");
                    string description = Console.ReadLine();
                    DateTime dueDate;
                    while (true)
                    {
                        Console.Write("Enter task due date (YYYY-MM-DD): ");
                        if (DateTime.TryParseExact(Console.ReadLine(), "yyyy-MM-dd", CultureInfo.InvariantCulture, DateTimeStyles.None, out dueDate))
                            break;
                        Console.WriteLine("Invalid date format. Please enter a valid date (YYYY-MM-DD).");
                    }
                    Console.Write("Enter task priority (1-5): ");
                    int priority = int.Parse(Console.ReadLine());
                    taskList.AddTask(new TTask(title, description, dueDate, priority));
                    Console.WriteLine("Task added successfully.");
                    break;
                case 2:
                    Console.Write("Enter the index of the task to remove: ");
                    int removeIndex = int.Parse(Console.ReadLine());
                    if (removeIndex >= 0 && removeIndex < taskList.GetTaskCount())
                    {
                        taskList.RemoveTask(taskList.GetTask(removeIndex));
                        Console.WriteLine("Task removed successfully.");
                    }
                    else
                    {
                        Console.WriteLine("Invalid task index.");
                    }
                    break;
                case 3:
                    Console.Write("Enter the index of the task to mark as completed: ");
                    int completeIndex = int.Parse(Console.ReadLine());
                    if (completeIndex >= 0 && completeIndex < taskList.GetTaskCount())
                    {
                        taskList.GetTask(completeIndex).MarkAsCompleted();
                        Console.WriteLine("Task marked as completed.");
                    }
                    else
                    {
                        Console.WriteLine("Invalid task index.");
                    }
                    break;
                case 4:
                    foreach (TTask task in taskList.FTasks)
                    {
                        Console.WriteLine($"Title: {task.Title}, Description: {task.Description}, Due Date: {task.DueDate.ToString("yyyy-MM-dd")}, Completed: {task.Completed}, Priority: {task.Priority}");
                    }
                    break;
                case 5:
                    taskList.SortTasksByPriority();
                    Console.WriteLine("Tasks sorted by priority.");
                    break;
                case 6:
                    taskList.SaveTasksToFile("tasks.txt");
                    Console.WriteLine("Tasks saved to file.");
                    break;
                case 0:
                    Console.WriteLine("Exiting...");
                    break;
                default:
                    Console.WriteLine("Invalid choice. Please enter a number between 0 and 6.");
                    break;
            }
        } while (choice != 0);
    }
}