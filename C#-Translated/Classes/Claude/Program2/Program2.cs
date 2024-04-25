namespace Classes2; //Manual declaration

using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        var taskList = new TaskList();

        // Load tasks from file (if exists)
        if (File.Exists("tasks.txt"))
            taskList.LoadTasksFromFile("tasks.txt");

        while (true)
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

            if (!int.TryParse(Console.ReadLine(), out var choice))
            {
                Console.WriteLine("Invalid choice. Please try again.");
                Console.WriteLine();
                continue;
            }

            switch (choice)
            {
                case 1:
                    Console.Write("Enter task title: ");
                    var title = Console.ReadLine();
                    Console.Write("Enter task description: ");
                    var description = Console.ReadLine();

                    DateTime dueDate;
                    while (true)
                    {
                        Console.Write("Enter task due date (YYYY-MM-DD): ");
                        var dueDateStr = Console.ReadLine();

                        if (DateTime.TryParseExact(dueDateStr, "yyyy-MM-dd", null, System.Globalization.DateTimeStyles.None, out dueDate))
                            break;

                        Console.WriteLine("Invalid date format. Please enter a valid date (YYYY-MM-DD).");
                    }

                    Console.Write("Enter task priority (1-5): ");
                    if (!int.TryParse(Console.ReadLine(), out var priority))
                    {
                        Console.WriteLine("Invalid priority value. Task not added.");
                        break;
                    }

                    taskList.AddTask(new Task(title, description, dueDate, priority));
                    Console.WriteLine("Task added successfully.");
                    break;
                case 2:
                    Console.Write("Enter the index of the task to remove: ");
                    if (!int.TryParse(Console.ReadLine(), out var removeIndex) || removeIndex < 0 || removeIndex >= taskList.GetTaskCount())
                    {
                        Console.WriteLine("Invalid task index.");
                        break;
                    }

                    taskList.RemoveTask(taskList.GetTask(removeIndex));
                    Console.WriteLine("Task removed successfully.");
                    break;
                case 3:
                    Console.Write("Enter the index of the task to mark as completed: ");
                    if (!int.TryParse(Console.ReadLine(), out var completeIndex) || completeIndex < 0 || completeIndex >= taskList.GetTaskCount())
                    {
                        Console.WriteLine("Invalid task index.");
                        break;
                    }

                    taskList.GetTask(completeIndex).MarkAsCompleted();
                    Console.WriteLine("Task marked as completed.");
                    break;
                case 4:
                    Console.WriteLine("Tasks:");
                    for (var i = 0; i < taskList.GetTaskCount(); i++)
                    {
                        var task = taskList.GetTask(i);
                        Console.WriteLine(
                            $"Index: {i}, Title: {task.Title}, Priority: {task.Priority}, Due Date: {task.DueDate:yyyy-MM-dd}, Completed: {task.Completed}");
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
                    Console.WriteLine("Exiting the program.");
                    return;
                default:
                    Console.WriteLine("Invalid choice. Please try again.");
                    break;
            }

            Console.WriteLine();
        }
    }
}

class Task
{
    public string Title { get; }
    public string Description { get; }
    public DateTime DueDate { get; }
    public bool Completed { get; private set; }
    public int Priority { get; }

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

class TaskList
{
    private readonly List<Task> _tasks = new List<Task>();

    public void AddTask(Task task)
    {
        _tasks.Add(task);
    }

    public void RemoveTask(Task task)
    {
        _tasks.Remove(task);
    }

    public int GetTaskCount()
    {
        return _tasks.Count;
    }

    public Task GetTask(int index)
    {
        return _tasks[index];
    }

    public void SortTasksByPriority()
    {
        _tasks.Sort((left, right) => left.Priority.CompareTo(right.Priority));
    }

    public void SaveTasksToFile(string fileName)
    {
        using var writer = new StreamWriter(fileName);
        foreach (var task in _tasks)
        {
            writer.WriteLine($"{task.Title},{task.Description},{task.DueDate:yyyy-MM-dd},{task.Completed},{task.Priority}");
        }
    }

    public void LoadTasksFromFile(string fileName)
    {
        _tasks.Clear();
        using var reader = new StreamReader(fileName);
        while (!reader.EndOfStream)
        {
            var line = reader.ReadLine();
            var parts = line.Split(',');
            var title = parts[0];
            var description = parts[1];
            var dueDate = DateTime.ParseExact(parts[2], "yyyy-MM-dd", null);
            var completed = bool.Parse(parts[3]);
            var priority = int.Parse(parts[4]);
            var task = new Task(title, description, dueDate, priority);
            if (completed)
                task.MarkAsCompleted();
            AddTask(task);
        }
    }
}