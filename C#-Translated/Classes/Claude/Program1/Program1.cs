namespace Classes1; //Manual declaration
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

class Program
{
    static void Main(string[] args)
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
            int choice = int.Parse(Console.ReadLine());

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
                        string dueDateStr = Console.ReadLine();

                        if (DateTime.TryParseExact(dueDateStr, "yyyy-MM-dd", null, System.Globalization.DateTimeStyles.None, out dueDate))
                            break;
                        else
                            Console.WriteLine("Invalid date format. Please enter a valid date (YYYY-MM-DD).");
                    }

                    Console.Write("Enter task priority (1-5): ");
                    int priority = int.Parse(Console.ReadLine());
                    taskList.AddTask(new Task(title, description, dueDate, priority));
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
                        Console.WriteLine("Invalid task index.");
                    break;
                case 3:
                    Console.Write("Enter the index of the task to mark as completed: ");
                    int completedIndex = int.Parse(Console.ReadLine());
                    if (completedIndex >= 0 && completedIndex < taskList.GetTaskCount())
                    {
                        taskList.GetTask(completedIndex).MarkAsCompleted();
                        Console.WriteLine("Task marked as completed.");
                    }
                    else
                        Console.WriteLine("Invalid task index.");
                    break;
                case 4:
                    Console.WriteLine("Tasks:");
                    for (int i = 0; i < taskList.GetTaskCount(); i++)
                    {
                        var task = taskList.GetTask(i);
                        Console.WriteLine($"Index: {i}, Title: {task.Title}, Priority: {task.Priority}, Due Date: {task.DueDate.ToShortDateString()}, Completed: {task.Completed}");
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
        tasks = tasks.OrderBy(t => t.Priority).ToList();
    }

    public void SaveTasksToFile(string fileName)
    {
        using (var writer = new StreamWriter(fileName))
        {
            foreach (var task in tasks)
            {
                writer.WriteLine($"{task.Title},{task.Description},{task.DueDate.ToString("yyyy-MM-dd")},{task.Completed},{task.Priority}");
            }
        }
    }

    public void LoadTasksFromFile(string fileName)
    {
        tasks.Clear();
        using (var reader = new StreamReader(fileName))
        {
            string line;
            while ((line = reader.ReadLine()) != null)
            {
                var parts = line.Split(',');
                var title = parts[0];
                var description = parts[1];
                var dueDate = DateTime.ParseExact(parts[2], "yyyy-MM-dd", null);
                var completed = bool.Parse(parts[3]);
                var priority = int.Parse(parts[4]);
                var task = new Task(title, description, dueDate, priority);
                if (completed)
                    task.MarkAsCompleted();
                tasks.Add(task);
            }
        }
    }
}