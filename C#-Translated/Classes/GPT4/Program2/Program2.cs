namespace Classes7; // manual declaration
using System;
using System.Collections.Generic;
using System.IO;
using System.Globalization;

class TTask
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

    public string Title { get { return FTitle; } }
    public string Description { get { return FDescription; } }
    public DateTime DueDate { get { return FDueDate; } }
    public bool Completed { get { return FCompleted; } }
    public int Priority { get { return FPriority; } }
}

class TTaskList
{
    private List<TTask> FTasks = new List<TTask>();

    public TTaskList()
    {
    }

    ~TTaskList()
    {
        FTasks.Clear();
    }

    public void AddTask(TTask Task)
    {
        FTasks.Add(Task);
    }

    public void RemoveTask(TTask Task)
    {
        FTasks.Remove(Task);
    }

    public int GetTaskCount()
    {
        return FTasks.Count;
    }

    public TTask GetTask(int Index)
    {
        return FTasks[Index];
    }

    private int CompareTasksByPriority(TTask Left, TTask Right)
    {
        return Left.Priority.CompareTo(Right.Priority);
    }

    public void SortTasksByPriority()
    {
        FTasks.Sort(CompareTasksByPriority);
    }

    public void SaveTasksToFile(string FileName)
    {
        using (StreamWriter writer = new StreamWriter(FileName))
        {
            foreach (TTask task in FTasks)
            {
                writer.WriteLine($"{task.Title},{task.Description},{task.DueDate.ToString("yyyy-MM-dd")},{Convert.ToInt32(task.Completed)},{task.Priority}");
            }
        }
    }

    public void LoadTasksFromFile(string FileName)
    {
        FTasks.Clear();
        using (StreamReader reader = new StreamReader(FileName))
        {
            string line;
            while ((line = reader.ReadLine()) != null)
            {
                string[] parts = line.Split(',');
                string title = parts[0];
                string description = parts[1];
                DateTime dueDate = DateTime.ParseExact(parts[2], "yyyy-MM-dd", CultureInfo.InvariantCulture);
                bool completed = Convert.ToBoolean(Convert.ToInt32(parts[3]));
                int priority = int.Parse(parts[4]);
                TTask newTask = new TTask(title, description, dueDate, priority);
                AddTask(newTask);
                if (completed)
                {
                    newTask.MarkAsCompleted();
                }
            }
        }
    }
}

class Program
{
    static void Main()
    {
        TTaskList TaskList = new TTaskList();
        int Choice = -1;
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
            Choice = int.Parse(Console.ReadLine());

            string Title, Description, DueDateStr;
            DateTime DueDate;
            int Priority, TaskIndex;

            switch (Choice)
            {
                case 1:
                    Console.Write("Enter task title: ");
                    Title = Console.ReadLine();
                    Console.Write("Enter task description: ");
                    Description = Console.ReadLine();

                    while (true)
                    {
                        Console.Write("Enter task due date (YYYY-MM-DD): ");
                        DueDateStr = Console.ReadLine();

                        if (DateTime.TryParseExact(DueDateStr, "yyyy-MM-dd", CultureInfo.InvariantCulture, DateTimeStyles.None, out DueDate))
                        {
                            break;
                        }
                        else
                        {
                            Console.WriteLine("Invalid date format. Please enter a valid date (YYYY-MM-DD).");
                        }
                    }

                    Console.Write("Enter task priority (1-5): ");
                    Priority = int.Parse(Console.ReadLine());
                    TaskList.AddTask(new TTask(Title, Description, DueDate, Priority));
                    Console.WriteLine("Task added successfully.");
                    break;
                case 2:
                    Console.Write("Enter the index of the task to remove: ");
                    TaskIndex = int.Parse(Console.ReadLine());
                    if (TaskIndex >= 0 && TaskIndex < TaskList.GetTaskCount())
                    {
                        TaskList.RemoveTask(TaskList.GetTask(TaskIndex));
                        Console.WriteLine("Task removed successfully.");
                    }
                    else
                    {
                        Console.WriteLine("Invalid task index.");
                    }
                    break;
                case 3:
                    Console.Write("Enterthe index of the task to mark as completed: ");
                    TaskIndex = int.Parse(Console.ReadLine());
                    if (TaskIndex >= 0 && TaskIndex < TaskList.GetTaskCount())
                    {
                        TaskList.GetTask(TaskIndex).MarkAsCompleted();
                        Console.WriteLine("Task marked as completed.");
                    }
                    else
                    {
                        Console.WriteLine("Invalid task index.");
                    }
                    break;
                case 4:
                    Console.WriteLine("Displaying all tasks:");
                    for (int i = 0; i < TaskList.GetTaskCount(); i++)
                    {
                        TTask task = TaskList.GetTask(i);
                        Console.WriteLine($"{i}: Title={task.Title}, Description={task.Description}, Due Date={task.DueDate.ToString("yyyy-MM-dd")}, Completed={task.Completed}, Priority={task.Priority}");
                    }
                    break;
                case 5:
                    TaskList.SortTasksByPriority();
                    Console.WriteLine("Tasks sorted by priority.");
                    break;
                case 6:
                    Console.Write("Enter filename to save tasks: ");
                    string FileName = Console.ReadLine();
                    TaskList.SaveTasksToFile(FileName);
                    Console.WriteLine("Tasks saved successfully.");
                    break;
                case 0:
                    Console.WriteLine("Exiting...");
                    break;
                default:
                    Console.WriteLine("Invalid choice. Please enter a valid number between 0 and 6.");
                    break;
            }
        } while (Choice != 0);
    }
}