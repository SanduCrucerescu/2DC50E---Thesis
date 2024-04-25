using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace TaskManagerApp5
{
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

        public string Title => FTitle;
        public string Description => FDescription;
        public DateTime DueDate => FDueDate;
        public bool Completed => FCompleted;
        public int Priority => FPriority;
    }

    class TTaskList
    {
        private List<TTask> FTasks;

        public TTaskList()
        {
            FTasks = new List<TTask>();
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

        public void SortTasksByPriority()
        {
            FTasks.Sort((left, right) => left.Priority - right.Priority);
        }

        public void SaveTasksToFile(string FileName)
        {
            using (StreamWriter writer = new StreamWriter(FileName))
            {
                foreach (TTask task in FTasks)
                {
                    writer.WriteLine($"{task.Title},{task.Description},{task.DueDate.ToString("yyyy-MM-dd")},{task.Completed},{task.Priority}");
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
                    string Title = parts[0];
                    string Description = parts[1];
                    DateTime DueDate = DateTime.ParseExact(parts[2], "yyyy-MM-dd", null);
                    bool Completed = bool.Parse(parts[3]);
                    int Priority = int.Parse(parts[4]);
                    TTask task = new TTask(Title, Description, DueDate, Priority);
                    if (Completed)
                        task.MarkAsCompleted();
                    FTasks.Add(task);
                }
            }
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            TTaskList TaskList = new TTaskList();

            // Load tasks from file (if exists)
            if (File.Exists("tasks.txt"))
                TaskList.LoadTasksFromFile("tasks.txt");

            int Choice;
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

                switch (Choice)
                {
                    case 1:
                        Console.Write("Enter task title: ");
                        string Title = Console.ReadLine();
                        Console.Write("Enter task description: ");
                        string Description = Console.ReadLine();

                        DateTime DueDate;
                        while (true)
                        {
                            Console.Write("Enter task due date (YYYY-MM-DD): ");
                            string DueDateStr = Console.ReadLine();

                            if (DateTime.TryParseExact(DueDateStr, "yyyy-MM-dd", null, System.Globalization.DateTimeStyles.None, out DueDate))
                                break;
                            else
                                Console.WriteLine("Invalid date format. Please enter a valid date (YYYY-MM-DD).");
                        }

                        Console.Write("Enter task priority (1-5): ");
                        int Priority = int.Parse(Console.ReadLine());
                        TaskList.AddTask(new TTask(Title, Description, DueDate, Priority));
                        Console.WriteLine("Task added successfully.");
                        break;
                    case 2:
                        Console.Write("Enter the index of the task to remove: ");
                        int RemoveIndex = int.Parse(Console.ReadLine());
                        if (RemoveIndex >= 0 && RemoveIndex < TaskList.GetTaskCount())
                        {
                            TaskList.RemoveTask(TaskList.GetTask(RemoveIndex));
                            Console.WriteLine("Task removed successfully.");
                        }
                        else
                            Console.WriteLine("Invalid task index.");
                        break;
                    case 3:
                        Console.Write("Enter the index of the task to mark as completed: ");
                        int CompletedIndex = int.Parse(Console.ReadLine());
                        if (CompletedIndex >= 0 && CompletedIndex < TaskList.GetTaskCount())
                        {
                            TaskList.GetTask(CompletedIndex).MarkAsCompleted();
                            Console.WriteLine("Task marked as completed.");
                        }
                        else
                            Console.WriteLine("Invalid task index.");
                        break;
                    case 4:
                        Console.WriteLine("Tasks:");
                        for (int i = 0; i < TaskList.GetTaskCount(); i++)
                        {
                            TTask task = TaskList.GetTask(i);
                            Console.WriteLine($"Index: {i}, Title: {task.Title}, Priority: {task.Priority}, Due Date: {task.DueDate.ToString("yyyy-MM-dd")}, Completed: {task.Completed}");
                        }
                        break;
                    case 5:
                        TaskList.SortTasksByPriority();
                        Console.WriteLine("Tasks sorted by priority.");
                        break;
                    case 6:
                        TaskList.SaveTasksToFile("tasks.txt");
                        Console.WriteLine("Tasks saved to file.");
                        break;
                    case 0:
                        Console.WriteLine("Exiting the program.");
                        break;
                    default:
                        Console.WriteLine("Invalid choice. Please try again.");
                        break;
                }

                Console.WriteLine();
            } while (Choice != 0);
        }
    }
}
