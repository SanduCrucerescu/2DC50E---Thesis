import os
import shutil

# Set the root directory
root_dir = "/Users/sandu/Uni/Thesis/examples/C#-Translated/Classes"

# Loop through each folder (Claude and GPT)
for folder in ["Claude", "GPT4"]:
    folder_path = os.path.join(root_dir, folder)

    # Check if the folder exists
    if os.path.isdir(folder_path):
        # Loop through Program1-5 files
        for i in range(1, 6):
            file_name = f"Program{i}"
            file_path = os.path.join(folder_path, f"{file_name}.cs")

            # Check if the file exists
            if os.path.isfile(file_path):
                # Create the directory if it doesn't exist
                new_dir_path = os.path.join(folder_path, file_name)
                if not os.path.exists(new_dir_path):
                    os.makedirs(new_dir_path)

                # Move the file to the new directory
                new_file_path = os.path.join(new_dir_path, f"{file_name}.cs")
                shutil.move(file_path, new_file_path)
                print(f"Moved {file_path} to {new_file_path}")

                # Create the test file in the new directory
                test_file_path = os.path.join(new_dir_path, f"{file_name}Tests.cs")
                with open(test_file_path, "w") as test_file:
                    test_file.write(f"// Tests for {file_name}.cs")
                print(f"Created {test_file_path}")