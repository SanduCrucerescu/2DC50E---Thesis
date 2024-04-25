import subprocess

# Run the dotnet test command
result = subprocess.run(["dotnet", "test"], stdout=subprocess.PIPE, stderr=subprocess.PIPE, universal_newlines=True)

# Get the output and error messages
output = result.stdout
error = result.stderr

# Check the return code to see if the command was successful
if result.returncode == 0:
    print("Tests passed!")
    print(f"Output:\n{output}")
else:
    print("Tests failed!")
    print(f"Error:\n{error}")