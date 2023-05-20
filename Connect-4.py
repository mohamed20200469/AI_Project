import tkinter as tk
import subprocess

class PrologGUI:
    def __init__(self, master):
        self.master = master
        self.master.title("Prolog Game")
        self.master.resizable(False, False)
        self.difficulty_value = tk.IntVar()
        self.strategy_value = tk.IntVar()
        self.create_widgets()

    def create_widgets(self):
        # Create the title label
        tk.Label(self.master, text="Prolog Game", font=("Arial", 16, "bold")).grid(row=0, column=0, columnspan=2, pady=10)

        # Create the difficulty buttons
        tk.Label(self.master, text="Select difficulty:", font=("Arial", 12)).grid(row=1, column=0, sticky="w", padx=20)
        tk.Radiobutton(self.master, text="Easy", variable=self.difficulty_value, value=1, font=("Arial", 12)).grid(row=2, column=0, sticky="w", padx=50, pady=5)
        tk.Radiobutton(self.master, text="Normal", variable=self.difficulty_value, value=3, font=("Arial", 12)).grid(row=3, column=0, sticky="w", padx=50, pady=5)
        tk.Radiobutton(self.master, text="Hard", variable=self.difficulty_value, value=5, font=("Arial", 12)).grid(row=4, column=0, sticky="w", padx=50, pady=5)

        # Create the strategy buttons
        tk.Label(self.master, text="Select strategy:", font=("Arial", 12)).grid(row=1, column=1, sticky="w", padx=20)
        tk.Radiobutton(self.master, text="Minmax", variable=self.strategy_value, value=1, font=("Arial", 12)).grid(row=2, column=1, sticky="w", padx=50, pady=5)
        tk.Radiobutton(self.master, text="Alpha-beta", variable=self.strategy_value, value=2, font=("Arial", 12)).grid(row=3, column=1, sticky="w", padx=50, pady=5)

        # Create the start button
        tk.Button(self.master, text="Start", font=("Arial", 12), bg="#4CAF50", fg="white", activebackground="#8BC34A", activeforeground="white", command=self.start_prolog).grid(row=5, column=0, columnspan=2, pady=20)

    def start_prolog(self):
        # Get the values of the difficulty and strategy variables
        difficulty = self.difficulty_value.get()
        strategy = self.strategy_value.get()

        # Set the difficulty and strategy variables in the Prologcode using the subprocess module
        prolog_file_path = "C:/Users/Hp/Desktop/AI_Project/Game.pl"
        subprocess.Popen(['swipl', '-q', '-t', f'assert(difficulty({difficulty})), assert(strategy({strategy})), consult(\'{prolog_file_path}\'), startGame.'])

# Create the main window and run the GUI
root = tk.Tk()
app = PrologGUI(root)
root.mainloop()