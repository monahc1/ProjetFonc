import tkinter as tk
from tkinter import ttk
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg
import matplotlib.pyplot as plt
import csv
from tkinter import filedialog, messagebox

performance_data = {
    "Graph": {
        "color": "#8B0000",
        "elements": [1000, 2000, 5000],
        "cpp_time": [0, 0, 1],
        "haskell_time": [0.0038935, 0.0087604, 0.0263161],
        "cpp_memory": [144000, 288000, 720000],
        "haskell_memory": [140000, 280000, 700000],
    },
    "Binary Search Tree": {
        "color": "#8B0000",
        "elements": [10000, 20000, 50000],
        "cpp_time": [1, 3, 9],
        "haskell_time": [0.0496712, 0.0545398, 0.0720584],
        "cpp_memory": [207072, 567456, 1183392],
        "haskell_memory": [240000, 480000, 1200000],
    },
    "Queue": {
        "color": "#8B0000",
        "elements": [10000, 20000, 50000],
        "cpp_time": [3, 7, 20],
        "haskell_time": [0.0516178, 0.0525915, 0.0632966],
        "cpp_memory": [200000, 400000, 1000000],
        "haskell_memory": [91226112, 93323264, 100663296],
    },
    "Stack": {
        "color": "#8B0000",
        "elements": [100000, 200000, 500000],
        "cpp_time": [125, 250, 650],
        "haskell_time": [0.15, 0.3, 12.0],
        "cpp_memory": [2400000, 4800000, 12000000],
        "haskell_memory": [2400000, 4800000, 12000000],
    },
    "List": {
        "color": "#8B0000",
        "elements": [10000, 20000, 50000],
        "cpp_time": [3, 8, 19],
        "haskell_time": [0.0338931, 0.0378727, 0.0543314],
        "cpp_memory": [584, 1120, 3016],
        "haskell_memory": [63963136, 74448896, 92274688],
    },
    "LinkedList": {
        "color": "#8B0000",
        "elements": [10000, 20000, 50000],
        "cpp_time": [70, 160, 400],
        "haskell_time": [0.0329523, 0.0368809, 0.0473629],
        "cpp_memory": [328, 640, 1500],
        "haskell_memory": [66060288, 77594624, 100663296],
    },
}

def update_graph(selected_ds, axes, fig):
    data = performance_data[selected_ds]
    color = data["color"]

    for ax in axes:
        ax.clear()

    axes[0].plot(data["elements"], data["cpp_time"], 'o-', color=color, label="C++ Time (s)")
    axes[0].plot(data["elements"], data["haskell_time"], 'o--', color="#4B0082", label="Haskell Time (s)")
    axes[0].set_title(f"{selected_ds}: Execution Time Comparison", fontsize=16, weight="bold", color="#483D8B")
    axes[0].set_xlabel("Number of Elements", fontsize=12, color="#483D8B")
    axes[0].set_ylabel("Time (s)", fontsize=12, color="#483D8B")
    axes[0].grid(color="gray", linestyle="--", linewidth=0.5)
    axes[0].tick_params(colors="#483D8B")
    axes[0].legend(fontsize=10, facecolor="#F5F5F5", edgecolor="white", labelcolor="#483D8B")

    axes[1].plot(data["elements"], data["cpp_memory"], 'o-', color=color, label="C++ Memory (bytes)")
    axes[1].plot(data["elements"], data["haskell_memory"], 'o--', color="#4B0082", label="Haskell Memory (bytes)")
    axes[1].set_title(f"{selected_ds}: Memory Usage Comparison", fontsize=16, weight="bold", color="#483D8B")
    axes[1].set_xlabel("Number of Elements", fontsize=12, color="#483D8B")
    axes[1].set_ylabel("Memory (bytes)", fontsize=12, color="#483D8B")
    axes[1].grid(color="gray", linestyle="--", linewidth=0.5)
    axes[1].tick_params(colors="#483D8B")
    axes[1].legend(fontsize=10, facecolor="#F5F5F5", edgecolor="white", labelcolor="#483D8B")

    fig.patch.set_facecolor('#E6E6FA')

def export_to_csv():
    file_path = filedialog.asksaveasfilename(defaultextension=".csv", filetypes=[("CSV files", "*.csv")])
    if file_path:
        with open(file_path, mode='w', newline='') as file:
            writer = csv.writer(file)
            writer.writerow(["Data Structure", "Number of Elements", "C++ Time (s)", "Haskell Time (s)", "C++ Memory (bytes)", "Haskell Memory (bytes)"])
            for ds_name, data in performance_data.items():
                for i in range(len(data["elements"])):
                    writer.writerow([
                        ds_name,
                        data["elements"][i],
                        data["cpp_time"][i],
                        data["haskell_time"][i],
                        data["cpp_memory"][i],
                        data["haskell_memory"][i]
                    ])
        messagebox.showinfo("Export Successful", "Data successfully exported to CSV.")

def go_to_analysis():
    for widget in root.winfo_children():
        widget.destroy()
    build_analysis_page()

def build_analysis_page():
    def return_to_navigation():
        for widget in root.winfo_children():
            widget.destroy()
        build_navigation_page()
        
    root.configure(bg="#E6E6FA")
    title_label = tk.Label(root, text="Functional Data Structures: C++ vs Haskell", font=("Arial", 26, "bold"), bg="#E6E6FA", fg="#483D8B")
    title_label.pack(pady=20)

    team_label = tk.Label(root, text="Team Members: Hussein Dakroub, Malih Assaad, Mona El Hajj Chehade", font=("Arial", 16), bg="#E6E6FA", fg="#BA55D3")
    team_label.pack(pady=10)
    professor_label = tk.Label(root, text="Instructor: Maroun Ayli", font=("Arial", 16), bg="#E6E6FA", fg="#BA55D3")
    professor_label.pack(pady=5)

    separator = ttk.Separator(root, orient='horizontal')
    separator.pack(fill='x', padx=20, pady=5)

    notebook = ttk.Notebook(root, style="TNotebook")
    notebook.pack(fill=tk.BOTH, expand=True, padx=10, pady=10)

    for ds_name in performance_data.keys():
        tab = ttk.Frame(notebook, style="TFrame")
        notebook.add(tab, text=ds_name)

        fig, axes = plt.subplots(2, 1, figsize=(10, 8), facecolor='#E6E6FA')
        fig.subplots_adjust(hspace=0.5)
        canvas = FigureCanvasTkAgg(fig, master=tab)
        canvas.get_tk_widget().pack(fill=tk.BOTH, expand=True, pady=10)

        update_graph(ds_name, axes, fig)

        button_frame = tk.Frame(tab, bg="#E6E6FA")
        button_frame.pack(fill=tk.X, pady=10)

        reset_button = ttk.Button(button_frame, text="Reset Graphs", command=lambda axes=axes, fig=fig, ds_name=ds_name: update_graph(ds_name, axes, fig), style="W.TButton")
        reset_button.pack(side=tk.LEFT, padx=10)

        return_button_tab = ttk.Button(button_frame, text="Return to Navigation", command=return_to_navigation, style="W.TButton")
        return_button_tab.pack(side=tk.RIGHT, padx=10)

    export_button_frame = tk.Frame(root, bg="#E6E6FA")
    export_button_frame.pack(fill=tk.X, padx=20, pady=20, side=tk.BOTTOM)

    return_button = ttk.Button(export_button_frame, text="Return to Navigation", command=return_to_navigation, style="W.TButton")
    return_button.pack(side=tk.LEFT, padx=20, pady=10)

    export_button = ttk.Button(export_button_frame, text="Export Data to CSV", command=export_to_csv, style="W.TButton")
    export_button.pack(side=tk.RIGHT, padx=20, pady=10)

def build_navigation_page():
    root.configure(bg="#E6E6FA")
    title_label = tk.Label(root, text="Welcome to Functional Data Structures Analysis", font=("Arial", 28, "bold"), bg="#E6E6FA", fg="#483D8B")
    title_label.pack(pady=30)

    team_label = tk.Label(root, text="Team Members: Hussein Dakroub, Malih Assaad, Mona El Hajj Chehade", font=("Arial", 16), bg="#E6E6FA", fg="#BA55D3")
    team_label.pack(pady=10)
    professor_label = tk.Label(root, text="Instructor: Maroun Ayli", font=("Arial", 16), bg="#E6E6FA", fg="#BA55D3")
    professor_label.pack(pady=5)

    separator = ttk.Separator(root, orient='horizontal')
    separator.pack(fill='x', padx=20, pady=5)

    subtitle_label = tk.Label(root, text="Explore the comparisons of data structures in C++ and Haskell.", font=("Arial", 18), bg="#E6E6FA", fg="#483D8B")
    subtitle_label.pack(pady=10)

    explore_button = ttk.Button(root, text="Go to Analysis", command=go_to_analysis, style="W.TButton")
    explore_button.pack(pady=30)

    complexity_button = ttk.Button(root, text="Complexity Analysis", command=show_complexity_analysis, style="W.TButton")
    complexity_button.pack(pady=10)

def show_complexity_analysis():
    complexity_window = tk.Toplevel(root)
    complexity_window.title("Complexity Analysis")
    complexity_window.geometry("700x500")
    complexity_window.configure(bg="#E6E6FA")

    title_label = tk.Label(complexity_window, text="Complexity Analysis of Data Structures", font=("Arial", 20, "bold"), bg="#E6E6FA", fg="#483D8B")
    title_label.pack(pady=20)

    info_label = tk.Label(complexity_window, text="Time and Space Complexity Summary", font=("Arial", 14), bg="#E6E6FA", fg="#483D8B")
    info_label.pack(pady=10)

    table_frame = tk.Frame(complexity_window, bg="#E6E6FA")
    table_frame.pack(pady=10, padx=20, fill="both", expand=True)

    headings = ["Data Structure", "C++ Time Complexity", "C++ Space Complexity", "Haskell Time Complexity", "Haskell Space Complexity"]
    for col, heading in enumerate(headings):
        header = tk.Label(table_frame, text=heading, font=("Arial", 12, "bold"), bg="#BA55D3", fg="white", borderwidth=1, relief="solid")
        header.grid(row=0, column=col, sticky="nsew", padx=1, pady=1)

    complexities = [
        ["Graph", "O(1)", "O(V + E)", "O(1)", "O(V + E)"],
        ["Binary Search Tree", "O(log n)", "O(n)", "O(log n)", "O(n)"],
        ["Queue", "O(1)", "O(n)", "O(1)", "O(n)"],
        ["Stack", "O(1)", "O(n)", "O(1)", "O(n)"],
        ["List", "O(n)", "O(n)", "O(n)", "O(n)"],
        ["LinkedList", "O(n)", "O(n)", "O(n)", "O(n)"]
    ]

    for row, complexity in enumerate(complexities, start=1):
        for col, value in enumerate(complexity):
            cell = tk.Label(table_frame, text=value, font=("Arial", 12), bg="#E6E6FA", fg="#483D8B", borderwidth=1, relief="solid")
            cell.grid(row=row, column=col, sticky="nsew", padx=1, pady=1)

    for col in range(len(headings)):
        table_frame.grid_columnconfigure(col, weight=1)

    return_button = ttk.Button(complexity_window, text="Return", command=complexity_window.destroy, style="W.TButton")
    return_button.pack(pady=20)

root = tk.Tk()
root.title("Functional Data Structures Analysis")
root.geometry("1500x1000")

style = ttk.Style()
style.configure("W.TButton", font=("Arial", 14, "bold"), foreground="black", background="#B0C4DE")
style.configure("TNotebook", tabposition='n')
style.configure("TFrame", background="#E6E6FA")

build_navigation_page()
root.mainloop()