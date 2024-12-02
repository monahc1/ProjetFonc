import tkinter as tk
from tkinter import ttk
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg
import matplotlib.pyplot as plt
import csv
from tkinter import filedialog, messagebox

performance_data = {
    "Graph": {
        "color": "blue",
        "elements": [1000, 2000, 5000],
        "cpp_time": [0, 1, 3],
        "haskell_time": [0.0079463, 0.0160045, 0.0628369],
        "cpp_memory": [144000, 288000, 720000],
        "haskell_memory": [140000, 280000, 700000],
    },
    "Binary Search Tree": {
        "color": "green",
        "elements": [100000, 200000, 500000],
        "cpp_time": [26, 67, 147],
        "haskell_time": [0.158111, 0.224362, 0.645415],
        "cpp_memory": [748704, 1533456, 2319888],
        "haskell_memory": [2400000, 4800000, 12000000],
    },
    "Queue": {
        "color": "cyan",
        "elements": [100000, 200000, 500000],
        "cpp_time": [150, 300, 850],
        "haskell_time": [0.18, 0.36, 0.92],
        "cpp_memory": [3200000, 6400000, 16000000],
        "haskell_memory": [3200000, 6400000, 16000000],
    },
    "Stack": {
        "color": "red",
        "elements": [100000, 200000, 500000],
        "cpp_time": [125, 250, 650],
        "haskell_time": [0.15, 0.3, 12.0],
        "cpp_memory": [2400000, 4800000, 12000000],
        "haskell_memory": [2400000, 4800000, 12000000],
    },
    "List": {
        "color": "orange",
        "elements": [100000, 200000, 500000],
        "cpp_time": [180, 360, 900],
        "haskell_time": [0.8, 1.6, 4.0],
        "cpp_memory": [900000, 1800000, 4500000],
        "haskell_memory": [3500000, 7000000, 17500000],
    },
    "LinkedList": {
        "color": "purple",
        "elements": [100000, 200000, 500000],
        "cpp_time": [70, 160, 400],
        "haskell_time": [0.3, 0.65, 1.75],
        "cpp_memory": [800000, 1600000, 4000000],
        "haskell_memory": [3200000, 6400000, 16000000],
    },
}

def update_graph(selected_ds, axes):
    data = performance_data[selected_ds]
    color = data["color"]

    for ax in axes:
        ax.clear()

    axes[0].plot(data["elements"], data["cpp_time"], 'o-', color=color, label="C++ Time (s)")
    axes[0].plot(data["elements"], data["haskell_time"], 'o--', color="gold", label="Haskell Time (s)")
    axes[0].set_title(f"{selected_ds}: Execution Time Comparison", fontsize=16, weight="bold")
    axes[0].set_xlabel("Number of Elements", fontsize=12)
    axes[0].set_ylabel("Time (s)", fontsize=12)
    axes[0].grid(color="gray", linestyle="--", linewidth=0.5)
    axes[0].legend(fontsize=10)

    axes[1].plot(data["elements"], data["cpp_memory"], 'o-', color=color, label="C++ Memory (bytes)")
    axes[1].plot(data["elements"], data["haskell_memory"], 'o--', color="gold", label="Haskell Memory (bytes)")
    axes[1].set_title(f"{selected_ds}: Memory Usage Comparison", fontsize=16, weight="bold")
    axes[1].set_xlabel("Number of Elements", fontsize=12)
    axes[1].set_ylabel("Memory (bytes)", fontsize=12)
    axes[1].grid(color="gray", linestyle="--", linewidth=0.5)
    axes[1].legend(fontsize=10)

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
    root.configure(bg="#eeccff")
    title_label = tk.Label(root, text="Functional Data Structures: C++ vs Haskell", font=("Arial", 26, "bold"), bg="#eeccff", fg="purple")
    title_label.pack(pady=20)

    separator = ttk.Separator(root, orient='horizontal')
    separator.pack(fill='x', padx=20, pady=5)

    notebook = ttk.Notebook(root, style="TNotebook")
    notebook.pack(fill=tk.BOTH, expand=True, padx=10, pady=10)

    for ds_name in performance_data.keys():
        tab = ttk.Frame(notebook, style="TFrame")
        notebook.add(tab, text=ds_name)

        fig, axes = plt.subplots(2, 1, figsize=(10, 8), facecolor='#f5f5f5')
        fig.subplots_adjust(hspace=0.5)
        canvas = FigureCanvasTkAgg(fig, master=tab)
        canvas.get_tk_widget().pack(fill=tk.BOTH, expand=True, pady=10)

        update_graph(ds_name, axes)

        button_frame = tk.Frame(tab, bg="#eeccff")
        button_frame.pack(fill=tk.X, pady=10)

        reset_button = ttk.Button(button_frame, text="Reset Graphs", command=lambda axes=axes, ds_name=ds_name: update_graph(ds_name, axes), style="W.TButton")
        reset_button.pack(side=tk.LEFT, padx=10)

        return_button_tab = ttk.Button(button_frame, text="Return to Navigation", command=return_to_navigation, style="W.TButton")
        return_button_tab.pack(side=tk.RIGHT, padx=10)

    export_button_frame = tk.Frame(root, bg="#eeccff")
    export_button_frame.pack(fill=tk.X, padx=20, pady=20, side=tk.BOTTOM)

    return_button = ttk.Button(export_button_frame, text="Return to Navigation", command=return_to_navigation, style="W.TButton")
    return_button.pack(side=tk.LEFT, padx=20, pady=10)

    export_button = ttk.Button(export_button_frame, text="Export Data to CSV", command=export_to_csv, style="W.TButton")
    export_button.pack(side=tk.RIGHT, padx=20, pady=10)

def build_navigation_page():
    root.configure(bg="#eeccff")
    title_label = tk.Label(root, text="Welcome to Functional Data Structures Analysis", font=("Arial", 28, "bold"), bg="#eeccff", fg="purple")
    title_label.pack(pady=30)

    separator = ttk.Separator(root, orient='horizontal')
    separator.pack(fill='x', padx=20, pady=5)

    subtitle_label = tk.Label(root, text="Explore the comparisons of data structures in C++ and Haskell.", font=("Arial", 18), bg="#eeccff", fg="purple")
    subtitle_label.pack(pady=10)

    explore_button = ttk.Button(root, text="Go to Analysis", command=go_to_analysis, style="W.TButton")
    explore_button.pack(pady=30)

    complexity_button = ttk.Button(root, text="Complexity Analysis", command=show_complexity_analysis, style="W.TButton")
    complexity_button.pack(pady=10)

def show_complexity_analysis():
    complexity_window = tk.Toplevel(root)
    complexity_window.title("Complexity Analysis")
    complexity_window.geometry("700x500")
    complexity_window.configure(bg="#eeccff")

    title_label = tk.Label(complexity_window, text="Complexity Analysis of Data Structures", font=("Arial", 20, "bold"), bg="#eeccff", fg="purple")
    title_label.pack(pady=20)

    info_label = tk.Label(complexity_window, text="Time and Space Complexity Summary", font=("Arial", 14), bg="#eeccff", fg="purple")
    info_label.pack(pady=10)

    table_frame = tk.Frame(complexity_window, bg="#eeccff")
    table_frame.pack(pady=10, padx=20, fill="both", expand=True)

    headings = ["Data Structure", "C++ Time Complexity", "C++ Space Complexity", "Haskell Time Complexity", "Haskell Space Complexity"]
    for col, heading in enumerate(headings):
        header = tk.Label(table_frame, text=heading, font=("Arial", 12, "bold"), bg="#ccccff", fg="black", borderwidth=1, relief="solid")
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
            cell = tk.Label(table_frame, text=value, font=("Arial", 12), bg="#eeccff", fg="black", borderwidth=1, relief="solid")
            cell.grid(row=row, column=col, sticky="nsew", padx=1, pady=1)

    for col in range(len(headings)):
        table_frame.grid_columnconfigure(col, weight=1)

    return_button = ttk.Button(complexity_window, text="Return", command=complexity_window.destroy, style="W.TButton")
    return_button.pack(pady=20)

root = tk.Tk()
root.title("Functional Data Structures Analysis")
root.geometry("1500x1000")

style = ttk.Style()
style.configure("W.TButton", font=("Arial", 14, "bold"), foreground="black", background="#ccccff")
style.configure("TNotebook", tabposition='n')
style.configure("TFrame", background="#eeccff")

build_navigation_page()
root.mainloop()
