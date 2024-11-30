import tkinter as tk
from tkinter import ttk
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg
import matplotlib.pyplot as plt


elements = [100000, 200000, 300000, 400000, 500000]
cpp_time = [125, 250, 375, 500, 625] 
haskell_time = [150, 300, 450, 600, 750]  
cpp_memory = [2400000, 4800000, 7200000, 9600000, 12000000]  
haskell_memory = [2600000, 5200000, 7800000, 10400000, 13000000] 


def update_graph(selected_operation):
    operation_title = f"{selected_operation.capitalize()} Comparison"
    

    for ax in axs:
        ax.clear()
    
    axs[0].plot(elements, cpp_time, 'o-', color='purple', label='C++ Execution Time (ms)')
    axs[0].plot(elements, haskell_time, 'o-', color='yellow', label='Haskell Execution Time (ms)')
    axs[0].set_title(f"Execution Time Comparison", fontsize=14)
    axs[0].set_xlabel("Number of Elements", fontsize=12)
    axs[0].set_ylabel("Time (ms)", fontsize=12)
    axs[0].tick_params(axis='both', labelsize=10)
    axs[0].grid(color='gray', linestyle='--', linewidth=0.5)
    axs[0].legend(fontsize=10)


    axs[1].plot(elements, cpp_memory, 'o-', color='purple', label='C++ Memory Usage (bytes)')
    axs[1].plot(elements, haskell_memory, 'o-', color='yellow', label='Haskell Memory Usage (bytes)')
    axs[1].set_title(f"Memory Usage Comparison", fontsize=14)
    axs[1].set_xlabel("Number of Elements", fontsize=12)
    axs[1].set_ylabel("Memory (bytes)", fontsize=12)
    axs[1].tick_params(axis='both', labelsize=10)
    axs[1].grid(color='gray', linestyle='--', linewidth=0.5)
    axs[1].legend(fontsize=10)


    fig.suptitle(operation_title, fontsize=16, weight='bold', y=0.95)
    canvas.draw()


root = tk.Tk()
root.title("Operation Comparison: C++ vs Haskell")


operation_label = tk.Label(root, text="Select Operation:", font=("Arial", 12))
operation_label.pack(pady=5, anchor="w")

operation_var = tk.StringVar(value="addLast")
operation_menu = ttk.Combobox(root, textvariable=operation_var, font=("Arial", 12), width=15)
operation_menu['values'] = ["addLast", "addFirst", "insert"]
operation_menu.pack(pady=5, anchor="w")
operation_menu.bind("<<ComboboxSelected>>", lambda e: update_graph(operation_var.get()))


fig, axs = plt.subplots(2, 1, figsize=(10, 8))
fig.subplots_adjust(hspace=0.5, top=0.85)  


canvas = FigureCanvasTkAgg(fig, master=root)
canvas.get_tk_widget().pack(fill=tk.BOTH, expand=True)


update_graph(operation_var.get())


root.mainloop()
