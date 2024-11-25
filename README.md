BST

Execution Time Measurement

C++: Uses std::chrono::high_resolution_clock for precise timing of insertions, measured in milliseconds, suitable for detailed performance tuning (cppreference.com).

Haskell: Utilizes Data.Time.Clock.getCurrentTime to record timestamps before and after operations, ideal for broader performance assessments. Detailed guidance is available on Stack Overflow.


Memory Usage Tracking

C++: Manually updates totalMemory during node insertion, providing continuous tracking of memory allocations.

Haskell: Combines System.Mem.performGC with GHC.Stats.getRTSStats to ensure memory measurements are accurate after garbage collection, reflecting true memory usage more relevant in a garbage-collected environment. Further details can be found on Stack Overflow.


Time Complexity

Both Languages: Insertion complexity is O(log n) in balanced cases but can worsen to O(n) with unbalanced insertions due to sorted inputs. This behavior is common across both paradigms but is more critically observed in functional implementations where immutability might introduce additional overhead.


Key Differences
C++ (Imperative): Focuses on micro-optimizations and precise control over memory and performance, suitable for environments where these factors are critical.

Haskell (Functional): Emphasizes correctness and maintainability, with performance measured in a way that accounts for garbage collection, which can significantly affect both runtime and memory usage.

