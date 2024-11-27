#include <iostream>
#include <chrono>
#include <vector>
#include <cstdlib>
#include <list>

static size_t totalAllocated = 0;

void* operator new(size_t size) {
    totalAllocated += size;
    return malloc(size);
}

void operator delete(void* memory) noexcept {
    free(memory);
}

class Graph {
public:
    explicit Graph(size_t vertices)
        : adjacencyList(vertices) {}

    void addEdge(int u, int v) {
        adjacencyList[u].push_back(v);
    }

    static size_t getMemoryUsage() {
        return totalAllocated;
    }

private:
    std::vector<std::list<int>> adjacencyList;
};

void benchmarkGraph(size_t vertices, size_t edges) {
    totalAllocated = 0;
    Graph graph(vertices);
    srand((unsigned)time(nullptr));

    auto start = std::chrono::high_resolution_clock::now();
    for (size_t i = 0; i < edges; ++i) {
        int u = std::rand() % vertices;
        int v = std::rand() % vertices;
        graph.addEdge(u, v);
    }
    auto stop = std::chrono::high_resolution_clock::now();

    auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(stop - start);
    size_t memoryUsage = Graph::getMemoryUsage();

    std::cout << "Benchmark Results for " << vertices << " vertices and " << edges << " edges:\n";
    std::cout << "Time taken: " << duration.count() << " milliseconds\n";
    std::cout << "Memory used: " << memoryUsage << " bytes\n";
    std::cout << "----------------------------------------\n";
}

int main() {
    benchmarkGraph(1000, 5000);
    benchmarkGraph(2000, 10000);
    benchmarkGraph(5000, 25000);

    return 0;
}
