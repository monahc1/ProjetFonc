#include <iostream>
#include <chrono>
#include <vector>
#include <algorithm>
#include <cstdlib>
#include <memory>
#include <functional>

// Global memory usage counter
static size_t totalAllocated = 0;

void* operator new(size_t size) {
    totalAllocated += size;
    return malloc(size);
}

void operator delete(void* memory, size_t size) noexcept {
    totalAllocated -= size;
    free(memory);
}

template<typename T>
class BST {
public:
    BST() : root(nullptr) {}

    void insert(const T& value) {
        insert(value, root);
    }

    void inorder(std::function<void(const T&)> visit) const {
        inorder(visit, root);
    }

    static size_t getMemoryUsage() {
        return totalAllocated;
    }

private:
    struct Node {
        T value;
        Node* left = nullptr, *right = nullptr;
        Node(T val) : value(val) {}
        ~Node() {
            delete left;
            delete right;
        }
    };

    Node* root;

    void insert(const T& value, Node*& node) {
        if (!node) {
            node = new Node(value);
        } else if (value < node->value) {
            insert(value, node->left);
        } else if (value > node->value) {
            insert(value, node->right);
        }
    }

    void inorder(std::function<void(const T&)> visit, Node* node) const {
        if (node) {
            inorder(visit, node->left);
            visit(node->value);
            inorder(visit, node->right);
        }
    }
};

void benchmarkInsertion(size_t n) {
    totalAllocated = 0; // Reset memory counter
    BST<int> bst;
    std::vector<int> values(n);
    std::generate(values.begin(), values.end(), []() { return std::rand() % 1000000; });

    auto start = std::chrono::high_resolution_clock::now();
    for (int value : values) {
        bst.insert(value);
    }
    auto stop = std::chrono::high_resolution_clock::now();

    auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(stop - start);
    size_t memoryUsage = BST<int>::getMemoryUsage();

    std::cout << "Benchmark Results for " << n << " elements:\n";
    std::cout << "Time taken: " << duration.count() << " milliseconds\n";
    std::cout << "Memory used: " << memoryUsage << " bytes\n";
    std::cout << "----------------------------------------\n";
}

int main() {
    benchmarkInsertion(100000);
    benchmarkInsertion(200000);
    benchmarkInsertion(500000);

    return 0;
}
