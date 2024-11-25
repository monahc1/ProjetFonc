#include <iostream>
#include <chrono>
#include <vector>
#include <algorithm>
#include <cstdlib>
#include <memory>
#include <functional>

template<typename T>
class BST {
public:
    BST() : root(nullptr), totalMemory(0) {}

    void insert(const T& value) {
        insert(value, root);
    }

    void inorder(std::function<void(const T&)> visit) const {
        inorder(visit, root);
    }

    size_t getMemoryUsage() const {
        return totalMemory;
    }

private:
    struct Node {
        T value;
        std::unique_ptr<Node> left, right;
        Node(T val) : value(val), left(nullptr), right(nullptr) {}
    };

    std::unique_ptr<Node> root;
    size_t totalMemory; // Tracks memory usage

    void insert(const T& value, std::unique_ptr<Node>& node) {
        if (!node) {
            node = std::make_unique<Node>(value);
            totalMemory += sizeof(Node); // Increment memory usage
        } else if (value < node->value) {
            insert(value, node->left);
        } else if (value > node->value) {
            insert(value, node->right);
        }
    }

    void inorder(std::function<void(const T&)> visit, const std::unique_ptr<Node>& node) const {
        if (node) {
            inorder(visit, node->left);
            visit(node->value);
            inorder(visit, node->right);
        }
    }
};

void benchmarkInsertion(size_t n) {
    BST<int> bst;
    std::vector<int> values(n);

    std::generate(values.begin(), values.end(), []() { return std::rand() % 1000000; });

    auto start = std::chrono::high_resolution_clock::now();

    for (int value : values) {
        bst.insert(value);
    }

    auto stop = std::chrono::high_resolution_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(stop - start);

    size_t memoryUsage = bst.getMemoryUsage();

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
