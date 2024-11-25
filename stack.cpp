#include <iostream>
#include <memory>
#include <functional>
#include <stdexcept>
#include <chrono>
#include <algorithm>
#include <memory>

template<typename T>
class Stack {
public:
    struct Node {
        T value;
        std::shared_ptr<const Node> next;
        Node(T val, std::shared_ptr<const Node> nxt = nullptr) : value(val), next(nxt) {}
    };
    std::shared_ptr<const Node> head;

public:
    Stack() : head(nullptr) {}
    Stack(std::shared_ptr<const Node> nodes) : head(nodes) {}

    Stack push(T value) const {
        return Stack(std::make_shared<const Node>(value, head));
    }

    Stack pop() const {
        if (!head) throw std::out_of_range("Empty stack");
        return Stack(head->next);
    }

    T top() const {
        if (!head) throw std::out_of_range("Empty stack");
        return head->value;
    }

    bool isEmpty() const {
        return head == nullptr;
    }
};

void benchmarkStack(size_t n) {
    Stack<int> stack;
    std::vector<int> values(n);
    std::generate(values.begin(), values.end(), []() { return std::rand() % 1000000; });

    auto start = std::chrono::high_resolution_clock::now();

    // Push all elements onto the stack
    for (int value : values) {
        stack = stack.push(value);
    }

    // Calculate memory usage
    size_t memoryUsage = n * (sizeof(int) + sizeof(std::shared_ptr<const typename Stack<int>::Node>));

    // Pop all elements from the stack
    while (!stack.isEmpty()) {
        stack = stack.pop();
    }

    auto stop = std::chrono::high_resolution_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(stop - start);

    std::cout << "Time taken to push and pop " << n << " elements: " << duration.count() << " milliseconds\n";
    std::cout << "Memory used: " << memoryUsage << " bytes\n";
}


int main() {
benchmarkStack(100000);
benchmarkStack(200000);
benchmarkStack(500000);
return 0;
 }
