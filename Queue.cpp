#include <iostream>
#include <memory>
#include <functional>
#include <stdexcept>
#include <chrono>
#include <vector>
#include <algorithm>

template<typename T>
class Queue {
public:
    struct Node {
        T value;
        std::shared_ptr<Node> next;
        Node(T val, std::shared_ptr<Node> nxt = nullptr) : value(val), next(nxt) {}
    };
    std::shared_ptr<Node> front;
    std::shared_ptr<Node> back;

public:
    Queue() : front(nullptr), back(nullptr) {}

    Queue enqueue(T value) const {
        auto newNode = std::make_shared<Node>(value);
        if (back) {
            back->next = newNode;
        }
        return Queue(front ? front : newNode, newNode);
    }

    Queue dequeue() const {
        if (!front) throw std::out_of_range("Empty queue");
        return Queue(front->next, front->next ? back : nullptr);
    }

    T peek() const {
        if (!front) throw std::out_of_range("Empty queue");
        return front->value;
    }

    bool isEmpty() const {
        return front == nullptr;
    }

private:
    Queue(std::shared_ptr<Node> f, std::shared_ptr<Node> b) : front(f), back(b) {}
};

void benchmarkQueue(size_t n) {
    Queue<int> queue;
    std::vector<int> values(n);
    std::generate(values.begin(), values.end(), []() { return std::rand() % 1000000; });

    auto start = std::chrono::high_resolution_clock::now();

   
    for (int value : values) {
        queue = queue.enqueue(value);
    }

    size_t memoryUsage = n * (sizeof(int) + sizeof(std::shared_ptr<typename Queue<int>::Node>));

    while (!queue.isEmpty()) {
        queue = queue.dequeue();
    }

    auto stop = std::chrono::high_resolution_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(stop - start);

    std::cout << "Time taken to enqueue and dequeue " << n << " elements: " << duration.count() << " milliseconds\n";
    std::cout << "Memory used: " << memoryUsage << " bytes\n";
}

int main() {
    benchmarkQueue(100000);
    benchmarkQueue(200000);
    benchmarkQueue(500000);
    return 0;
}
