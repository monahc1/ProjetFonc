#include <iostream>
#include <vector>
#include <list>
#include <chrono>
#include <cstdlib>

static size_t customAllocatedMemory = 0;

template<typename K, typename V>
class HashTable {
private:
    struct Node {
        K key;
        V value;
        Node(K key, V value) : key(key), value(value) {
            customAllocatedMemory += sizeof(Node);
        }
        ~Node() {
            customAllocatedMemory -= sizeof(Node);
        }
    };

    std::vector<std::list<Node*>> table;
    int capacity;

    int hash(K key) {
        return std::hash<K>{}(key) % capacity;
    }

public:
    HashTable(int size) : capacity(size) {
        table.resize(capacity);
    }

    ~HashTable() {
        for (auto &bucket : table) {
            for (auto node : bucket) {
                delete node;
            }
        }
    }

    void insert(K key, V value) {
        int index = hash(key);
        for (auto node : table[index]) {
            if (node->key == key) {
                node->value = value;
                return;
            }
        }
        table[index].push_back(new Node(key, value));
    }

    V* get(K key) {
        int index = hash(key);
        for (auto node : table[index]) {
            if (node->key == key) {
                return &node->value;
            }
        }
        return nullptr;
    }

    void displayHashTable() {
        for (int i = 0; i < capacity; i++) {
            std::cout << "Bucket " << i << ": ";
            for (auto node : table[i]) {
                std::cout << "(" << node->key << ", " << node->value << ") ";
            }
            std::cout << std::endl;
        }
    }
};

void benchmark(int n) {
    HashTable<int, int> ht(n);

    auto start = std::chrono::high_resolution_clock::now();
    for (int i = 0; i < n; i++) {
        ht.insert(rand() % n, rand() % 1000);
    }
    auto stop = std::chrono::high_resolution_clock::now();

    auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(stop - start).count();
    std::cout << "Benchmark Results for " << n << " elements:\n";
    std::cout << "Time taken: " << duration << " ms\n";
    std::cout << "Memory used: " << customAllocatedMemory << " bytes\n";
    std::cout << "----------------------------------------\n";
}

int main() {
    srand(time(NULL));
    benchmark(10000);
    benchmark(20000);
    benchmark(50000);
    return 0;
}
