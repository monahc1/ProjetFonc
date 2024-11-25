#include <iostream>
#include <chrono>
#include <vector>
#include <algorithm>
#include <memory>
#include <functional>
#include <windows.h>
#include <psapi.h>
using namespace std;

SIZE_T getMemoryUsage() {
    PROCESS_MEMORY_COUNTERS memInfo;
    GetProcessMemoryInfo(GetCurrentProcess(), &memInfo, sizeof(memInfo));
    return memInfo.WorkingSetSize; // Memory usage in bytes
}
template<typename T>
class chained{
public:
    chained() : first(nullptr) {}

    void addlast(const T &v,SIZE_T &a){
        addl(v,first,last,a);
    }

    void showc(){
        showc1(first);
    }
    void remove(T v){
        rm(v,first);
    }
    void insert(const T &v,int pos,SIZE_T &a){
        insert1(v,first,pos,a);
    }
    ~chained() {
    Node* current = first;
    while (current) {
        Node* toDelete = current;
        current = current->next;
        delete toDelete;
    }
}

private:
    struct Node{
    T value;
    Node * next;
    Node(T value) : value(value), next(nullptr) {}
    };
    Node * first;
    Node * last;
    void addl(const T &v,Node* &first,Node * &last,SIZE_T &a){
            SIZE_T b=getMemoryUsage();
            Node * node1=new Node(v);
            if (first == nullptr) {
                first = node1;
                last = node1;
            } else {
                last->next = node1;
                last = node1;
            }
            SIZE_T c=getMemoryUsage();
            a=a+(c-b);
    }
    void showc1(Node * first){
        if (first==nullptr){
            cout<<"no list to show";
            return;
        }
        Node * tmp=first;
        while(tmp!=nullptr){
            cout<<" "<<tmp->value<<" ";
            tmp=tmp->next; 
        }
        cout<<endl;
        return;
    }
    void rm(T v, Node*& first) {
    if (first == nullptr) {
        cout << "List is empty, nothing to remove." << endl;
        return;
    }

    Node* pt = first;
    Node* pt1 = nullptr;

    
    while (pt != nullptr && pt->value != v) {
        pt1 = pt;       
        pt = pt->next; 
    }

    if (pt == nullptr) {
        cout << "Value " << v << " not found in the list." << endl;
        return;
    }

    if (pt == first) {
        first = pt->next; 
    } else {
        pt1->next = pt->next; 
    }
    cout << "Node with value " << v << " removed." << endl;
    delete pt; 
}
    void insert1(const T &v,Node * &first,int pos,SIZE_T &a){
        SIZE_T b=getMemoryUsage();
        Node * n1=new Node(v);
        SIZE_T f=0;
        if(pos==0){
            SIZE_T m1=getMemoryUsage();
            n1->next=first;
            first=n1;
            SIZE_T m2=getMemoryUsage();
            f=f+(m2-m1);
            return;
        }
        if (first == nullptr && pos > 0) {
            SIZE_T m1=getMemoryUsage();
            first=n1;
            SIZE_T m2=getMemoryUsage();
            f=f+(m2-m1);
            return;
        }
        else{
            SIZE_T a1=getMemoryUsage();
            int c=0;
            Node * tmp=first;
            while(c<pos-1 && tmp->next!=nullptr ){
                SIZE_T m1=getMemoryUsage();
                tmp=tmp->next;
                c++;
                SIZE_T m2=getMemoryUsage();
                f=f+(m2-m1);
            }
            n1->next=tmp->next;
            tmp->next=n1;
            SIZE_T a2=getMemoryUsage();
            f=f+(a2-a1);

        }
        SIZE_T c=getMemoryUsage();
        a=a+(c-b)+f;
        return;
    }
};


void benchmarkaddlast(size_t n) {
    chained<int> ch1;
    std::vector<int> values(n);

    std::generate(values.begin(), values.end(), []() { return std::rand() % 1000000; });
    auto start = std::chrono::high_resolution_clock::now();
    SIZE_T before = getMemoryUsage();
    SIZE_T total2=0;
    for (int value : values) {
        ch1.addlast(value,total2);
    }
    SIZE_T after = getMemoryUsage();
    total2=total2+(after-before);
    auto stop = std::chrono::high_resolution_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(stop - start);

    std::cout << "Time taken to addlast " << n << " elements: " << duration.count() << " milliseconds\n";
    std::cout << "Memory used by addlast: " << (total2) / 1024 << " KB" << std::endl;
}

void benchmarkaddfirst(size_t n) {
    chained<int> ch3;
    std::vector<int> values(n);

    std::generate(values.begin(), values.end(), []() { return std::rand() % 1000000; });

    auto start = std::chrono::high_resolution_clock::now();
    SIZE_T total1=0;
    SIZE_T before=getMemoryUsage();
    for (int value : values) {
        ch3.insert(value,0,total1);
    }
    auto stop = std::chrono::high_resolution_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(stop - start);
    SIZE_T after=getMemoryUsage();
    total1=total1+(after-before);
    std::cout << "Time taken to addfirst " << n << " elements: " << duration.count() << " milliseconds\n";
    std::cout << "Memory used by addfirst: " << (total1) / 1024 << " KB" << std::endl;
}

void benchmarkinsert(size_t n) {
    chained<int> ch2;
    std::vector<int> values(n);

    std::generate(values.begin(), values.end(), []() { return std::rand() % 1000000; });

    auto start = std::chrono::high_resolution_clock::now();
    SIZE_T total=0;
    SIZE_T before=getMemoryUsage();
    for (int i=0;i<n;i++) {
        ch2.insert(values[i],i/2,total);
    }
   ;
    auto stop = std::chrono::high_resolution_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(stop - start);
    SIZE_T after=getMemoryUsage();
    total=total+(after-before);
    std::cout << "Time taken to insert " << n << " elements: " << duration.count() << " milliseconds\n";
    std::cout << "Memory used by insert: " << (total) / 1024 << " KB" << std::endl;

}

int main() {
    benchmarkaddfirst(100000);
    benchmarkaddlast(100000);
    benchmarkinsert(100000);

    return 0;
}
