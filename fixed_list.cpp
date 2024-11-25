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
class Array{
    private:
        T* a;
        int size;
        int pos1=0;
    public:
        ~Array(){
            delete a;
        }
        Array(int n){
            a=new T[n];
            size=n;
        }
        void addlast(T v,SIZE_T &b){
            if (pos1<size){
            SIZE_T m1=getMemoryUsage();
            a[pos1]=v;
            pos1++;
            SIZE_T m2=getMemoryUsage();
            b=b+(m2-m1);
            return;}
            else{
                cout<<"array is full"<<endl;
                return;
            }
        }
        void addfirst(T v,SIZE_T & b){
            SIZE_T f=0;
            if (pos1<size){
                SIZE_T m1=getMemoryUsage();
                int i=pos1;
                for(i;i>0;i--){
                    SIZE_T h1=getMemoryUsage();
                    a[i]=a[i-1];  
                    SIZE_T h2=getMemoryUsage();
                    f=f+(h2-h1);
                }   
            a[i]=v;
            pos1++;
            SIZE_T m2=getMemoryUsage();
            b=b+(m1-m2)+f ; 
            return;
        }
            else{
                cout<<"array is full"<<endl;
                return;
            }
        }
        void insert(T v, int pos,SIZE_T &b){
            if (pos<0 || pos>=size){
                cout<<"index out of bounds"<<endl;
                return;
            }
            if (pos1>=size){
                cout<<"array is full"<<endl;
                return;
            }
            else{
                SIZE_T m1=getMemoryUsage();
                int i=pos1;
                if(pos>=pos1){
                    a[pos1]=v;
                    pos1++;
                    SIZE_T m2=getMemoryUsage();
                    b=b+(m2-m1);
                    return;
                }
                else{
                    SIZE_T f=0;
                    for(i;i>pos;i--){
                        SIZE_T a1=getMemoryUsage();
                        a[i]=a[i-1];
                        SIZE_T a2=getMemoryUsage();
                        f=f+(a2-a1);
                    }
                    a[i]=v;
                    pos1++;
                    SIZE_T m2=getMemoryUsage();
                    b=b+(m2-m1)+f;
                    return;
                }
            }
        }
        void showc(){
            for(int i=0;i<pos1;i++){
                cout<<" "<<a[i]<<" ";
            }cout<<endl;
        }

};

void benchmarkaddlast(size_t n) {
    std::vector<int> values(n);

    std::generate(values.begin(), values.end(), []() { return std::rand() % 1000000; });
    auto start = std::chrono::high_resolution_clock::now();
    SIZE_T before = getMemoryUsage();
    Array<int> a(n);
    SIZE_T total2=0;
    for (int value : values) {
        a.addlast(value,total2);
    }
    SIZE_T after = getMemoryUsage();
    total2=total2+(after-before);
    auto stop = std::chrono::high_resolution_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(stop - start);

    std::cout << "Time taken to addlast " << n << " elements: " << duration.count() << " milliseconds\n";
    std::cout << "Memory used by addlast: " << (total2)  << " B" << std::endl;
}

void benchmarkaddfirst(size_t n) {
    std::vector<int> values(n);

    std::generate(values.begin(), values.end(), []() { return std::rand() % 1000000; });

    auto start = std::chrono::high_resolution_clock::now();
    SIZE_T total1=0;
    SIZE_T before=getMemoryUsage();
    Array<int> ch3(n);
    for (int value : values) {
        ch3.addfirst(value,total1);
    }
    auto stop = std::chrono::high_resolution_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(stop - start);
    SIZE_T after=getMemoryUsage();
    total1=total1+(after-before);
    std::cout << "Time taken to addfirst " << n << " elements: " << duration.count() << " milliseconds\n";
    std::cout << "Memory used by addfirst: " << (total1)  << " B" << std::endl;
}

void benchmarkinsert(size_t n) {
    std::vector<int> values(n);

    std::generate(values.begin(), values.end(), []() { return std::rand() % 1000000; });

    auto start = std::chrono::high_resolution_clock::now();
    SIZE_T total=0;
    SIZE_T before=getMemoryUsage();
    Array<int> ch2(n);
    for (int i=0;i<n;i++) {
        ch2.insert(values[i],i/2,total);
    }
   ;
    auto stop = std::chrono::high_resolution_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(stop - start);
    SIZE_T after=getMemoryUsage();
    total=total+(after-before);
    std::cout << "Time taken to insert " << n << " elements: " << duration.count() << " milliseconds\n";
    std::cout << "Memory used by insert: " << (total)  << " B" << std::endl;

}

int main(){
    benchmarkaddfirst(10000);
    benchmarkaddlast(10000);
    benchmarkinsert(10000);

    return 0;

}