#include <condition_variable>
#include <iostream>
#include <mutex>
#include <queue>
#include <string>
#include <thread>


using namespace std;

queue<long> dataProd;

mutex pc_mutex, output_mutex;

condition_variable consumerSync;

void print(string str) {
  lock_guard<mutex> lg(output_mutex);
  cout << str << endl;
}

void prod() {
  int idx = 0;
  while (1) {
    unique_lock<mutex> ul(pc_mutex);
    if (dataProd.size() >= 6) {
      print("Producer: Queue is full");
      consumerSync.wait(ul, []() { return !(dataProd.size() >= 6); });)
    }
    dataProd.push(idx);
    ul.unlock();
    consumerSync.notify_one();
    this_thread::sleep_for(chrono::milliseconds(100));
    print("Producer: Produced " + to_string(idx));
    idx++;
  }
}

void cons() {
  while (1) {
    unique_lock<mutex> ul(pc_mutex);
    if (dataProd.size() == 0) {
      print("Consumer: Queue is empty");
      consumerSync.wait(ul, []() { return !(dataProd.size() == 0); });
    }
    long data = dataProd.front();
    dataProd.pop();
    ul.unlock();
    consumerSync.notify_one();
    this_thread::sleep_for(chrono::milliseconds(100));
    print("Consumer: Consumed " + to_string(data));
  }
}

int main() {
  thread prodThread(prod);
  thread consThread(cons);
  prodThread.detach();
  consThread.detach();
  while (1)
    ;
}