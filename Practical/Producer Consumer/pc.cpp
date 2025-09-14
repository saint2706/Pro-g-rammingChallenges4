/*
 * Modern Producer-Consumer Example (C++17)
 * ---------------------------------------
 * A bounded buffer implementation using std::mutex and std::condition_variable.
 * Features:
 *  - Configurable number of producers / consumers
 *  - Finite production (each producer produces N items) for graceful shutdown
 *  - Condition variable predicates to avoid spurious wake hazards
 *  - Encapsulated BoundedBuffer class with push/pop operations
 *  - Thread-safe structured logging with timestamps
 *  - Clean thread join (no busy infinite loop / detach)
 *  - Minimal CLI style parsing (portable, no external dependencies)
 *
 * Build (examples):
 *  g++ -std=c++17 -O2 -pthread pc.cpp -o pc
 *  (On Windows with MSVC) cl /std:c++17 /O2 pc.cpp
 *
 * Usage:
 *  ./pc -n 100 -P 2 -C 2 -c 8 -p 50 -q 75
 *    -n <total items per producer>
 *    -P <producer threads>
 *    -C <consumer threads>
 *    -c <buffer capacity>
 *    -p <producer delay ms>
 *    -q <consumer delay ms>
 * All flags optional; defaults shown in Config struct.
 */

#include <atomic>
#include <chrono>
#include <condition_variable>
#include <cstddef>
#include <cstdlib>
#include <iomanip>
#include <iostream>
#include <mutex>
#include <queue>
#include <string>
#include <thread>
#include <vector>

namespace pc
{

  struct Config
  {
    std::size_t bufferCapacity = 6;
    std::size_t itemsPerProducer = 25; // each producer produces this many items
    std::size_t producerThreads = 1;
    std::size_t consumerThreads = 1;
    std::chrono::milliseconds producerDelay{100};
    std::chrono::milliseconds consumerDelay{100};
  };

  // Simple bounded buffer (FIFO) for long values.
  class BoundedBuffer
  {
  public:
    explicit BoundedBuffer(std::size_t capacity) : capacity_(capacity) {}

    void push(long value)
    {
      std::unique_lock<std::mutex> lock(mtx_);
      not_full_cv_.wait(lock, [&]
                        { return queue_.size() < capacity_ || shutdown_; });
      if (shutdown_)
        return; // stop accepting if shutting down
      queue_.push(value);
      not_empty_cv_.notify_one();
    }

    // Returns false if shutdown and empty (no more items expected)
    bool pop(long &out)
    {
      std::unique_lock<std::mutex> lock(mtx_);
      not_empty_cv_.wait(lock, [&]
                         { return !queue_.empty() || shutdown_; });
      if (queue_.empty())
        return false; // shutdown & empty
      out = queue_.front();
      queue_.pop();
      not_full_cv_.notify_one();
      return true;
    }

    void shutdown()
    {
      {
        std::lock_guard<std::mutex> lock(mtx_);
        shutdown_ = true;
      }
      not_full_cv_.notify_all();
      not_empty_cv_.notify_all();
    }

    std::size_t size() const
    {
      std::lock_guard<std::mutex> lock(mtx_);
      return queue_.size();
    }

  private:
    std::size_t capacity_;
    mutable std::mutex mtx_;
    std::queue<long> queue_;
    std::condition_variable not_full_cv_;
    std::condition_variable not_empty_cv_;
    bool shutdown_ = false;
  };

  // Thread-safe logger
  class Logger
  {
  public:
    template <typename T>
    void log(const T &msg)
    {
      using namespace std::chrono;
      auto now = system_clock::now();
      auto ms = duration_cast<milliseconds>(now.time_since_epoch()) % 100000;
      std::lock_guard<std::mutex> lock(mu_);
      std::cout << '[' << std::setw(5) << ms.count() << "] " << msg << std::endl;
    }

  private:
    std::mutex mu_;
  };

  struct Stats
  {
    std::atomic<std::size_t> produced{0};
    std::atomic<std::size_t> consumed{0};
  };

} // namespace pc

// Minimal CLI parsing (expects flag followed by value; ignores unknowns)
pc::Config parse_args(int argc, char *argv[])
{
  pc::Config cfg;
  for (int i = 1; i < argc; ++i)
  {
    std::string flag = argv[i];
    auto next_val = [&](std::size_t &target)
    { if (i + 1 < argc) target = std::strtoull(argv[++i], nullptr, 10); };
    auto next_ms = [&](std::chrono::milliseconds &target)
    { if (i + 1 < argc) target = std::chrono::milliseconds(std::strtoull(argv[++i], nullptr, 10)); };
    if (flag == "-c")
      next_val(cfg.bufferCapacity);
    else if (flag == "-n")
      next_val(cfg.itemsPerProducer);
    else if (flag == "-P")
      next_val(cfg.producerThreads);
    else if (flag == "-C")
      next_val(cfg.consumerThreads);
    else if (flag == "-p")
      next_ms(cfg.producerDelay);
    else if (flag == "-q")
      next_ms(cfg.consumerDelay);
  }
  return cfg;
}

int main(int argc, char *argv[])
{
  using namespace pc;
  Config cfg = parse_args(argc, argv);
  Logger logger;
  Stats stats;
  BoundedBuffer buffer(cfg.bufferCapacity);

  auto start_time = std::chrono::steady_clock::now();
  std::atomic<std::size_t> activeProducers{cfg.producerThreads};

  // Producer thread function
  auto producer = [&](std::size_t id)
  {
    for (std::size_t i = 0; i < cfg.itemsPerProducer; ++i)
    {
      buffer.push(static_cast<long>(i));
      ++stats.produced;
      logger.log("P" + std::to_string(id) + " produced " + std::to_string(i));
      if (cfg.producerDelay.count() > 0)
        std::this_thread::sleep_for(cfg.producerDelay);
    }
    if (--activeProducers == 0)
    {
      // last producer signals no more data
      buffer.shutdown();
    }
  };

  // Consumer thread function
  auto consumer = [&](std::size_t id)
  {
    long value;
    while (buffer.pop(value))
    {
      ++stats.consumed;
      logger.log("C" + std::to_string(id) + " consumed " + std::to_string(value));
      if (cfg.consumerDelay.count() > 0)
        std::this_thread::sleep_for(cfg.consumerDelay);
    }
    logger.log("C" + std::to_string(id) + " exiting (no more data)");
  };

  std::vector<std::thread> threads;
  threads.reserve(cfg.producerThreads + cfg.consumerThreads);
  for (std::size_t p = 0; p < cfg.producerThreads; ++p)
    threads.emplace_back(producer, p);
  for (std::size_t c = 0; c < cfg.consumerThreads; ++c)
    threads.emplace_back(consumer, c);

  for (auto &t : threads)
    t.join();

  auto elapsed = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::steady_clock::now() - start_time);
  std::cout << "\nSummary:\n";
  std::cout << "  Producers:        " << cfg.producerThreads << " (" << cfg.itemsPerProducer << " each)\n";
  std::cout << "  Consumers:        " << cfg.consumerThreads << "\n";
  std::cout << "  Buffer capacity:  " << cfg.bufferCapacity << "\n";
  std::cout << "  Produced total:   " << stats.produced.load() << "\n";
  std::cout << "  Consumed total:   " << stats.consumed.load() << "\n";
  std::cout << "  Remaining in buf: " << buffer.size() << "\n";
  std::cout << "  Elapsed (ms):     " << elapsed.count() << "\n";
  std::cout << std::endl;
  return 0;
}
