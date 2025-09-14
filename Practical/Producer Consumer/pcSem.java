/*
 * Modern Producer-Consumer (Semaphore Variant)
 * -------------------------------------------
 * Demonstrates a bounded buffer using counting semaphores (classic pattern):
 *   - emptySlots semaphore tracks remaining capacity
 *   - filledSlots semaphore tracks available items
 *   - A mutex (binary semaphore) guards the critical section on the queue
 *
 * Features added:
 *   - Configurable producers / consumers / items / capacity / delays via CLI (key=value pairs)
 *   - Graceful shutdown using finite production + poison-pill sentinels for consumers
 *   - Thread-safe logging with timestamps (millis since start)
 *   - Summary statistics at completion
 *   - Clear separation of concerns (Config, Stats, BoundedBuffer, Producer, Consumer)
 *
 * Build:
 *   javac pcSem.java
 * Run (example):
 *   java pcSem itemsPerProducer=50 producers=2 consumers=3 capacity=8 prodDelayMs=20 consDelayMs=35
 */

import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;

public class pcSem {

    /*
     * ------------------------------ Config & Stats ------------------------------
     */
    static final class Config {
        int capacity = 6;
        int itemsPerProducer = 25;
        int producers = 1;
        int consumers = 1;
        int prodDelayMs = 100;
        int consDelayMs = 100;
        boolean quiet = false;
        boolean timestamps = true;
    }

    static final class Stats {
        volatile long produced = 0;
        volatile long consumed = 0;
    }

    /*
     * ------------------------------ Logging Utility -----------------------------
     */
    static final class Logger {
        private final boolean enabled;
        private final boolean timestamps;
        private final Instant start = Instant.now();

        Logger(boolean enabled, boolean timestamps) {
            this.enabled = enabled;
            this.timestamps = timestamps;
        }

        synchronized void log(String msg) {
            if (!enabled)
                return;
            if (timestamps) {
                long ms = Duration.between(start, Instant.now()).toMillis();
                System.out.printf("[%05d ms] %s%n", ms, msg);
            } else {
                System.out.println(msg);
            }
        }
    }

    /*
     * ----------------------------- Bounded Buffer ------------------------------
     */
    /**
     * A bounded FIFO buffer implemented on top of an array list, coordinated via
     * three semaphores: emptySlots, filledSlots, and a binary mutex. This mirrors
     * the classical Dijkstra solution while supporting multiple producers &
     * consumers.
     */
    static final class BoundedBuffer<T> {
        private final Object[] data;
        private int head = 0; // removal index
        private int tail = 0; // insertion index
        private int size = 0;

        private final Semaphore emptySlots; // counts remaining capacity
        private final Semaphore filledSlots; // counts available items
        private final Semaphore mutex = new Semaphore(1); // binary semaphore for mutual exclusion
        private volatile boolean shutdown = false; // signal to stop accepting new items

        BoundedBuffer(int capacity) {
            if (capacity <= 0)
                throw new IllegalArgumentException("capacity must be > 0");
            this.data = new Object[capacity];
            this.emptySlots = new Semaphore(capacity);
            this.filledSlots = new Semaphore(0);
        }

        void put(T item) throws InterruptedException {
            if (shutdown)
                return; // ignore late puts after shutdown
            emptySlots.acquire();
            mutex.acquire();
            try {
                if (shutdown) { // re-check after acquiring
                    // restore semaphore state and bail
                    mutex.release();
                    emptySlots.release();
                    return;
                }
                data[tail] = item;
                tail = (tail + 1) % data.length;
                size++;
            } finally {
                mutex.release();
                filledSlots.release();
            }
        }

        @SuppressWarnings("unchecked")
        T take() throws InterruptedException {
            filledSlots.acquire();
            mutex.acquire();
            try {
                T item = (T) data[head];
                data[head] = null; // help GC
                head = (head + 1) % data.length;
                size--;
                return item;
            } finally {
                mutex.release();
                emptySlots.release();
            }
        }

        int size() throws InterruptedException {
            // Acquire mutex briefly to read size consistently
            mutex.acquire();
            try {
                return size;
            } finally {
                mutex.release();
            }
        }

        void shutdown() {
            shutdown = true;
        }
    }

    /*
     * ----------------------------- Producer / Consumer -------------------------
     */
    static final class Producer implements Runnable {
        private final int id;
        private final Config cfg;
        private final Stats stats;
        private final BoundedBuffer<Object> buffer;
        private final Logger log;

        Producer(int id, Config cfg, Stats stats, BoundedBuffer<Object> buffer, Logger log) {
            this.id = id;
            this.cfg = cfg;
            this.stats = stats;
            this.buffer = buffer;
            this.log = log;
        }

        @Override
        public void run() {
            try {
                for (int i = 0; i < cfg.itemsPerProducer; i++) {
                    buffer.put(new Item(id, i));
                    stats.produced++;
                    log.log("P" + id + " produced seq=" + i);
                    if (cfg.prodDelayMs > 0)
                        TimeUnit.MILLISECONDS.sleep(cfg.prodDelayMs);
                }
            } catch (InterruptedException ie) {
                Thread.currentThread().interrupt();
                log.log("P" + id + " interrupted");
            }
        }
    }

    static final class Consumer implements Runnable {
        private final int id;
        private final Config cfg;
        private final Stats stats;
        private final BoundedBuffer<Object> buffer;
        private final Logger log;

        Consumer(int id, Config cfg, Stats stats, BoundedBuffer<Object> buffer, Logger log) {
            this.id = id;
            this.cfg = cfg;
            this.stats = stats;
            this.buffer = buffer;
            this.log = log;
        }

        @Override
        public void run() {
            try {
                while (true) {
                    Object obj = buffer.take();
                    if (obj == POISON_PILL) {
                        log.log("C" + id + " received poison pill -> exit");
                        break;
                    }
                    Item item = (Item) obj;
                    stats.consumed++;
                    log.log("C" + id + " consumed P" + item.producerId + " seq=" + item.sequence);
                    if (cfg.consDelayMs > 0)
                        TimeUnit.MILLISECONDS.sleep(cfg.consDelayMs);
                }
            } catch (InterruptedException ie) {
                Thread.currentThread().interrupt();
                log.log("C" + id + " interrupted");
            }
        }
    }

    /*
     * ----------------------------- Data Model ----------------------------------
     */
    static final class Item {
        final int producerId;
        final int sequence;

        Item(int producerId, int sequence) {
            this.producerId = producerId;
            this.sequence = sequence;
        }
    }

    private static final Object POISON_PILL = new Object();

    /*
     * ----------------------------- CLI Parsing ---------------------------------
     */
    static Config parseArgs(String[] args) {
        Config cfg = new Config();
        for (String arg : args) {
            if (!arg.contains("="))
                continue;
            String[] parts = arg.split("=", 2);
            String k = parts[0].trim();
            String v = parts[1].trim();
            try {
                switch (k) {
                    case "capacity":
                        cfg.capacity = Integer.parseInt(v);
                        break;
                    case "itemsPerProducer":
                        cfg.itemsPerProducer = Integer.parseInt(v);
                        break;
                    case "producers":
                        cfg.producers = Integer.parseInt(v);
                        break;
                    case "consumers":
                        cfg.consumers = Integer.parseInt(v);
                        break;
                    case "prodDelayMs":
                        cfg.prodDelayMs = Integer.parseInt(v);
                        break;
                    case "consDelayMs":
                        cfg.consDelayMs = Integer.parseInt(v);
                        break;
                    case "quiet":
                        cfg.quiet = Boolean.parseBoolean(v);
                        break;
                    case "timestamps":
                        cfg.timestamps = Boolean.parseBoolean(v);
                        break;
                    default: // ignore unknown
                }
            } catch (NumberFormatException nfe) {
                System.err.println("Ignoring malformed arg: " + arg);
            }
        }
        return cfg;
    }

    /*
     * ----------------------------- Main ----------------------------------------
     */
    public static void main(String[] args) throws InterruptedException {
        Config cfg = parseArgs(args);
        Logger log = new Logger(!cfg.quiet, cfg.timestamps);
        Stats stats = new Stats();
        BoundedBuffer<Object> buffer = new BoundedBuffer<>(cfg.capacity);

        log.log("Starting with P=" + cfg.producers + " C=" + cfg.consumers +
                " capacity=" + cfg.capacity + " itemsPerProducer=" + cfg.itemsPerProducer);

        Instant start = Instant.now();

        List<Thread> threads = new ArrayList<>();
        // Launch consumers first (optionally) to avoid initial backlog
        for (int c = 0; c < cfg.consumers; c++) {
            Thread t = new Thread(new Consumer(c, cfg, stats, buffer, log), "Consumer-" + c);
            threads.add(t);
            t.start();
        }
        for (int p = 0; p < cfg.producers; p++) {
            Thread t = new Thread(new Producer(p, cfg, stats, buffer, log), "Producer-" + p);
            threads.add(t);
            t.start();
        }

        // Wait for all producers to finish
        for (Thread t : threads) {
            if (t.getName().startsWith("Producer-"))
                t.join();
        }

        // Insert one poison pill per consumer to gracefully stop them
        for (int i = 0; i < cfg.consumers; i++) {
            buffer.put(POISON_PILL);
        }

        // Join consumers
        for (Thread t : threads) {
            if (t.getName().startsWith("Consumer-"))
                t.join();
        }

        buffer.shutdown();

        long elapsedMs = Duration.between(start, Instant.now()).toMillis();
        long expected = (long) cfg.producers * cfg.itemsPerProducer;
        boolean success = stats.produced == expected && stats.consumed == expected;

        System.out.println();
        System.out.println("Summary:");
        System.out.println("  Producers:        " + cfg.producers + " (each " + cfg.itemsPerProducer + ")");
        System.out.println("  Consumers:        " + cfg.consumers);
        System.out.println("  Capacity:         " + cfg.capacity);
        System.out.println("  Produced total:   " + stats.produced);
        System.out.println("  Consumed total:   " + stats.consumed);
        System.out.println("  Elapsed (ms):     " + elapsedMs);
        System.out.println("  Success:          " + success);

        if (!success)
            System.exit(1);
    }
}