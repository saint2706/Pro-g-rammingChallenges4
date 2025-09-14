import java.time.Duration;
import java.time.Instant;
import java.util.LinkedList;
import java.util.List;

/**
 * Modern producer-consumer example using intrinsic locks (synchronized) and
 * wait/notifyAll. Demonstrates a bounded FIFO with graceful shutdown.
 *
 * Differences vs pcSem:
 * - Uses intrinsic monitor methods (wait/notifyAll) instead of Semaphores.
 * - Explicit predicate checks inside while loops (handles spurious wakeups).
 * - Employs a poison-pill sentinel to stop consumers.
 *
 * CLI args use key=value form. Example:
 * java pcLL producers=2 consumers=2 itemsPerProducer=10 capacity=5
 * prodDelayMs=50 consDelayMs=80
 */
public class pcLL {
    /*
     * ------------------------------- Data Classes ------------------------------
     */
    static final class Config {
        int capacity = 4;
        int itemsPerProducer = 20;
        int producers = 1;
        int consumers = 1;
        int prodDelayMs = 100;
        int consDelayMs = 100;
        boolean quiet = false;
        boolean timestamps = true;
    }

    static final class Stats {
        long produced = 0;
        long consumed = 0;
    }

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
     * ------------------------------- Buffer ------------------------------------
     */
    static final class BoundedQueue<T> {
        private final LinkedList<T> list = new LinkedList<>();
        private final int capacity;
        private boolean shutdown = false;

        BoundedQueue(int capacity) {
            if (capacity <= 0)
                throw new IllegalArgumentException();
            this.capacity = capacity;
        }

        public synchronized void put(T item) throws InterruptedException {
            while (!shutdown && list.size() == capacity) {
                wait(); // wait for space
            }
            if (shutdown)
                return; // reject puts after shutdown
            list.addLast(item);
            notifyAll(); // signal item available
        }

        public synchronized T take() throws InterruptedException {
            while (list.isEmpty() && !shutdown) {
                wait(); // wait for item
            }
            if (list.isEmpty())
                return null; // shutdown & empty
            T val = list.removeFirst();
            notifyAll(); // signal space
            return val;
        }

        public synchronized void shutdown() {
            shutdown = true;
            notifyAll();
        }

        public synchronized int size() {
            return list.size();
        }
    }

    /*
     * ------------------------------- Item & Sentinel ---------------------------
     */
    static final class Item {
        final int producerId;
        final int sequence;

        Item(int p, int s) {
            producerId = p;
            sequence = s;
        }
    }

    private static final Object POISON_PILL = new Object();

    /*
     * ------------------------------- Producers ---------------------------------
     */
    static final class Producer implements Runnable {
        private final int id;
        private final Config cfg;
        private final Stats stats;
        private final BoundedQueue<Object> q;
        private final Logger log;

        Producer(int id, Config cfg, Stats stats, BoundedQueue<Object> q, Logger log) {
            this.id = id;
            this.cfg = cfg;
            this.stats = stats;
            this.q = q;
            this.log = log;
        }

        @Override
        public void run() {
            try {
                for (int i = 0; i < cfg.itemsPerProducer; i++) {
                    q.put(new Item(id, i));
                    synchronized (stats) {
                        stats.produced++;
                    }
                    log.log("P" + id + " produced seq=" + i);
                    if (cfg.prodDelayMs > 0)
                        Thread.sleep(cfg.prodDelayMs);
                }
            } catch (InterruptedException ie) {
                Thread.currentThread().interrupt();
                log.log("P" + id + " interrupted");
            }
        }
    }

    /* ------------------------------- Consumers -------------------------------- */
    static final class Consumer implements Runnable {
        private final int id;
        private final Config cfg;
        private final Stats stats;
        private final BoundedQueue<Object> q;
        private final Logger log;

        Consumer(int id, Config cfg, Stats stats, BoundedQueue<Object> q, Logger log) {
            this.id = id;
            this.cfg = cfg;
            this.stats = stats;
            this.q = q;
            this.log = log;
        }

        @Override
        public void run() {
            try {
                while (true) {
                    Object obj = q.take();
                    if (obj == null)
                        break; // shutdown & empty
                    if (obj == POISON_PILL) {
                        log.log("C" + id + " received poison pill -> exit");
                        break;
                    }
                    Item it = (Item) obj;
                    synchronized (stats) {
                        stats.consumed++;
                    }
                    log.log("C" + id + " consumed P" + it.producerId + " seq=" + it.sequence);
                    if (cfg.consDelayMs > 0)
                        Thread.sleep(cfg.consDelayMs);
                }
            } catch (InterruptedException ie) {
                Thread.currentThread().interrupt();
                log.log("C" + id + " interrupted");
            }
        }
    }

    /*
     * ------------------------------- CLI Parsing -------------------------------
     */
    static Config parseArgs(String[] args) {
        Config cfg = new Config();
        for (String arg : args) {
            if (!arg.contains("="))
                continue;
            String[] parts = arg.split("=", 2);
            String k = parts[0];
            String v = parts[1];
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
                    default: // ignore
                }
            } catch (NumberFormatException e) {
                System.err.println("Ignoring malformed arg: " + arg);
            }
        }
        return cfg;
    }

    /*
     * -------------------------------- Main -------------------------------------
     */
    public static void main(String[] args) throws InterruptedException {
        Config cfg = parseArgs(args);
        Logger log = new Logger(!cfg.quiet, cfg.timestamps);
        Stats stats = new Stats();
        BoundedQueue<Object> queue = new BoundedQueue<>(cfg.capacity);

        log.log("Starting with P=" + cfg.producers + " C=" + cfg.consumers + " capacity=" + cfg.capacity
                + " itemsPerProducer=" + cfg.itemsPerProducer);

        Instant start = Instant.now();
        List<Thread> threads = new LinkedList<>();
        // Consumers
        for (int c = 0; c < cfg.consumers; c++) {
            Thread t = new Thread(new Consumer(c, cfg, stats, queue, log), "Consumer-" + c);
            threads.add(t);
            t.start();
        }
        // Producers
        for (int p = 0; p < cfg.producers; p++) {
            Thread t = new Thread(new Producer(p, cfg, stats, queue, log), "Producer-" + p);
            threads.add(t);
            t.start();
        }

        // Join producers first
        for (Thread t : threads) {
            if (t.getName().startsWith("Producer-"))
                t.join();
        }
        // Insert poison pills
        for (int i = 0; i < cfg.consumers; i++) {
            queue.put(POISON_PILL);
        }
        // Join consumers
        for (Thread t : threads) {
            if (t.getName().startsWith("Consumer-"))
                t.join();
        }

        queue.shutdown();

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