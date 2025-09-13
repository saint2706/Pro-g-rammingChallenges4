import java.util.concurrent.Semaphore;

class pcSem {
    public static void main(String[] args) {
        PC q = new PC();
        new Consumer(q);
        new Producer(q);
    }
}

class Producer implements Runnable {
    PC q;

    Producer(PC q) {
        this.q = q;
        new Thread(this, "Producer").start();
    }

    @Override
    public void run() {
        for (int i = 0; i < 20; i++) {
            q.put(i);
        }

    }
}

class Consumer implements Runnable {
    PC q;

    Consumer(PC q) {
        this.q = q;
        new Thread(this, "Consumer").start();
    }

    public void run() {
        for (int i = 0; i < 20; i++) {
            q.get();
            System.out.println("Consumer consumed " + i);
        }
    }
}

class PC {
    int item;
    static Semaphore con = new Semaphore(0);
    static Semaphore pro = new Semaphore(1);

    void get() {
        try {
            con.acquire();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        System.out.println("Consumer got " + item);
        pro.release();
    }

    void put(int item) {
        try {
            pro.acquire();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        this.item = item;
        System.out.println("Producer put " + item);
        con.release();
    }
}