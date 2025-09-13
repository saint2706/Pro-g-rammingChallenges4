import threading
import time

cap = 10
buffer = [-1 for i in range(cap)]
inIDX = 0
outIDX = 0

mutex = threading.Semaphore()
empty = threading.Semaphore(cap)
full = threading.Semaphore(0)


class Prod(threading.Thread):
    def run(self):
        global cap, buffer, inIDX, outIDX
        global mutex, empty, full

        items = 0
        cnt = 0

        while items < 20:
            empty.acquire()
            mutex.acquire()

            cnt += 1
            buffer[inIDX] = cnt
            inIDX = (inIDX + 1) % cap
            print("Produced: ", cnt)

            mutex.release()
            full.release()
            time.sleep(1)
            items += 1


class Cons(threading.Thread):
    def run(self):
        global cap, buffer, inIDX, outIDX
        global mutex, empty, full

        items = 0

        while items < 20:
            full.acquire()
            mutex.acquire()

            items += 1
            print("Consumed: ", buffer[outIDX])
            outIDX = (outIDX + 1) % cap

            mutex.release()
            empty.release()
            time.sleep(2.5)
            items += 1


producer = Prod()
consumer = Cons()

consumer.start()
producer.start()

producer.join()
consumer.join()
