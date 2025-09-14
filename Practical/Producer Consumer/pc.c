/*
 * pc.c - Cross‑platform bounded buffer producer/consumer demo (modernized)
 *
 * Features:
 *   - Threads + mutex + condition variables (POSIX pthreads or Win32 primitives)
 *   - Configurable buffer size, total items, producer & consumer counts (CLI)
 *   - Graceful shutdown: consumers exit after all items consumed
 *   - No busy waiting; proper signalling
 *   - Single source builds on Linux/macOS/WSL (pthreads) and Windows (Win32)
 *
 * Build (POSIX):
 *   gcc -O2 -Wall -Wextra -pthread -o pc pc.c
 *
 * Build (Windows, MSVC cl):
 *   cl /O2 /W4 pc.c /Fe:pc.exe
 *
 * Examples:
 *   ./pc
 *   ./pc -b 32 -n 100 -P 2 -C 3
 *   pc.exe -b 20 -n 200 -P 4 -C 4
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <process.h>
#else
#include <pthread.h>
#include <unistd.h>
#endif

/* ---------------- Configuration ---------------- */

typedef struct Config
{
  size_t buffer_size; /* capacity */
  size_t total_items; /* total items to produce across all producers */
  int producers;      /* number of producer threads */
  int consumers;      /* number of consumer threads */
} Config;
static void usage(const char *prog)
{
  fprintf(stderr,
          "Usage: %s [-b buffer_size] [-n total_items] [-P producers] [-C consumers]\n",
          prog);
}

static Config parse_args(int argc, char **argv)
{
  Config cfg = {10, 50, 1, 1};
  for (int i = 1; i < argc; ++i)
  {
    if ((strcmp(argv[i], "-b") == 0 || strcmp(argv[i], "--buffer") == 0) && i + 1 < argc)
    {
      cfg.buffer_size = (size_t)strtoul(argv[++i], NULL, 10);
    }
    else if ((strcmp(argv[i], "-n") == 0 || strcmp(argv[i], "--items") == 0) && i + 1 < argc)
    {
      cfg.total_items = (size_t)strtoul(argv[++i], NULL, 10);
    }
    else if ((strcmp(argv[i], "-P") == 0 || strcmp(argv[i], "--producers") == 0) && i + 1 < argc)
    {
      cfg.producers = atoi(argv[++i]);
    }
    else if ((strcmp(argv[i], "-C") == 0 || strcmp(argv[i], "--consumers") == 0) && i + 1 < argc)
    {
      cfg.consumers = atoi(argv[++i]);
    }
    else if (strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "--help") == 0)
    {
      usage(argv[0]);
      exit(0);
    }
    else
    {
      fprintf(stderr, "Unknown argument: %s\n", argv[i]);
      usage(argv[0]);
      exit(2);
    }
  }
  if (!cfg.buffer_size || !cfg.total_items || cfg.producers < 1 || cfg.consumers < 1)
  {
    fprintf(stderr, "Invalid configuration values.\n");
    exit(2);
  }
  return cfg;
}

/* ---------------- Bounded Buffer ---------------- */

/* Cross‑platform synchronization layer */
#ifdef _WIN32
typedef CRITICAL_SECTION mutex_t;
typedef CONDITION_VARIABLE cond_t;
static void mutex_init(mutex_t *m) { InitializeCriticalSection(m); }
static void mutex_destroy(mutex_t *m) { (void)m; /* no-op */ }
static void mutex_lock(mutex_t *m) { EnterCriticalSection(m); }
static void mutex_unlock(mutex_t *m) { LeaveCriticalSection(m); }
static void cond_init(cond_t *c) { InitializeConditionVariable(c); }
static void cond_destroy(cond_t *c) { (void)c; }
static void cond_signal(cond_t *c) { WakeConditionVariable(c); }
static void cond_broadcast(cond_t *c) { WakeAllConditionVariable(c); }
static void cond_wait(cond_t *c, mutex_t *m) { SleepConditionVariableCS(c, m, INFINITE); }
#else
typedef pthread_mutex_t mutex_t;
typedef pthread_cond_t cond_t;
static void mutex_init(mutex_t *m)
{
  if (pthread_mutex_init(m, NULL))
  {
    perror("pthread_mutex_init");
    exit(1);
  }
}
static void mutex_destroy(mutex_t *m) { pthread_mutex_destroy(m); }
static void mutex_lock(mutex_t *m) { pthread_mutex_lock(m); }
static void mutex_unlock(mutex_t *m) { pthread_mutex_unlock(m); }
static void cond_init(cond_t *c)
{
  if (pthread_cond_init(c, NULL))
  {
    perror("pthread_cond_init");
    exit(1);
  }
}
static void cond_destroy(cond_t *c) { pthread_cond_destroy(c); }
static void cond_signal(cond_t *c) { pthread_cond_signal(c); }
static void cond_broadcast(cond_t *c) { pthread_cond_broadcast(c); }
static void cond_wait(cond_t *c, mutex_t *m) { pthread_cond_wait(c, m); }
#endif

typedef struct Buffer
{
  int *data;
  size_t capacity;
  size_t head;
  size_t tail;
  size_t count;
  size_t produced_total;
  mutex_t mtx;
  cond_t not_full;
  cond_t not_empty;
  Config cfg;
} Buffer;

static void buffer_init(Buffer *b, Config cfg)
{
  b->data = (int *)calloc(cfg.buffer_size, sizeof(int));
  if (!b->data)
  {
    perror("calloc");
    exit(1);
  }
  b->capacity = cfg.buffer_size;
  b->head = b->tail = b->count = 0;
  b->produced_total = 0;
  b->cfg = cfg;
  mutex_init(&b->mtx);
  cond_init(&b->not_full);
  cond_init(&b->not_empty);
}
static void buffer_destroy(Buffer *b)
{
  mutex_destroy(&b->mtx);
  cond_destroy(&b->not_full);
  cond_destroy(&b->not_empty);
  free(b->data);
}

/* Push item; returns 0 if produced, 1 if production is complete */
static int buffer_produce(Buffer *b, int value)
{
  mutex_lock(&b->mtx);
  while (b->count == b->capacity && b->produced_total < b->cfg.total_items)
    cond_wait(&b->not_full, &b->mtx);
  if (b->produced_total >= b->cfg.total_items)
  {
    mutex_unlock(&b->mtx);
    cond_broadcast(&b->not_empty);
    return 1;
  }
  b->data[b->tail] = value;
  b->tail = (b->tail + 1) % b->capacity;
  b->count++;
  b->produced_total++;
  cond_signal(&b->not_empty);
  mutex_unlock(&b->mtx);
  return 0;
}

/* Pop item; returns 0 if consumed, 1 if no more items & production complete */
static int buffer_consume(Buffer *b, int *out)
{
  mutex_lock(&b->mtx);
  while (b->count == 0 && b->produced_total < b->cfg.total_items)
    cond_wait(&b->not_empty, &b->mtx);
  if (b->count == 0 && b->produced_total >= b->cfg.total_items)
  {
    mutex_unlock(&b->mtx);
    cond_broadcast(&b->not_full);
    return 1;
  }
  *out = b->data[b->head];
  b->head = (b->head + 1) % b->capacity;
  b->count--;
  cond_signal(&b->not_full);
  mutex_unlock(&b->mtx);
  return 0;
}

/* ---------------- Worker Threads ---------------- */

typedef struct ThreadArgs
{
  Buffer *buf;
  int id;
} ThreadArgs;

#ifdef _WIN32
static unsigned __stdcall producer_thread(void *arg)
{
  ThreadArgs *ta = (ThreadArgs *)arg;
  int val = 0;
  while (1)
  {
    if (buffer_produce(ta->buf, val))
      break;
    if ((val % 1000) == 0)
      printf("Producer %d produced %d\n", ta->id, val);
    val++;
  }
  return 0;
}
static unsigned __stdcall consumer_thread(void *arg)
{
  ThreadArgs *ta = (ThreadArgs *)arg;
  int v;
  while (1)
  {
    if (buffer_consume(ta->buf, &v))
      break;
    if ((v % 1000) == 0)
      printf("Consumer %d consumed %d\n", ta->id, v);
  }
  return 0;
}
#else
static void *producer_thread(void *arg)
{
  ThreadArgs *ta = (ThreadArgs *)arg;
  int val = 0;
  while (1)
  {
    if (buffer_produce(ta->buf, val))
      break;
    if ((val % 1000) == 0)
      printf("Producer %d produced %d\n", ta->id, val);
    val++;
  }
  return NULL;
}
static void *consumer_thread(void *arg)
{
  ThreadArgs *ta = (ThreadArgs *)arg;
  int v;
  while (1)
  {
    if (buffer_consume(ta->buf, &v))
      break;
    if ((v % 1000) == 0)
      printf("Consumer %d consumed %d\n", ta->id, v);
  }
  return NULL;
}
#endif

/* ---------------- Main ---------------- */

int main(int argc, char **argv)
{
  Config cfg = parse_args(argc, argv);
  Buffer buf;
  buffer_init(&buf, cfg);
  ThreadArgs *pargs = (ThreadArgs *)calloc(cfg.producers, sizeof(ThreadArgs));
  ThreadArgs *cargs = (ThreadArgs *)calloc(cfg.consumers, sizeof(ThreadArgs));
  if (!pargs || !cargs)
  {
    perror("calloc");
    exit(1);
  }

#ifdef _WIN32
  HANDLE *prod_handles = (HANDLE *)calloc(cfg.producers, sizeof(HANDLE));
  HANDLE *cons_handles = (HANDLE *)calloc(cfg.consumers, sizeof(HANDLE));
  if (!prod_handles || !cons_handles)
  {
    perror("calloc");
    exit(1);
  }
  for (int i = 0; i < cfg.producers; ++i)
  {
    pargs[i].buf = &buf;
    pargs[i].id = i;
    prod_handles[i] = (HANDLE)_beginthreadex(NULL, 0, producer_thread, &pargs[i], 0, NULL);
  }
  for (int i = 0; i < cfg.consumers; ++i)
  {
    cargs[i].buf = &buf;
    cargs[i].id = i;
    cons_handles[i] = (HANDLE)_beginthreadex(NULL, 0, consumer_thread, &cargs[i], 0, NULL);
  }
  WaitForMultipleObjects(cfg.producers, prod_handles, TRUE, INFINITE);
  WaitForMultipleObjects(cfg.consumers, cons_handles, TRUE, INFINITE);
  for (int i = 0; i < cfg.producers; ++i)
    CloseHandle(prod_handles[i]);
  for (int i = 0; i < cfg.consumers; ++i)
    CloseHandle(cons_handles[i]);
  free(prod_handles);
  free(cons_handles);
#else
  pthread_t *prod_threads = (pthread_t *)calloc(cfg.producers, sizeof(pthread_t));
  pthread_t *cons_threads = (pthread_t *)calloc(cfg.consumers, sizeof(pthread_t));
  if (!prod_threads || !cons_threads)
  {
    perror("calloc");
    exit(1);
  }
  for (int i = 0; i < cfg.producers; ++i)
  {
    pargs[i].buf = &buf;
    pargs[i].id = i;
    if (pthread_create(&prod_threads[i], NULL, producer_thread, &pargs[i]))
    {
      perror("pthread_create prod");
      exit(1);
    }
  }
  for (int i = 0; i < cfg.consumers; ++i)
  {
    cargs[i].buf = &buf;
    cargs[i].id = i;
    if (pthread_create(&cons_threads[i], NULL, consumer_thread, &cargs[i]))
    {
      perror("pthread_create cons");
      exit(1);
    }
  }
  for (int i = 0; i < cfg.producers; ++i)
    pthread_join(prod_threads[i], NULL);
  for (int i = 0; i < cfg.consumers; ++i)
    pthread_join(cons_threads[i], NULL);
  free(prod_threads);
  free(cons_threads);
#endif
  buffer_destroy(&buf);
  free(pargs);
  free(cargs);
  return 0;
}