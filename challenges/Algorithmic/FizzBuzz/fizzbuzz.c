#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

/*
 * Modernized FizzBuzz (C Version)
 * ==============================
 *
 * Features:
 *  - Configurable upper limit via command-line argument (default 100)
 *  - Single responsibility fizzbuzz_value() helper
 *  - Clear, maintainable structure
 *  - Basic input validation and helpful usage message
 *
 * Build:
 *     cc -O2 -Wall -Wextra -pedantic -o fizzbuzz fizzbuzz.c
 *
 * Run:
 *     ./fizzbuzz         # default to 100
 *     ./fizzbuzz 50      # custom limit
 */

#define DEFAULT_LIMIT 100
#define FIZZ 3
#define BUZZ 5

static inline const char *fizzbuzz_value(int n, char *buffer, size_t buf_size)
{
    /*
     * Compute the FizzBuzz representation for n.
     * Writes into buffer when needed (number case) and returns a pointer
     * to a string literal or the provided buffer.
     */
    if (n % (FIZZ * BUZZ) == 0)
        return "FizzBuzz";
    if (n % FIZZ == 0)
        return "Fizz";
    if (n % BUZZ == 0)
        return "Buzz";
    snprintf(buffer, buf_size, "%d", n);
    return buffer;
}

static void run_fizzbuzz(int limit)
{
    char numbuf[16];
    for (int i = 1; i <= limit; ++i)
    {
        puts(fizzbuzz_value(i, numbuf, sizeof(numbuf)));
    }
}

static void print_usage(const char *prog)
{
    fprintf(stderr, "Usage: %s [limit]\n", prog);
    fprintf(stderr, "  limit: positive integer (default %d)\n", DEFAULT_LIMIT);
}

int main(int argc, char **argv)
{
    int limit = DEFAULT_LIMIT;

    if (argc > 2)
    {
        print_usage(argv[0]);
        return EXIT_FAILURE;
    }
    if (argc == 2)
    {
        char *end = NULL;
        long val = strtol(argv[1], &end, 10);
        if (*end != '\0' || val <= 0 || val > 100000000)
        { /* simple sanity bounds */
            fprintf(stderr, "Error: invalid limit '%s'\n", argv[1]);
            print_usage(argv[0]);
            return EXIT_FAILURE;
        }
        limit = (int)val;
    }

    run_fizzbuzz(limit);
    return EXIT_SUCCESS;
}