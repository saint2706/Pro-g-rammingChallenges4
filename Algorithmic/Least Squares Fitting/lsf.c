/*
 * Least Squares Linear Regression (modernized C version)
 * ------------------------------------------------------
 * Generates a synthetic dataset (x,y) and computes the best-fit line y = m x + b
 * using ordinary least squares. Demonstrates clear separation of concerns,
 * double precision arithmetic, and simple CLI configuration.
 *
 * Usage:
 *   ./lsf                 # default 20 points with random y in [0,100]
 *   ./lsf -n 50 -s 123    # 50 points with a fixed random seed
 *   ./lsf -n 40 -o output.txt
 *
 * Options:
 *   -n <points>   Number of data points (>=2, default 20)
 *   -s <seed>     RNG seed (unsigned int). If omitted, srand(time(NULL)).
 *   -o <file>     Write resulting line and all points to a file
 *   -h            Show help
 *
 * The x values are generated uniformly in [0,1]; y = true_m * x + true_b + noise
 * where noise is uniform in [-0.5,0.5]. A simple model avoids external libs.
 */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

typedef struct
{
  double m; /* slope */
  double b; /* intercept */
} RegressionResult;

static void usage(const char *prog)
{
  fprintf(stderr,
          "Usage: %s [-n points] [-s seed] [-o out.txt]\n"
          "Options:\n"
          "  -n points  Number of points (>=2, default 20)\n"
          "  -s seed    RNG seed (unsigned)\n"
          "  -o file    Output file (append data & result)\n"
          "  -h         Help\n",
          prog);
}

/* Compute regression line for arrays x,y of length n. */
static int linear_regression(const double *x, const double *y, size_t n, RegressionResult *out)
{
  if (!x || !y || !out || n < 2)
    return -1;
  double sum_x = 0.0, sum_y = 0.0, sum_xy = 0.0, sum_x2 = 0.0;
  for (size_t i = 0; i < n; ++i)
  {
    sum_x += x[i];
    sum_y += y[i];
    sum_xy += x[i] * y[i];
    sum_x2 += x[i] * x[i];
  }
  double denom = n * sum_x2 - sum_x * sum_x;
  if (fabs(denom) < 1e-15)
  {
    return -2; /* vertical line */
  }
  out->m = (n * sum_xy - sum_x * sum_y) / denom;
  out->b = (sum_y - out->m * sum_x) / (double)n;
  return 0;
}

int main(int argc, char **argv)
{
  size_t n = 20;         /* default number of points */
  unsigned int seed = 0; /* 0 means auto-seed */
  int have_seed = 0;
  const char *out_path = NULL;

  for (int i = 1; i < argc; ++i)
  {
    if (strcmp(argv[i], "-h") == 0)
    {
      usage(argv[0]);
      return 0;
    }
    else if (strcmp(argv[i], "-n") == 0 && i + 1 < argc)
    {
      n = (size_t)strtoul(argv[++i], NULL, 10);
    }
    else if (strcmp(argv[i], "-s") == 0 && i + 1 < argc)
    {
      seed = (unsigned int)strtoul(argv[++i], NULL, 10);
      have_seed = 1;
    }
    else if (strcmp(argv[i], "-o") == 0 && i + 1 < argc)
    {
      out_path = argv[++i];
    }
    else
    {
      fprintf(stderr, "Unknown or incomplete option '%s'\n", argv[i]);
      usage(argv[0]);
      return 1;
    }
  }

  if (n < 2)
  {
    fprintf(stderr, "Number of points must be >= 2 (got %zu)\n", n);
    return 1;
  }

  if (!have_seed)
  {
    seed = (unsigned int)time(NULL);
  }
  srand(seed);

  double *x = (double *)malloc(n * sizeof(double));
  double *y = (double *)malloc(n * sizeof(double));
  if (!x || !y)
  {
    fprintf(stderr, "Allocation failure\n");
    free(x);
    free(y);
    return 1;
  }

  /* True underlying line (hidden from the regression). */
  const double true_m = 0.75;
  const double true_b = 10.0;

  for (size_t i = 0; i < n; ++i)
  {
    x[i] = (double)i / (double)(n - 1);                       /* uniform in [0,1] */
    double noise = ((double)rand() / (double)RAND_MAX) - 0.5; /* [-0.5, 0.5] */
    y[i] = true_m * x[i] + true_b + noise;
  }

  RegressionResult result;
  int rc = linear_regression(x, y, n, &result);
  if (rc == -2)
  {
    fprintf(stderr, "Degenerate data set (vertical line).\n");
    free(x);
    free(y);
    return 2;
  }
  else if (rc != 0)
  {
    fprintf(stderr, "Regression failed (code %d).\n", rc);
    free(x);
    free(y);
    return 3;
  }

  printf("Fitted line: y = %.6f x + %.6f\n", result.m, result.b);
  printf("(True line was y = %.2f x + %.2f)\n", true_m, true_b);
  printf("Seed: %u, Points: %zu\n", seed, n);

  if (out_path)
  {
    FILE *f = fopen(out_path, "w");
    if (!f)
    {
      fprintf(stderr, "Could not open output file '%s'\n", out_path);
    }
    else
    {
      fprintf(f, "# x y\n");
      for (size_t i = 0; i < n; ++i)
      {
        fprintf(f, "%f %f\n", x[i], y[i]);
      }
      fprintf(f, "# Fitted y = %.6f x + %.6f\n", result.m, result.b);
      fclose(f);
      printf("Wrote data + fit to %s\n", out_path);
    }
  }

  free(x);
  free(y);
  return 0;
}