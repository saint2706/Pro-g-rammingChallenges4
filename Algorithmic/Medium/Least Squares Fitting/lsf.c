#include <math.h>
#include <stdio.h>
#include <stdlib.h>

// generate array of 10 random integers between 1 to 100
void generateRandomArray(int *arr, int size) {
  int i;
  for (i = 0; i < size; i++)
    arr[i] = rand() % 100 + 1;
}

// function to calculate sum of array
int sum(int *arr, int size) {
  int i, sum = 0;
  for (i = 0; i < size; i++)
    sum += arr[i];
  return sum;
}

// function to generate array with values of x[i] * y[i] of two arrays
void generateXY(int *x, int *y, int *xy, int size) {
  int i;
  for (i = 0; i < size; i++)
    xy[i] = x[i] * y[i];
}

// function to generate array with square values
void generateSquare(int *arr, int *square, int size) {
  int i;
  for (i = 0; i < size; i++)
    square[i] = arr[i] * arr[i];
}

// return length of array
int length(int *arr) {
  int i = 0;
  while (arr[i] != '\0')
    i++;
  return i;
}

int main() {
  int x[10], y[10];
  int xy[10], xSquare[10];
  // generate random arrays x and y
  generateRandomArray(x, 10);
  generateRandomArray(y, 10);

  // make xy array
  generateXY(x, y, xy, 10);
  // make xSquare array
  generateSquare(x, xSquare, 10);

  int n = length(x);

  // calculate slope m
  int m = (n * sum(xy, n) - sum(x, n) * sum(y, n)) /
          (n * sum(xSquare, n) - sum(x, n) * sum(x, n));

  // calculate intercept c
  int c = (sum(y, n) - m * sum(x, n)) / n;

  printf("y = %dx + %d", m, c);

  return 0;
}