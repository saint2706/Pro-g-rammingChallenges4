#include <stdio.h>
#include <stdlib.h>

int mutex = 1;
int full = 0;
int empty = 10;
int x = 0;

void prod() {
  --mutex, ++full, --empty, x++;
  printf("Producer: %d\n", x);
  ++mutex;
}

void cons() {
  --mutex, --full, ++empty;
  printf("Consumer: %d\n", x);
  ++mutex, x--;
}

int main() {
  int n, i;
  printf("\n1. Press 1 for Producer\n2. Press 2 for Consumer\n3. Press 3 for "
         "Exit\n");

  for (i = 1; i > 0; i++) {
    printf("\nEnter your choice: ");
    scanf("%d", &n);

    switch (n) {
    case 1:
      if ((mutex == 1) && (empty != 0)) {
        prod();
      } else {
        printf("\nBuffer is full\n");
      }
      break;
    case 2:
      if ((mutex == 1) && (full != 0)) {
        cons();
      } else {
        printf("\nBuffer is empty\n");
      }
      break;
    case 3:
      exit(0);
      break;
    default:
      printf("\nInvalid choice\n");
      break;
    }
  }
}