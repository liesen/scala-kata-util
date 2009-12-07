#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <stdint.h>
#include <string.h>

typedef struct {
  unsigned int *elements;
  size_t size;
} bitset;

unsigned int *bitset_elem(bitset *bits, uint64_t bit) {
  return &bits->elements[bit >> 5];
}

int bitset_mask(bitset *bits, uint64_t bit) {
  return 1 << (bit & 0x1f);
}

int bitset_isset(bitset *bits, uint64_t bit) {
  return (*bitset_elem(bits, bit) & bitset_mask(bits, bit)) != 0;
}

void bitset_set(bitset *bits, uint64_t bit) {
  *bitset_elem(bits, bit) |= bitset_mask(bits, bit); 
}

void bitset_unset(bitset *bits, uint64_t bit) {
  *bitset_elem(bits, bit) &= ~bitset_mask(bits, bit); 
}

void writeVarint64(uint64_t value, FILE *stream) {
  for (;;) {
    if ((value & ~0x7fUL) == 0) {
      fputc((int) value, stream);
      return;
    } else {
      fputc(((int) value & 0x7f) | 0x80, stream);
      value >>= 7;
    }
  }
}

int main(int args, char **argv) {
  size_t bitset_size = 1 << (30 - 5);
  uint64_t max_prime = bitset_size << 5; 

  fprintf(stderr, "Upper limit: %d\n", max_prime);

  bitset bits;
  bits.size = bitset_size;
  bits.elements = calloc(sizeof(unsigned int), bits.size);

  // Set all multiples of two to be non-primes and the rest
  // to be primes
  memset(bits.elements, 2 + 8 + 32 + 128, bits.size * sizeof(unsigned int));

  bitset *primes = &bits;
  bitset_unset(primes, 1);
  bitset_set(primes, 2);

  uint64_t p = 3;

  while (p < max_prime) {
    // Filter out all multiples of p
    for (uint32_t i = p + p; i < max_prime; i += p) {
      bitset_unset(primes, i);
    }

    // Find next number to filter out
    do {
      p += 2;
    } while (p < max_prime && !bitset_isset(primes, p));
  }

  // Write to file
  /*
  FILE *fp = fopen("test2.raw", "w");

  for (p = 2; p < max_prime; p++) {
    if (bitset_isset(primes, p)) {
      writeVarint64(p, fp);
    }
  }

  fflush(fp);
  fclose(fp);
  fprintf(stderr, "wrote varints\n");
  */

  FILE *fp = fopen("primes.bin", "w");
  fwrite(bits.elements, sizeof(unsigned int), bits.size, fp);
  fflush(fp);
  fclose(fp);

  fprintf(stderr, "Largest prime: %d\n", p - 2);
  fprintf(stderr, "Wrote bitset: primes.bin\n");
  free(bits.elements);
}
