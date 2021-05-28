#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

void debug( int *a, size_t length ) {
  int i;
  for (i=0;i < length;i++) {
    printf("%d ",a[i]);
  }
};



int main(void) {
  int i, size;
  int *v;
  int *c;
  int *buf;

  scanf("%d", &size);

  v = malloc(size * sizeof(int));  // Vals
  for(i=0; i < size; i++)
    scanf("%d", &v[i]); 

  // Initialize counts to one, the trivial redundant RLE
  c = malloc(size * sizeof(int)); // Run lengths
  for(i=0; i < size; i++) {
    c[i] = 1;
  } 

  int bufSize = 2 * size;
  buf = malloc(bufSize*sizeof(int)); // Printable buffer of departures
  int bufi = 0;
  int r = 0; // Time taken/number of rounds
  bool departures;
  int j;
  do {
    r++;
    departures = false; // Departures bool to determine fixed point
    j = 0;
    for(i=0; i < size; i++) {
      
      if(c[i] > 0){
        if(v[i] != v[j]){
          j++;
          v[j] = v[i];
          c[j] = c[i];
        }
        else if(i != j){
          c[j] += c[i];
        }
      }
    }

    size = j+1;
    // Departures pass
    for(i=1; i < size; i++) {
      
      // current candidate is better than prev
      if(v[i] > v[i-1]){
        // prev hasn't been eliminated
        if(c[i-1] > 0){
          c[i-1]--; // departure from prev group
          buf[bufi] = v[i-1]; // record departure for printing
          bufi++;
          departures = true; // Not a fixed point yet.
        }      
      }
      // previous candidate is better than current
      /* else if(v[i] < v[i-1]){ */
      else {
        // current candidates exist
        if(c[i] > 0){
          c[i]--; // departure from current group
          /* printf("%d ", v[i]); */
          buf[bufi] = v[i]; // Record departure
          bufi++;
          departures = true; // No fixed point yet
        }
      }
    }
    buf[bufi] = -2; // Newline sentinel
    bufi++;
  } while (departures);
  
  printf("%d\n",r-1);

  for (i=0; i < bufi; i++){
    if (buf[i]==-2){
      if (buf[i+1]!=-1) {
        printf("\n");
      }
    }
    else {
      printf("%d ", buf[i]);
    }        
  }
  for (i=0; i < size; i++) {
    int m;
    for (m=0; m < c[i]; m++) {
      printf("%d ",v[i]);
    }
  }
  printf("\n");
  // Drop memory on the floor
  /* free(v); */
  /* free(c); */
  return 0;
};

