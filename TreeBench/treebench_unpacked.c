#include <stdio.h>
#include <stdlib.h>
#include <time.h>

enum Tree {
    Leaf,
    Node,
};

// Manual layout:
// one byte for each tag, 64 bit integers
typedef char* TreeRef;
typedef long long Num;

// Helper function
char* fillTree(char* cursor, int n, Num root) {
  // printf("  filltree: %p, n=%d, fill=%lld", cursor, n, root); fflush(stdout);
  if (n == 0) {
    *cursor = Leaf;
    cursor++;
    *((Num*)cursor) = root; // Unaligned!
    // printf("; wrote tag %d, payload %lld\n", Leaf, root); fflush(stdout);
    return (cursor + sizeof(Num));
  } else {
    *cursor = Node;
    // printf("; wrote tag %d\n", Node); fflush(stdout);
    char* cur2 = fillTree(cursor+1, n-1, root);
    return fillTree(cur2, n-1, root + (1<<(n-1)));
  }
}

int treeSize(int n) {
  int leaves = 1 << n;
  int nodes  = leaves - 1;
  // Both nodes and leaves are tagged:
  int bytes  = sizeof(Num)*leaves + sizeof(char)*(nodes+leaves);
  /* printf("treeSize(%d): %d bytes (%d/%d nodes/leaves)\n", */
  /*        n, bytes, nodes, leaves); */
  return bytes;
}

TreeRef buildTree(int n) {
  int bytes = treeSize(n);
  char* buf = malloc(bytes);
  char* res = fillTree(buf, n, 1);
  printf("wrote %d\n", (int)(res - buf));  
  return buf;
}

TreeRef printTree(TreeRef t) {
  if (*t == Leaf) {
    t++;
    printf("%lld", *(Num*)t);
    return (t+sizeof(Num));
  } else {
    t++;
    printf("(");
    TreeRef t2 = printTree(t);
    printf(",");
    TreeRef t3 = printTree(t2);
    printf(")");
    return t3;    
  }
}

TreeRef add1Tree(TreeRef t, TreeRef tout) {
  if (*t == Leaf) {
    *tout = Leaf;    
    t++; tout++;
    *(Num*)tout = *(Num*)t + 1;
    return (t+sizeof(Num));
  } else {
    *tout = Node;
    t++; tout++;
    TreeRef t2 = add1Tree(t,tout);
    tout += (t2 - t);
    return add1Tree(t2,tout);
  }
}

void main() {
  int depth = 20;
  printf("Building tree, depth %d\n", depth);
  // CLOCK_REALTIME
  clock_t begin = clock();
  TreeRef tr = buildTree(depth);
  clock_t end = clock();
  double time_spent = (double)(end - begin) / CLOCKS_PER_SEC;
  printf("done building, took %lf seconds\n\n", time_spent);
  // printTree(tr); printf("\n");
  TreeRef t2 = malloc(treeSize(depth));
  for(int i=0; i<10; i++) {
    begin = clock();
    add1Tree(tr,t2);
    end = clock();
    time_spent = (double)(end - begin) / CLOCKS_PER_SEC;
    printf("  run(%d): %lf\n", i, time_spent);
  }  
  // printTree(t2); printf("\n");
  free(tr);
}
