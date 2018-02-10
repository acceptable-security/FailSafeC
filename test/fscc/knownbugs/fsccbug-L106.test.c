#include <common.h>

#include <stdlib.h>
#include <string.h>

typedef struct dns_rbtnode {
	struct dns_rbtnode *parent;
} dns_rbtnode_t;


#define NAME(node) ((char *)(&(node)[1]))

dns_rbtnode_t *create_node(char *name, int namelen)
{
  dns_rbtnode_t *p = malloc(sizeof(*p) + namelen);
  p->parent = p;
  memcpy(NAME(p), name, namelen);
  return p;
}

TEST_CASE(fsccbug_L106)
{
  char name[128];
  int i;
  dns_rbtnode_t *p1, *p2;

  for (i = 0; i < 128; i++) name[i] = 'D';
  printf("STEP 1\n");
  p1 = create_node(name, 128);
  printf("STEP 2\n");
  p2 = create_node(NAME(p1)+2, 126);
  TEST(1);
}
