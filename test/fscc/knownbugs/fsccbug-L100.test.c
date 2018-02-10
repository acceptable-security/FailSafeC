#include <unistd.h>
#include "common.h"

TEST_CASE(fsccbug_L100)
{
  int s = 1;
  alarm(3);

  while(s){
    switch(1){
    case 1:
      switch (1) {
      default:
        s = 0;
        break;
      }
      break;
    }
  }
  TEST(1);
}
