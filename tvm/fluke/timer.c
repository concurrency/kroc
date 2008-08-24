#include "fluke-tvm.h"

WORD arm7tdmi_get_time(ECTX ectx)
{
  return getSysTICs();
}

void sleep_until(WORD timeout)
{
}

void sleep(void)
{
}
