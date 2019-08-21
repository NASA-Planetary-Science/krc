#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define CC_NAME(a)                                   a
#define FTN_NAME(a)                               a##_

void FTN_NAME(u_actinf) (int *con_tsec, int *con_tms, int *cpu_tsec,
                         int *cpu_tms, int *dir_io, int *pg_flts,
                         int *proc_swaps, int *ret)
{

  extern int CC_NAME(u_actinf) (int *con_tsec, int *con_tms, int *cpu_tsec,
                                int *cpu_tms, int *dir_io, int *pg_flts, 
                                int *proc_swaps, int *ret);

  (void) CC_NAME(u_actinf) (con_tsec, con_tms, cpu_tsec, cpu_tms,
                            dir_io, pg_flts, proc_swaps, ret);
  return;
}

