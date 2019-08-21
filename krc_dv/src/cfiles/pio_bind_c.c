#include <stdio.h>
#include <string.h>
#include "isistypes.h"
#include "binding.h"
#include "primio.h"
#if defined(VMS)
#include <descrip.h>
#endif


#if defined(VMS)
void FTN_NAME(pio_in) (INT4 *fid, struct dcs$descriptor *vms_string, 
                       INT4 *nbytes, INT4 *mode, INT4 *ret)
#else
void FTN_NAME(pio_in) (INT4 *fid, CHAR *flspec, INT4 *nbytes, INT4 *mode, 
                       INT4 *ret, INT4 flspec_len)
#endif
{
   CHAR fname[260];
   register INT4 i;

#if defined(VMS)
   INT4 flspec_len = vms_string->dcs$w_length;
   CHAR *flspec =   vms_string->dcs$a_pointer;
#endif

   for (i = flspec_len-1 ; i >= 0 ; i--)
     if (flspec[i] != ' ') break;

   (void) strncpy(fname, flspec, i+1);
   fname[i+1] = '\0';

   (void) CC_NAME(pio_in)(fid, fname, nbytes, *mode, ret);
   return;
}



void FTN_NAME(pio_cl)(INT4 *fid, INT4 *idele, INT4 *ret)
{

  (void) CC_NAME(pio_cl)(*fid, *idele, ret);
  return;
}



void FTN_NAME(pio_rd)(INT4 *fid, INT4 *ibyte, INT4 *nbytes, void *buf, INT4 *ret)
{

  (void) CC_NAME(pio_rd)(*fid, *ibyte, *nbytes, buf, ret);
  return;
}



void FTN_NAME(pio_wt)(INT4 *fid, INT4 *ibyte, INT4 *nbytes, void *buf, INT4 *ret)
{

  (void) CC_NAME(pio_wt)(*fid, *ibyte, *nbytes, buf, ret);
  return;
}



void FTN_NAME(pio_ap)(INT4 *fid, INT4 *nbytes, INT4 *ntbytes, INT4 *ret)
{

  (void) CC_NAME(pio_ap)(*fid, *nbytes, ntbytes, ret);
  return;
}

void FTN_NAME(pio_reset_access)(INT4 *fid, INT4 *access, INT4 *ret)
{

  (void) CC_NAME(pio_reset_access)(*fid, *access, ret);
  return;
}


void FTN_NAME(pio_truncate)(INT4 *fid, INT4 *nbytes, INT4 *ret)
{

  (void) CC_NAME(pio_truncate)(*fid, *nbytes, ret);
  return;
}
