#include <stdio.h>
#include <signal.h>

#define import_spp
#define import_knames
#include <iraf.h>



void ex_handler ( int, siginfo_t *, void * );


/*
 * ZZEPRO.C -- Code which is executed at the end of every procedure.
 */

int ZZEPRO ( void) { return (XOK); }
