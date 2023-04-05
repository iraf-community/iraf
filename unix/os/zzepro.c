#include <stdio.h>
#include <signal.h>

#define import_spp
#define import_knames
#include <iraf.h>

#include "osproto.h"

/*
 * ZZEPRO.C -- Code which is executed at the end of every procedure.
 */

int ZZEPRO ( void) { return (XOK); }
