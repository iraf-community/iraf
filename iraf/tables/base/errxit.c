/* This routine takes an error exit on unix systems 
 * It is called by  exit_handler.x
 *
 *  Nelson Zarate	30-Nov-95	original
 */

#include <stdlib.h>

#define import_spp
#include <iraf.h>

/* Usual Fortran/C calling convention confusion */

#if defined (__hpux) || defined (_AIX) || defined(vms)
#define ERRXIT errxit
#else
#define ERRXIT errxit_
#endif

/* exit_code : i: pointer because of Fortran calling conventions */
void ERRXIT ( XINT *exit_code )
{ 
	exit (*exit_code);
}
