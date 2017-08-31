/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_knames
#include <iraf.h>

/*
 * ZSHLIB.C -- This file contains dummy shared library descriptors to be linked
 * into executables which do not use the Sun/IRAF shared library.  See zzstrt.c
 * and the code in the directory unix/shlib for additional information on the
 * shared library facility.
 */
int	sh_debug = 0;
unsigned USHLIB[3] = { 0, 0, 0 };	/* actual length does not matter */
unsigned VSHLIB[3] = { 0, 0, 0 };
unsigned VSHEND;

void 
VLIBINIT (void){}
