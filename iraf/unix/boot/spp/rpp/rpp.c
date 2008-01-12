/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include "ratlibc/ratdef.h"

extern void RATFOR( void );
/* ratlibc/initst.c */
extern void INITST( void );
/* ratlibc/endst.c */
extern void ENDST( void );

int	xargc;
char	**xargv;

/* RPP -- Second pass of the SPP preprocessor.  Converts a Ratfor like
 * input language into Fortran.  RPP differs from standard tools ratfor
 * in a number of ways.  Its input language is the output of XPP and
 * contains tokens not intended for use in any programming language.
 * Support is provided for SPP language features, and the output fortran
 * is pretty-printed.
 */
int main ( int argc, char **argv )
{
	xargc = argc;
	xargv = argv;

	INITST();
	RATFOR();
	ENDST();

	return 0;
}
