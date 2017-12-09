/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_ctype
#include <iraf.h>

#ifdef vms
globaldef  vms_ctype_defs;		/* [MACHDEP]	*/
#endif

/* Character class associations for the ctype.h macros.
*/
char u_ctype_[] = {
	0,
	_C,	_C,	_C,	_C,	_C,	_C,	_C,	_C,
	_C,	_S,	_S,	_S,	_S,	_S,	_C,	_C,
	_C,	_C,	_C,	_C,	_C,	_C,	_C,	_C,
	_C,	_C,	_C,	_C,	_C,	_C,	_C,	_C,
	_S,	_P,	_P,	_P,	_P,	_P,	_P,	_P,
	_P,	_P,	_P,	_P,	_P,	_P,	_P,	_P,
	_N,	_N,	_N,	_N,	_N,	_N,	_N,	_N,
	_N,	_N,	_P,	_P,	_P,	_P,	_P,	_P,
	_P,	_U|_X,	_U|_X,	_U|_X,	_U|_X,	_U|_X,	_U|_X,	_U,
	_U,	_U,	_U,	_U,	_U,	_U,	_U,	_U,
	_U,	_U,	_U,	_U,	_U,	_U,	_U,	_U,
	_U,	_U,	_U,	_P,	_P,	_P,	_P,	_P,
	_P,	_L|_X,	_L|_X,	_L|_X,	_L|_X,	_L|_X,	_L|_X,	_L,
	_L,	_L,	_L,	_L,	_L,	_L,	_L,	_L,
	_L,	_L,	_L,	_L,	_L,	_L,	_L,	_L,
	_L,	_L,	_L,	_P,	_P,	_P,	_P,	_C
};

int isalpha(int c)
{
	return isascii(c) && ((u_ctype_+1)[(unsigned int)(c)]&(_U|_L));
}

int isupper(int c)
{
	return isascii(c) && ((u_ctype_+1)[(unsigned int)(c)]&_U);
}

int islower(int c)
{
	return isascii(c) && ((u_ctype_+1)[(unsigned int)(c)]&_L);
}

int isdigit(int c)
{
	return isascii(c) && ((u_ctype_+1)[(unsigned int)(c)]&_N);
}

int isxdigit(int c)
{
	return isascii(c) && ((u_ctype_+1)[(unsigned int)(c)]&(_N|_X));
}

int isspace(int c)
{
	return isascii(c) && ((u_ctype_+1)[(unsigned int)(c)]&_S);
}

int ispunct(int c)
{
	return isascii(c) && ((u_ctype_+1)[(unsigned int)(c)]&_P);
}

int isalnum(int c)
{
	return isascii(c) && ((u_ctype_+1)[(unsigned int)(c)]&(_U|_L|_N));
}

int isprint(int c)
{
	return isascii(c) && ((u_ctype_+1)[(unsigned int)(c)]&(_P|_U|_L|_N));
}

int iscntrl(int c)
{
	return isascii(c) && ((u_ctype_+1)[(unsigned int)(c)]&_C);
}

