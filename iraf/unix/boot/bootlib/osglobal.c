
#define import_spp
#include <iraf.h>

#include "bootlib.h"

int	bdebug = 0;			/* print debug stuff		*/
int	osfiletype;			/* type of single output file	*/
XCHAR	text[SZ_FBUF];			/* output text line if textfile	*/
XCHAR	*txop;				/* next char in output buf	*/
