/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include "ratdef.h"

CLOSE(fd)
FINT	*fd;
{
	fclose(_fdtofile[*fd]);
}
