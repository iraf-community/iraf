#include "ratdef.h"

CLOSE(fd)
FINT	*fd;
{
	fclose(_fdtofile[*fd]);
}
