/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <unistd.h>
#include <termios.h>
#include <sys/ioctl.h>

#define import_knames
#define import_spp
#include <iraf.h>

int ZTTYSZ ( XINT *dev, XINT *width, XINT *height )
{
    struct winsize buf;
    int ret;
    ret = ioctl( *dev, TIOCGWINSZ, &buf );
    if ( ret == 0 ) {
	*width = buf.ws_col;
	*height = buf.ws_row;
	return XOK;
    }
    *width = 0;
    *height = 0;
    return XERR;
}
