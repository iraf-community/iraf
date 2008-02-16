# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <ctype.h>

# IMDRCUR -- Read the logical image cursor of the named image display device.
# opened with IMDOPEN).  This is a high level cursor read, returning image
# pixel coordinates and relying upon the display server to use the keyboard or
# mouse to terminate the cursor read.  Nonblocking reads and frame buffer
# coordinates are available as options.  The user is expected to select the
# frame for which coordintes are to be returned; the frame number is returned
# in the encoded WCS.  The cursor key is returned as the function value.

int procedure imdrcur (device, x, y, wcs, key, strval, maxch, in_wcs, pause)

char    device[ARB]             #I image display device
real    x, y                    #O cursor coords given WCS
int     wcs                     #O WCS of coordinates (frame*100+in_wcs)
int     key                     #O keystroke which triggered cursor read
char    strval[maxch]           #O optional string value
int     maxch                   #I max chars out
int     in_wcs                  #I desired wcs: 0=frame, 1=image
int     pause                   #I blocking cursor read? (YES|NO)

char    ch
int     fd, op
pointer sp, curval, devname, tty, dd, ip

bool    streq()
pointer ttygdes()
int     imdopen(), ttygets(), envgets(), nscan(), stg_getline()

string  eof "EOF\n"
string  stdimage "stdimage"
errchk  ttygdes, imdopen, imdrcuro

begin
        call smark (sp)
        call salloc (devname, SZ_FNAME, TY_CHAR)
        call salloc (curval, SZ_LINE, TY_CHAR)
        call salloc (dd, SZ_LINE, TY_CHAR)

        # Get the logical device name.
        if (streq (device, stdimage)) {
            if (envgets (stdimage, Memc[devname], SZ_FNAME) <= 0)
                call strcpy (device, Memc[devname], SZ_FNAME)
        } else
            call strcpy (device, Memc[devname], SZ_FNAME)

        # Get the DD kernel driver string for the device.
        tty = ttygdes (Memc[devname])
        if (ttygets (tty, "DD", Memc[dd], SZ_LINE) <= 0)
            call strcpy (Memc[devname], Memc[dd], SZ_FNAME)

        # Open the device and read the logical image cursor.
        fd = imdopen (Memc[dd], READ_WRITE)
        call imdrcuro (tty, Memc[curval], SZ_LINE, in_wcs, pause)

        # Decode the formatted cursor value string.
        if (streq (Memc[curval], eof)) {
            key = EOF
        } else {
            call sscan (Memc[curval])
                call gargr (x)
                call gargr (y)
                call gargi (wcs)
                call gargc (ch)
		call gargstr (Memc[curval], SZ_LINE)

            key = ch
            if (nscan() < 4)
                key = ERR

	    ip = curval
	    if (nscan() < 5)
		Memc[curval] = EOS
	    else {
		while (IS_WHITE(Memc[ip]) || Memc[ip] == '\n')
		    ip = ip + 1
	    }
        }

        # In this implementation, string input for colon commands is via the
        # terminal to avoid the complexities of character i/o to the display.
	# Note that the lower level code can return the string value if it
	# chooses to (must be a nonnull string).

        strval[1] = EOS
        if (key == ':') {
	    # String value not already set by imdrcuro?
	    if (Memc[ip] == EOS) {
		call stg_putline (STDOUT, ":")
		if (stg_getline (STDIN, Memc[curval]) == EOF)
		    Memc[curval] = EOS
		else
		    for (ip=curval;  IS_WHITE (Memc[ip]);  ip=ip+1)
			;
	    }

	    # Copy to the output string argument.
	    op = 1
	    while (Memc[ip] != '\n' && Memc[ip] != EOS) {
		strval[op] = Memc[ip]
		op = min (op + 1, maxch)
		ip = ip + 1
	    }
	    strval[op] = EOS
        }

        # Map ctrl/d and ctrl/z onto EOF.
        if (key == '\004' || key == '\032')
            key = EOF

        call close (fd)
        call ttycdes (tty)

        return (key)
end
