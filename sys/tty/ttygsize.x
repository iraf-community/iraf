# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ttyset.h>
include	<ttset.h>
include	<fset.h>

define	TIMEOUT  2000		# timeout interval, msec, for size query


# TTYGSIZE -- Get the width and height of the terminal screen in characters.
# For a conventional terminal these values are given as constants in the
# termcap entry for the device.  If the terminal has a screen which can vary
# in size at runtime (e.g., a window on a workstation), then the screen size
# is queried at runtime.

procedure ttygsize (in, out, tty, width, height)

int	in, out			# input and output streams
pointer	tty			# terminal descriptor
int	width			# ncols on screen (out)
int	height			# nlines on screen (out)

pointer	sp, patbuf, buf, qs, wh, ip, op
int	index, len_qs, len_wh, w_index, h_index, sv_iomode, nchars, junk

int	patmake(), patindex(), gstrcpy(), ctoi()
int	ttygets(), ttyread(), ttystati(), ttstati(), fstati()
define	noquery_ 91
errchk	ttyread

begin
	call smark (sp)
	call salloc (patbuf, SZ_LINE, TY_CHAR)
	call salloc (buf, SZ_LINE, TY_CHAR)
	call salloc (qs, SZ_FNAME, TY_CHAR)
	call salloc (wh, SZ_FNAME, TY_CHAR)

	width = 0
	height = 0
	index = 0

	# Just use the termcap values if in stty playback or record mode.
	if (ttstati(in,TT_LOGIN) == YES || ttstati(in,TT_PLAYBACK) == YES)
	    goto noquery_

	len_qs = ttygets (tty, "qs", Memc[qs], SZ_FNAME)
	len_wh = ttygets (tty, "wh", Memc[wh], SZ_FNAME)

	# Process the string DS (decode size string) to map the %W %H fields
	# into the pattern strings "%[0-9]*", noting the index positions of
	# the W and H fields.

	if (len_wh > 0) {
	    op = buf
	    for (ip=wh;  Memc[ip] != EOS;  ip=ip+1) {
		if ((Memc[ip] == '%') && ip > wh && Memc[ip-1] != '\\' &&
		    (Memc[ip+1] == 'W' || Memc[ip+1] == 'H')) {

		    index = index + 1
		    op = op + gstrcpy ("%[0-9]*", Memc[op], ARB)
		    ip = ip + 1

		    if (Memc[ip] == 'W')
			w_index = index
		    else
			h_index = index
		} else {
		    Memc[op] = Memc[ip]
		    op = op + 1
		}
	    }
	    Memc[op] = EOS
	    junk = patmake (Memc[buf], Memc[patbuf], SZ_LINE)
	}

	# Query the terminal for the screen size, read back and decode the
	# encoded screen size string.

	if (len_qs > 0 && len_wh > 0) {
	    sv_iomode = fstati (in, F_IOMODE)
	    if (sv_iomode != IO_RAW)
		call fseti (in, F_IOMODE, IO_RAW)

	    call ttywrite (out, tty, Memc[qs], len_qs, 0)
	    call flush (out)

	    nchars = ttyread (in, tty, Memc[buf],SZ_LINE,Memc[patbuf], TIMEOUT)
	    if (nchars > 0) {
		if (ctoi (Memc[buf],patindex(Memc[patbuf],w_index),width) <= 0)
		    width = 0
		if (ctoi (Memc[buf],patindex(Memc[patbuf],h_index),height) <= 0)
		    height = 0
	    }

	    if (sv_iomode != IO_RAW)
		call fseti (in, F_IOMODE, sv_iomode)

	    if (width == 0 && nchars == 0) {
		call eprintf ("timeout - terminal type set wrong? ")
		call eprintf ("(`stty termtype' to reset)\n")
	    }
	}

noquery_
	# If we still do not know the screen width or height, use the values
	# given in the user environment, else in the termcap entry for the
	# device.

	if (width <= 0)
	    width = ttystati (tty, TTY_NCOLS)
	if (height <= 0)
	    height = ttystati (tty, TTY_NLINES)

	call sfree (sp)
end
