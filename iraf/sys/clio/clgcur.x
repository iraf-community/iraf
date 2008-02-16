# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>

# CLGCUR -- Return the next cursor value from a list structured cursor type
# parameter.  The format of a cursor value is as follows:
#
#	X Y WCS KEY [SVAL]
#
#	X,Y	x and y cursor coordinates
#	WCS	wcs in which cursor coordinates are given
#	KEY	key(stroke) value associated with cursor read
#	SVAL	optional string associated with given key
#
# All fields need not be given, and extra fields may be supplied and will be
# either ignored or returned in SVAL.  The X-Y-WCS fields may be omitted
# (in which case the input is KEY-[SVAL]), causing INDEF INDEF 0 KEY SVAL to be
# returned, exactly as if the INDEF INDEF 0 had been typed in.  The number of
# fields read is returned as the function value; EOF is returned when the end
# of the cursor list is reached.

int procedure clgcur (param, wx, wy, wcs, key, strval, maxch)

char	param[ARB]		# parameter to be read
real	wx, wy			# cursor coordinates
int	wcs			# wcs to which coordinates belong
int	key			# keystroke value of cursor event
char	strval[ARB]		# string value, if any
int	maxch

char	ch
pointer	sp, buf, ip
int	nitems, op, delim
int	ctor(), ctoi(), cctoc(), clglstr(), stridx()
define	quit_ 91

begin
	call smark (sp)
	call salloc (buf, SZ_LINE + maxch, TY_CHAR)

	# Flush any buffered text or graphics output.
	call flush (STDERR)
	call flush (STDOUT)
	call gexfls()

	# Read the cursor.
	if (clglstr (param, Memc[buf], SZ_LINE + maxch) == EOF) {
	    call sfree (sp)
	    return (EOF)
	}

	ip = buf
	nitems = 0
	while (IS_WHITE (Memc[ip]))
	    ip = ip + 1

	if (IS_PRINT(Memc[ip]) && stridx (Memc[ip], "+-.0123456789") == 0) {
	    # The X-Y-WCS fields have been omitted; supply default values.
	    wx = INDEF
	    wy = INDEF
	    wcs = 0
	    nitems = 3

	} else {
	    # Decode the X-Y-WCS fields.
	    if (ctor (Memc, ip, wx) == 0)
		goto quit_
	    nitems = nitems + 1
	    if (ctor (Memc, ip, wy) == 0)
		goto quit_
	    nitems = nitems + 1
	    if (ctoi (Memc, ip, wcs) == 0)
		goto quit_
	    nitems = nitems + 1
	}

	# Get the KEY field.
	if (cctoc (Memc, ip, ch) == 0)
	    goto quit_
	key = ch
	nitems = nitems + 1

	# Get the optional SVAL field.
	while (IS_WHITE (Memc[ip]))
	    ip = ip + 1

	if (Memc[ip] != '\n' && Memc[ip] != EOS) {
	    # Check for a quoted string.
	    if (Memc[ip] == '"' || Memc[ip] == '\'') {
		delim = Memc[ip]
		ip = ip + 1
	    } else
		delim = 0

	    # Extract the string value.
	    op = 1
	    while (op <= maxch && Memc[ip] != '\n' && Memc[ip] != EOS &&
		Memc[ip] != delim) {
		strval[op] = Memc[ip]
		op = op + 1
		ip = ip + 1
	    }
	    strval[op] = EOS
	    nitems = nitems + 1
	}

quit_
	call sfree (sp)
	return (nitems)
end
