# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>

define	SZ_TMPFILBUF		100
define	SZ_INCREMENT		100


# FSVTFN -- Save the name of a temporary file for automatic deletion at task
# termination.

procedure fsvtfn (fname)

char	fname[ARB]
bool	first_time
int	sz_tmpbuf, nchars
int	strlen()
errchk	malloc, realloc

pointer	tmpbuf
int	nextch
common	/ftfcom/ tmpbuf, nextch
data	first_time /true/

begin
	if (first_time) {
	    tmpbuf = NULL
	    first_time = false
	}

	# Call with a null filename permits first time initialization.
	if (fname[1] == EOS)
	    return

	# Initial allocation of buffer.
	if (tmpbuf == NULL) {
	    sz_tmpbuf = SZ_TMPFILBUF
	    call malloc (tmpbuf, sz_tmpbuf, TY_CHAR)
	    nextch = 0
	}

	# Increase size of buffer if necessary.
	nchars = strlen (fname)
	if (nchars == 0)
	    return
	else {
	    while (nextch + nchars + 1 >= sz_tmpbuf) {
		sz_tmpbuf = sz_tmpbuf + SZ_INCREMENT
		call realloc (tmpbuf, sz_tmpbuf, TY_CHAR)
	    }
	}

	# Save name of temporary file in buffer.
	call strcpy (fname, Memc[tmpbuf+nextch], ARB)
	nextch = nextch + nchars + 1
end


# FRMTMP -- Delete all temporary files and return space.  It seems harmless
# for the user to explicitly delete a temporary file, so we do not complain
# if the file does not exist.

procedure frmtmp()

pointer	buftop, ip
int	strlen(), access()

pointer	tmpbuf
int	nextch
common	/ftfcom/ tmpbuf, nextch

begin
	if (tmpbuf != NULL) {
	    buftop = tmpbuf + nextch
	    for (ip=tmpbuf;  ip < buftop;  ip = ip + strlen (Memc[ip]) + 1)
		if (access (Memc[ip],0,0) == YES)
		    iferr (call delete (Memc[ip]))
			call erract (EA_WARN)
	    call mfree (tmpbuf, TY_CHAR)
	}
end
