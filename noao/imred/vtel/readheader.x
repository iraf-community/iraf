include <mach.h>
include <fset.h>
include	"vt.h"

# READHEADER -- Read header info from the input.

int procedure readheader(inputfd, hbuf, selfbuf)

int	inputfd		# input file discriptor
pointer	hbuf		# header data input buffer pointer (short, SZ_VTHDR)
bool	selfbuf		# flag to tell if we should do our own buffering

int	numchars
pointer	sp, tempbuf
int	read()
errchk	read

begin
	call smark (sp)
	call salloc (tempbuf, 100, TY_SHORT)

	# If we are reading from tape and buffering for ourselves then
	# do a large read and see how many chars we get.  If too few or
	# too many give an error.  Otherwise just read the correct number 
	# of chars.

	if (selfbuf) {
	    iferr (numchars = read (inputfd, Mems[tempbuf],
		10000*SZB_SHORT/SZB_CHAR)) {
		call fseti (inputfd, F_VALIDATE, SZ_VTHDR*SZB_SHORT/SZB_CHAR)
		call printf ("Error reading header.\n")
		numchars = read (inputfd, Mems[tempbuf],
		    SZ_VTHDR*SZB_SHORT/SZB_CHAR)
	    }
	    if (numchars < 10 || numchars >= 100) {
	        call error (0, "error reading header")
		return (numchars)
	    }
	    call amovs (Mems[tempbuf], Mems[hbuf], SZ_VTHDR*SZB_SHORT/SZB_CHAR)
	} else {
	    iferr (numchars = read (inputfd, Mems[hbuf],
		SZ_VTHDR*SZB_SHORT/SZB_CHAR)) {
		call fseti (inputfd, F_VALIDATE, SZ_VTHDR*SZB_SHORT/SZB_CHAR)
		call printf ("Error reading header.\n")
		numchars = read (inputfd, Mems[tempbuf],
		    SZ_VTHDR*SZB_SHORT/SZB_CHAR)
	    }
	    if (numchars < SZ_VTHDR*SZB_SHORT/SZB_CHAR) {
	        call error (0, "eof encountered when reading header")
		return (0)
	    }
	}

	if (BYTE_SWAP2 == YES)
	    call bswap2 (Mems[hbuf], 1, Mems[hbuf], 1, SZ_VTHDR*SZB_SHORT)
	call sfree (sp)

	return (SZ_VTHDR*SZB_SHORT/SZB_CHAR)
end
