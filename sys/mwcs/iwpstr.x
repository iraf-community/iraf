# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"imwcs.h"

# IW_PUTSTR -- Put an arbitrarily large string valued parameter to a FITS
# header, using multiple FITS cards if necessary.  The input string value is
# passed in as a byte stream file.

procedure iw_putstr (fd, iw, axis, ctype, fmtstr)

int	fd			#I input file
pointer	iw			#I pointer to IMWCS descriptor
int	axis			#I axis to which parameter belongs
int	ctype			#I card type
char	fmtstr[ARB]		#I keyword name format

bool	update
int	index, nchars
pointer	sp, bigstr, im, cp
char	kwname[SZ_KWNAME]

pointer	iw_findcard()
int	read(), strncmp()
errchk	read, imaddf, impstr

begin
	call smark (sp)
	call salloc (bigstr, SZ_BIGSTR, TY_CHAR)

	index = 0
	im = IW_IM(iw)

	repeat {
	    # Get enough data to fit on a FITS card.
	    nchars = read (fd, Memc[bigstr], SZ_BIGSTR)
	    if (nchars <= 0)
		break

	    # Blank fill the last card if necessary.
	    while (nchars < SZ_BIGSTR && mod (nchars, SZ_BIGSTR) != 0) {
		Memc[bigstr+nchars] = ' '
		nchars = nchars + 1
	    }

	    index = index + 1
	    cp = iw_findcard (iw, ctype, axis, index)

	    update = true
	    if (cp != NULL)
		if (strncmp (Memc[C_RP(cp)+IDB_STARTVALUE+1],
		    Memc[bigstr], SZ_BIGSTR) == 0) {
		    update = false
		}

	    # Output the card.
	    if (update) {
		call sprintf (kwname, SZ_KWNAME, fmtstr)
		    call pargi (axis)
		    call pargi (index)
		if (cp == NULL)
		    call imaddf (im, kwname, "c")
		call impstr (im, kwname, Memc[bigstr])
	    }

	    if (cp != NULL)
		C_UPDATED(cp) = YES
	}

	call sfree (sp)
end
