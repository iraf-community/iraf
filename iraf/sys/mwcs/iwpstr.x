# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"imwcs.h"

# IW_PUTSTR -- Put an arbitrarily large string valued parameter to a FITS
# header, using multiple FITS cards if necessary.  The input string value is
# passed in as a byte stream file.

procedure iw_putstr (fd, iw, axis, ctype, fmt1, fmt2, max_index)

int	fd			#I input file
pointer	iw			#I pointer to IMWCS descriptor
int	axis			#I axis to which parameter belongs
int	ctype			#I card type
char	fmt1[ARB], fmt2[ARB]	#I keyword name formats
int	max_index		#I use fmt2 if index > max_index

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
	    #while (nchars < SZ_BIGSTR && mod (nchars, SZ_BIGSTR) != 0) {
	    #	Memc[bigstr+nchars] = ' '
	    #	nchars = nchars + 1
	    #}
	    Memc[bigstr+nchars] = EOS

	    index = index + 1
	    cp = iw_findcard (iw, ctype, axis, index)

	    update = true
	    if (cp != NULL)
		if (strncmp (Memc[C_RP(cp)+IDB_STARTVALUE+1],
		    Memc[bigstr], SZ_BIGSTR) == 0) {
		    update = false
		}

	    # Output the card.  The format string should contain two %d
	    # fields, unless axis=ERR, in which case only the index value
	    # is used.  If the index value is greater than max_index then
	    # fmt2 is used as the print format, otherwise fmt1 is used.

	    if (update) {
		if (max_index > 0 && index > max_index)
		    call sprintf (kwname, SZ_KWNAME, fmt2)
		else
		    call sprintf (kwname, SZ_KWNAME, fmt1)
		if (axis >= 0)
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
