include <tbset.h>
include "../lib/daophotdef.h"
include "../lib/apseldef.h"


# DP_RESTARS -- Read in the IDS of the stars to be excluded, find these stars
# in the photometry list, and set their magnitudes to INDEF.

int procedure dp_restars (dao, im, ext, text_file)

pointer	dao		# pointer to the daophot structure
pointer	im		# the input image descriptor
int	ext		# the exclude list file descriptor
bool	text_file	# text or table file ?

real	tx, ty, rjunk
pointer	apsel, sp, fields, indices, key
int	i, nrow, idno, nexcl, starno
int	tbpsta(), dp_apsel(), dp_exfind()

begin
	# Get some pointers.
	apsel = DP_APSEL(dao)

	# Get some working space.
	call smark (sp)
	call salloc (fields, SZ_LINE, TY_CHAR)
	call salloc (indices, 1, TY_INT) 

	# Initialize the read.
	if (text_file) {
	    call pt_kyinit (key)
	    Memi[indices] = DP_PAPID
	    call dp_gappsf (Memi[indices], Memc[fields], 1)
	} else {
	    call dp_tptinit (ext, Memi[indices])
	    nrow = tbpsta (ext, TBL_NROWS)
	}

	i = 1
	nexcl = 0
	repeat {

	    # Read the next star.
	    if (text_file) {
		if (dp_apsel (key, ext, Memc[fields], Memi[indices], idno,
		    rjunk, rjunk, rjunk, rjunk) == EOF)
		    break
	    } else {
		if (i > nrow)
		    break
		call dp_tptread (ext, Memi[indices], idno, rjunk, rjunk, rjunk,
		    i)
	    }

	    # Subtract star from the photometry list.
	     if (idno > 0) {
	        starno = dp_exfind (Memi[DP_APID(apsel)],
		    Memr[DP_APXCEN(apsel)], Memr[DP_APYCEN(apsel)],
		    Memr[DP_APMAG(apsel)], DP_APNUM(apsel), idno)
		if (starno > 0) {
		    if (DP_VERBOSE(dao) == YES) {
			call dp_wout (dao, im, Memr[DP_APXCEN(apsel)+starno-1],
			    Memr[DP_APYCEN(apsel)+starno-1], tx, ty, 1)
		        call printf (
		        "EXCLUDING   - Star:%5d X =%8.2f Y =%8.2f Mag =%8.2f\n")
		        call pargi (Memi[DP_APID(apsel)+starno-1])
		        call pargr (tx)
		        call pargr (ty)
		        call pargr (Memr[DP_APMAG(apsel)+starno-1])
		    }
		    nexcl = nexcl + 1
		} else if (DP_VERBOSE(dao) == YES) {
		    call printf ("EXCLUDING   - Star:%5d not found\n")
		        call pargi (idno)
		}
	    }

	    i = i + 1
	}

	if (text_file)
	    call pt_kyfree (key)
	call sfree (sp)

	return (nexcl)
end


# DP_EXFIND -- Find the star to be exclude in the photometry list.

int procedure dp_exfind (ids, xcen, ycen, mags, nstars, idex)

int	ids[ARB]		# array of stellar ids
real	xcen[ARB]		# array of x coordinates
real	ycen[ARB]		# array of y coordinates
real	mags[ARB]		# array of magnitudes
int	nstars			# number of stars in photometry list
int	idex			# id of star to be excluded

int	i, found

begin
	found = 0
	do i = 1, nstars {
	    if (ids[i] != idex)
		next
	    found = i
	    break
	}

	if (found > 0)
	    mags[i] = INDEFR

	return (found)
end
