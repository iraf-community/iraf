include <imhdr.h>
include "rvflags.h"
include "rvpackage.h"

# READ_TEMPLATE_LIST - What it says, read the list pointer and get all of
# image spectra.  Checks for max number of template spectra and returns the 
# number of templates read or ERR_READ.

int procedure read_template_list (rv, list)

pointer	rv					#I RV struct pointer
pointer	list					#I Template file list pointer

pointer	sp, name
int	i, ntemps, npts

int	imtlen(), imtrgetim(), get_spec()
real	rv_imtempvel()
errchk	immap, imtrgetim, realloc, get_spec

define	error_		99

begin
	call smark (sp)
	call salloc (name, SZ_FNAME, TY_CHAR)

	# Do some simple error checking
	if (list == NULL) {
	    call rv_errmsg ("No input images in template list.")
	    goto error_
	}
	ntemps = imtlen(list)
	if (ntemps > MAXTEMPS) {
	    call eprintf("Too many images in template list (MAXTEMPS=%d)")
		call pargi (MAXTEMPS)
	    call flush (STDERR)
	    goto error_
	}

	# Start reading the files and storing them
	npts = 0
	call realloc (RV_TCODE(rv), ntemps, TY_INT)
	call realloc (RV_TEMPVEL(rv), ntemps, TY_REAL)
	RV_TEMPCODE(rv) = 0
	do i = 1, ntemps {
	    RV_TEMPNUM(rv) = i
	    if (imtrgetim(list, i, Memc[name], SZ_FNAME) == EOF) {
	    	call rv_errmsg ("Error getting image name from list.")
	    	goto error_
	    }

	    RV_TEMPCODE(rv) = RV_TEMPCODE(rv) + 1
	    TEMPCODE(rv,i) = RV_TEMPCODE(rv)
	    TEMPVEL(rv,i) = rv_imtempvel (rv, Memc[name])
	}

	# Now read the first template.
	RV_TEMPNUM(rv) = 1
	RV_NTEMPS(rv) = ntemps
	call realloc (RV_RIMAGE(rv), SZ_FNAME, TY_CHAR)
	if (imtrgetim(list, 1, RIMAGE(rv), SZ_FNAME) == EOF)
	    goto error_
	if (get_spec(rv,RIMAGE(rv),REFER_SPECTRUM) == ERR_READ) {
	    call sfree (sp)
            call error (0,"Error reading template.")
	}
	RV_TEMPCODE(rv) = TEMPCODE(rv,1)

	call sfree (sp)
	if (list != NULL)
	    call imtrew (list)			# rewind list pointer
	return (ntemps)

error_	call sfree (sp)
	if (list != NULL)
	    call imtrew (list)			# rewind list pointer
	return (ERR_READ)
end
