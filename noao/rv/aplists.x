include <imhdr.h>
include "rvpackage.h"
include "rvflags.h"
include "rvmwcs.h"

# RV_APNUM_RANGE - Given a string in 'ranges' format decode it and
# load the apnum array in the header.

int procedure rv_apnum_range (rv, apnum)

pointer	rv				#I RV struct pointer
char	apnum[SZ_LINE]			#I APNUM range string

pointer	sp, ranges, image, aplist
pointer	imo, optr
int	i, number, naps, onum

pointer	immap()
int	get_next_number(), imaccess()
int	imtrgetim(), decode_ranges(), rv_verify_aps()
int	rv_apformat()
bool	streq()
errchk	immap, imaccess, realloc, rv_verify_aps

begin
	if (streq(apnum,"") || streq(apnum," ")) {
	    call rv_errmsg ("Aperture list specified as a NULL string")
	    return (ERR_READ)
	}

	# Let's get some storage space.
	call smark (sp)
	call salloc (ranges, 3*SZ_APLIST, TY_INT)
	call salloc (image, SZ_FNAME, TY_CHAR)

	onum = RV_IMNUM(rv)
	optr = RV_OBJECTS(rv)

	# Map the images so we can read the headers.
	if (imtrgetim (optr, onum, Memc[image], SZ_FNAME) == EOF) {
	    call rv_errmsg ("Error getting object image `%s'.")
	        call pargstr (Memc[image])
	    call sfree (sp)
	    return (ERR_READ) 
	}
	if (imaccess(Memc[image],0) == YES)
	    imo = immap (Memc[image], READ_ONLY, 0)
	else {
	    call sfree (sp)
	    call rv_errmsg ("Object image does not exist.")
	    return (ERR_READ)
	}

	# Now create the aperture lists.
        if (RV_MWCSP(rv) == NULL)
           call rv_mwcs_open (rv)
	RV_OFORMAT(rv) = rv_apformat (rv, imo)
	if (apnum[1] == '*') {
	    if (RV_OFORMAT(rv) == ONEDSPEC) {
	        call salloc (aplist, 2, TY_INT)
		Memi[aplist] = 1
		Memi[aplist] = RMW_AP(rv,1)
		naps = 1
	    } else {
		# Load all of the aperture present in the image.
	        call salloc (aplist, RVMW_NSPEC(rv), TY_INT)
		call amovi (RMW_AP(rv,1), Memi[aplist], RVMW_NSPEC(rv))
	        naps = RVMW_NSPEC(rv)
	    }

	} else {
	    if (decode_ranges(apnum,Memi[ranges],SZ_APLIST,naps) == ERR) {
		call sfree (sp)
	      	call rv_errmsg ("Error decoding APNUM range string.")
		return (ERR_READ)
	    }
	    call salloc (aplist, naps, TY_INT)
	    number = 0
	    i = 0
	    while (get_next_number (Memi[ranges], number) != EOF) {
	        Memi[aplist+i] = number
	        i = i + 1
	    }
	}
	call rv_mwcs_close (rv)

	# Now that we've parsed the aperture parameter, let's make sure it's
	# legal for the images we're given. If it is then we copy the new info
	# into the struct, otherwise return ERR_READ.

	if (rv_verify_aps (rv, apnum, Memi[aplist], naps) == OK) {
	    call realloc (RV_APLIST(rv), 4*naps, TY_INT)
	    call realloc (RV_APPARAM(rv), SZ_LINE, TY_CHAR)

	    call amovi (Memi[aplist], APLIST(rv,1), naps)
	    call strcpy (apnum, APPARAM(rv), SZ_LINE)
	    RV_APNUM(rv) = APLIST(rv,1)
	    RV_OAPNUM(rv) = APLIST(rv,1)
	    NUMAPS(rv) = naps
	    CURAPNUM(rv) = 1

	} else {
	    call sfree (sp)
	    call imunmap (imo)
	    return (ERR_READ)
	}

	# Clean up.
	call imunmap (imo)
	call sfree (sp)
	return (OK)
end


# RV_APFORMAT - Get the format of the image (onedspec, multispec, or echelle).
# This isn't very efficient because it opens the image at each invocation,
# but can be optomized later.

int procedure rv_apformat (rv, im)

pointer	rv					#I RV struct pointer
pointer	im					#I Image pointer

int	pre_alloc, code

begin
	# Get the structure
	pre_alloc = YES
	if (RV_MWCSP(rv) == NULL) {
	   call rv_mwcs_open (rv)
	   pre_alloc = NO
	}
	call rv_mwcs (im, RVMW_AP(rv), RVMW_DTYPE(rv), RVMW_W1(rv), RVMW_DW(rv),
	    RVMW_NW(rv), RVMW_NSPEC(rv))

	# Now figure it out....
	if (RVMW_NSPEC(rv) > 1)
	    code = MULTISPEC
	else if (RVMW_NSPEC(rv) == 1)
	    code = ONEDSPEC

	# Clean up.
	if (pre_alloc == NO)
	    call rv_mwcs_close (rv)

	return (code)
end


# RV_READ_AP - Given a line in an image, construct an APNUMn keyword and
# return the value of the aperture in the string.

int procedure rv_read_ap (rv, im, line)

pointer	rv					#I RV struct descriptor
pointer	im					#I Image descriptor
int	line					#I Line in the data

int	apid

begin
        if (RV_MWCSP(rv) == NULL) {
            call rv_mwcs_open (rv)
            call rv_mwcs (im, RVMW_AP(rv), RVMW_DTYPE(rv), RVMW_W1(rv), 
		RVMW_DW(rv), RVMW_NW(rv), RVMW_NSPEC(rv))
	    apid = RMW_AP(rv,line)
	    call rv_mwcs_close (rv)
	} else {
            call rv_mwcs (im, RVMW_AP(rv), RVMW_DTYPE(rv), RVMW_W1(rv), 
	 	RVMW_DW(rv), RVMW_NW(rv), RVMW_NSPEC(rv))
	    apid = RMW_AP(rv,line)
	}

	if (DBG_DEBUG(rv) == YES || RV_APODIZE(rv) == 0.116) {
	    call d_printf(DBG_FD(rv), "rv_read_ap:  line=%d => apid=%d\n")
		call pargi(line);	call pargi(apid)
	}
	return (apid)
end


# RV_VERIFY_APS - Scan the object and template image lists and verify that
# list of apertures selected is valid for the two current images.
#
# NOTE: As this is currently implemented, we are restricted to either match-
# ing aperture pairs directly, or allowing only 1-D templates.  This will
# change in a future release once separate aperture lists are maintained for
# both the object and the template images.

int procedure rv_verify_aps (rv, apnum, aplist, naps)

pointer	rv				#I RV struct pointer
char	apnum[SZ_LINE]			#I Aperture parameter string
int	aplist[ARB]			#I List of selected apertures
int	naps				#I Number of apertures.

pointer	sp, oimage, timage
pointer	imo, imt, optr, tptr
int	i, legal, onum, tnum, nspec

pointer	immap()
int	rv_apformat(), rv_apmatch()
int	imaccess(), imtrgetim()
errchk	immap, imaccess, realloc

define	exit_		99

begin
	call smark (sp) 			# let's get some storage space
	call salloc (oimage, SZ_FNAME, TY_CHAR)
	call salloc (timage, SZ_FNAME, TY_CHAR)

	onum = RV_IMNUM(rv)			# initialize stuff
	optr = RV_OBJECTS(rv)
	tnum = RV_TEMPNUM(rv)
	tptr = RV_TEMPLATES(rv)

	# Map the images so we can read the headers.
	if (imtrgetim (optr, onum, Memc[oimage], SZ_FNAME) == EOF) {
	    call rv_errmsg ("Error getting object image `%s'.")
	        call pargstr (Memc[oimage])
	        call sfree (sp)
	        return (ERR_READ)
	}
	if (imaccess(Memc[oimage],0) == YES) {
	    imo = immap (Memc[oimage], READ_ONLY, 0)
	} else {
	    call sfree (sp)
	    call rv_errmsg ("Object image does not exist.")
	    return (ERR_READ)
	}

	if (imtrgetim (tptr, tnum, Memc[timage], SZ_FNAME) == EOF) {
	    call rv_errmsg ("Error getting template image `%s'.")
	        call pargstr (Memc[timage])
	        call sfree (sp)
	        return (ERR_READ)
	}
	if (imaccess(Memc[timage],0) == YES) {
	    imt = immap (Memc[timage], READ_ONLY, 0)
	} else {
	    call sfree (sp)
	    call rv_errmsg ("Template image does not exist.")
	    return (ERR_READ)
	}

	# Now verify that the aperture lists are correct for the two images.
	# The check is done only against the current object/temp image pairs
	# since the routine is only called in an instance of a new image.
	legal = OK
	RV_OFORMAT(rv) = rv_apformat (rv, imo)
	RV_RFORMAT(rv) = rv_apformat (rv, imt)
	if (RV_OFORMAT(rv) == ONEDSPEC && 
	    RV_RFORMAT(rv) == ONEDSPEC && apnum[1] != '*') {
	        call rv_errmsg(
		    "Aperture list cannot be applied to onedspec images.")
		    legal = ERR_READ
	} else if (RV_OFORMAT(rv) == ONEDSPEC &&
	    (RV_RFORMAT(rv) != ONEDSPEC && IM_LEN(imt,2) > 1)) {
	        call rv_errmsg (
		    "Aperture list cannot be applied to template image.")
	        legal = ERR_READ
	} else if (RV_OFORMAT(rv) == MULTISPEC) {
	    # Check object aperture list to see if the requested apertures are
	    # in the image list.
	    call rv_mwcs_open (rv)
            call rv_mwcs (imo, RVMW_AP(rv), RVMW_DTYPE(rv), RVMW_W1(rv),
		RVMW_DW(rv), RVMW_NW(rv), RVMW_NSPEC(rv))
	    nspec = RVMW_NSPEC(rv)
	    do i = 1, naps {
		if (rv_apmatch(aplist[i],RMW_AP(rv,1),nspec) == ERR) {
		    call rv_errmsg (
		        "Requested aperture not present in object image.")
		    legal = ERR_READ
		    call rv_mwcs_close (rv)
		    goto exit_
		}
	    }
	    call rv_mwcs_close (rv)

	    # If we have a two dimensional template do the same thing.
	    if (RV_RFORMAT(rv) == MULTISPEC && IM_LEN(imt,2) > 1) {
	        call rv_mwcs_open (rv)
                call rv_mwcs (imt, RVMW_AP(rv), RVMW_DTYPE(rv), RVMW_W1(rv),
		    RVMW_DW(rv), RVMW_NW(rv), RVMW_NSPEC(rv))
	        nspec = RVMW_NSPEC(rv)
	        do i = 1, naps {
		    if (rv_apmatch(aplist[i],RMW_AP(rv,1),nspec) == ERR) {
		        call rv_errmsg (
		            "Requested aperture not present in object image.")
		        legal = ERR_READ
		        call rv_mwcs_close (rv)
		        goto exit_
		    }
	        }
	        call rv_mwcs_close (rv)
	    }
	}

exit_	call sfree (sp) 			# clean up
	call imunmap (imo)
	call imunmap (imt)
	return (legal)
end


# RV_APMATCH - Given an aperture number see if it is in the given list.

int procedure rv_apmatch (apnum, aplist, naps)

int	apnum					#I Requested aperture
int	aplist[ARB]				#I Aperture list
int	naps					#I Number of apertures

int	i

begin
	do i = 1, naps {
	    if (apnum == aplist[i])
		return (OK)
	}
	return (ERR)
end
