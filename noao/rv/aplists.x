include	<smw.h>
include "rvpackage.h"
include	"rvflags.h"


# RV_APNUM_RANGE - Given a string in 'ranges' format decode it and
# load the apnum array in the header.

int procedure rv_apnum_range (rv, apnum)

pointer	rv				#I RV struct pointer
char	apnum[SZ_LINE]			#I APNUM range string

pointer	sp, ranges, image, aplist
pointer	im, smw
int	i, number, naps, stat

pointer	immap(), smw_openim()
int	get_next_number(), imaccess()
int	imtrgetim(), decode_ranges(), rv_verify_aps()
bool	streq()
errchk	immap, swm_openim, imaccess, rv_aplist, realloc, rv_verify_aps

begin
	if (streq(apnum,"") || streq(apnum," ")) {
	    call rv_errmsg ("Aperture list specified as a NULL string")
	    return (ERR_READ)
	}

	call smark (sp)

	# If a wildcard template is given read the image otherwise
	# expand the template list.

	if (apnum[1] == '*') {
	    call salloc (image, SZ_FNAME, TY_CHAR)
	    if (imtrgetim (RV_OBJECTS(rv), RV_IMNUM(rv), Memc[image],
		SZ_FNAME) == EOF) {
		call rv_errmsg ("Error getting object image `%s'.")
		    call pargstr (Memc[image])
		call sfree (sp)
		return (ERR_READ) 
	    }
	    if (imaccess (Memc[image], 0) == YES) {
		im = immap (Memc[image], READ_ONLY, 0)
		smw = smw_openim (im)
	    } else {
		call rv_errmsg ("Object image does not exist.")
		call sfree (sp)
		return (ERR_READ)
	    }

	    call rv_aplist (smw, aplist, naps)

	    call smw_close (smw)
	    call imunmap (im)

	} else {
	    call salloc (ranges, 3*SZ_APLIST, TY_INT)
	    if (decode_ranges (apnum, Memi[ranges], SZ_APLIST, naps) == ERR) {
		call sfree (sp)
	      	call rv_errmsg ("Error decoding APNUM range string.")
		return (ERR_READ)
	    }

	    call malloc (aplist, naps, TY_INT)
	    number = 0
	    i = 0
	    while (get_next_number (Memi[ranges], number) != EOF) {
	        Memi[aplist+i] = number
	        i = i + 1
	    }
	}

	# Now that we've parsed the aperture parameter, let's make sure it's
	# legal for the images we're given. If it is then we copy the new info
	# into the struct, otherwise return ERR_READ.

	stat = rv_verify_aps (rv, apnum, Memi[aplist], naps)
	if (stat == OK) {
	    call realloc (RV_APLIST(rv), 4*naps, TY_INT)
	    call realloc (RV_APPARAM(rv), SZ_LINE, TY_CHAR)

	    call amovi (Memi[aplist], APLIST(rv,1), naps)
	    call strcpy (apnum, APPARAM(rv), SZ_LINE)
	    RV_APNUM(rv) = APLIST(rv,1)
	    RV_OAPNUM(rv) = APLIST(rv,1)
	    NUMAPS(rv) = naps
	    CURAPNUM(rv) = 1
	}

	call mfree (aplist, TY_INT)
	call sfree (sp)
	return (stat)
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

pointer	sp, oimage, timage, aplist1
pointer	imo, smwo, imt, smwt, optr, tptr
int	i, legal, onum, tnum, onspec, tnspec

pointer	immap(), smw_openim()
int	rv_apmatch()
int	imaccess(), imtrgetim()
errchk	immap, smw_openim, imaccess, rv_aplist, realloc

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
	    smwo = smw_openim (imo)
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
	    smwt = smw_openim (imt)
	} else {
	    call sfree (sp)
	    call rv_errmsg ("Template image does not exist.")
	    return (ERR_READ)
	}

	# Now verify that the aperture lists are correct for the two images.
	# The check is done only against the current object/temp image pairs
	# since the routine is only called in an instance of a new image.
	aplist1 = NULL
	legal = OK
	onspec = SMW_NSPEC(smwo)
	tnspec = SMW_NSPEC(smwt)
	if (onspec == 1 && tnspec == 1 && apnum[1] != '*') {
	        call rv_errmsg(
		    "Aperture list cannot be applied to onedspec images.")
		    legal = ERR_READ
	} else if (onspec == 1 && tnspec > 1) {
	        call rv_errmsg (
		    "Aperture list cannot be applied to template image.")
	        legal = ERR_READ
	} else if (onspec > 1) {
	    # Check object aperture list to see if the requested apertures are
	    # in the image list.
	    call rv_aplist (smwo, aplist1, onspec)
	    do i = 1, naps {
		if (rv_apmatch(aplist[i], Memi[aplist1], onspec) == ERR) {
		    call rv_errmsg (
		        "Requested aperture not present in object image.")
		    legal = ERR_READ
		    goto exit_
		}
	    }

	    # If we have a two dimensional template do the same thing.
	    if (tnspec > 1) {
		call mfree (aplist1, TY_INT)
		call rv_aplist (smwt, aplist1, tnspec)
	        do i = 1, naps {
		    if (rv_apmatch(aplist[i], Memi[aplist1], tnspec) == ERR) {
		        call rv_errmsg (
		            "Requested aperture not present in object image.")
		        legal = ERR_READ
		        goto exit_
		    }
	        }
	    }
	}

exit_	call mfree (aplist1, TY_INT) 			# clean up
	call smw_close (smwo)
	call smw_close (smwt)
	call imunmap (imo)
	call imunmap (imt)
	call sfree (sp)
	return (legal)
end


# RV_APLIST -- Get a list of apertures from an SMW pointer.
# This routine allocates an integer array pointer which must be freed
# by the calling program.

procedure rv_aplist (smw, aplist, naps)

pointer	smw					#I SMW pointer
pointer	aplist					#O Aperture list pointer
int	naps					#O Number of apertures

int	i, ap, beam, dtype, nw
real	aplow[2], aphigh[2]
double	w1, dw, z
pointer	coeff
errchk	malloc, smw_gwattrs

begin
	naps = SMW_NSPEC(smw)
	call malloc (aplist, naps, TY_INT)

	coeff = NULL
	do i = 1, naps {
	    call smw_gwattrs (smw, i, 1, ap, beam, dtype, w1, dw, nw, z,
		aplow, aphigh, coeff)
	    Memi[aplist+i-1] = ap
	}
	call mfree (coeff, TY_CHAR)
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
