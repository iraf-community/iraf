include	"../identify.h"


# AID_SHIFT -- Find a new shifted dispersion solution assuming (nearly) the
# same dispersion per pixel and the same dispersion direction.  The shift is
# assumed to be less than or equal to the dispersion range of the input
# dispersion.  The input is an ID pointer have the previous dispersion
# solution and features but with the new spectrum.  If there are more than 10
# features then the list of user feature coordinates is used as the reference
# list.  If there are not enough features or the previous search fails then
# the the coordinate list is used as the reference.  The returned result is a
# new ID pointer if the algorithm succeeds or the original ID pointer if it
# fails along with an error status.

procedure aid_shift (id, crsearch, cdsearch)

pointer	id		#I ID object
double	crsearch	#I Search range
double	cdsearch	#I Search range

pointer	aid
bool	found, aid_autoid()
double	crpix, crval, cdelt, id_fitpt()

begin
	# Set approximate dispersion from input dispersion solution.
	crpix = ID_NPTS(id) / 2 + 1
	crval = id_fitpt (id, crpix)
	cdelt = (FITDATA(id,ID_NPTS(id)) - FITDATA(id,1)) /
	    (ID_NPTS(id) - 1)

	# Initialize AUTOID.
	call aid_init (aid, "aidpars")
	call aid_setd (aid, "crval", crval)
	call aid_setd (aid, "cdelt", cdelt)
	call aid_setd (aid, "crpix", crpix)
	call aid_sets (aid, "cddir", "sign")
	call aid_setd (aid, "crsearch", crsearch)
	call aid_setd (aid, "cdsearch", cdsearch)
	call aid_seti (aid, "nbest", 5)

	found = false
	if (ID_NFEATURES(id) > 10) {
	    # Try shift using features.
	    call aid_seti (aid, "ntarget", ID_NFEATURES(id))
	    call aid_seti (aid, "nreference", ID_NFEATURES(id))
	    call aid_setr (aid, "wrms", 0.5)
	    call aid_setr (aid, "wfmatch", 0.5)
	    call aid_setr (aid, "wftmatch", 0.)
	    call aid_sets (aid, "refspec", "FEATURES")
	    found = aid_autoid (id, aid)
	}
	if (!found) {
	    # Try shift using coordinate list.
	    call aid_seti (aid, "ntarget", max (ID_NFEATURES(id),20))
	    call aid_seti (aid, "nreference", max (ID_NFEATURES(id),40))
	    call aid_setr (aid, "wrms", 0.5)
	    call aid_setr (aid, "wfmatch", 0.25)
	    call aid_setr (aid, "wftmatch", 0.25)
	    call aid_sets (aid, "refspec", "COORDLIST")
	    found = aid_autoid (id, aid)
	}

	call aid_free (aid)
	if (!found)
	    call error (1, "No solution not found")
end
