include <imhdr.h>
include <pkg/cq.h>

# Add a valid WCS to the image header if it does not already have one using
# the image WCS status specified by the wcs keyword in the image survey
# database. If the wcs keyord  is "fits", the image is assumed to have a
# valid FITS WCS and no new wcs is computed, if it is "dss" the image is assumed
# to have a valid DSS image header which will be transformed to a valid FITS
# WCS if a FITS WCS is not already present, if it is "none" the image is
# assumed to have no valid WCS and the code will attempt to insert one using
# information in the image results structure. An error status is returned
# only if there is no valid wcs code.

procedure at_wedit (im, res, at, wcstype, update, verbose)

pointer	im			#I the input image descriptor
pointer	res			#I the image query results descriptor
pointer	at			#I the astrometry package descriptor
int	wcstype			#I the default wcs type
bool	update			#I actually update the header ?
bool	verbose			#I verbose mode ?

int	cq_istati(), at_mkdss(), at_dbwcs(), at_parwcs()

begin
	# Update WCS from database
	if (res != NULL) {

	    switch (cq_istati (res, CQWCS)) {

	    # Image surveys database indicates image already has a FITS WCS.
	    case CQ_WFITS:
	        ;

	    # Image surveys database indicates image has a DSS WCS.
	    case CQ_WDSS:
	        if (at_mkdss (im, update, verbose) == ERR) {
	            #if (update || verbose)
	            if (verbose)
		        call printf (
			    "    Error converting DSS wcs to FITS wcs\n")
	        }

	    # Image surveys database indicates image has no WCS. If the proper
	    # information is not in the image survey then default to awcspars.
	    default:
	        if (at_dbwcs (im, res, update, verbose) == ERR) {
	            #if (update || verbose)
	            if (verbose)
		        call printf (
		        "    Error creating FITS wcs using image survey db\n")
	        }
	    }

	} else {

	    switch (wcstype) {

	    # User parameter indicates image already has a FITS WCS.
	    case CQ_WFITS:
		;

	    # User parameter indicates image has a DSS WCS.
	    case CQ_WDSS:
	        if (at_mkdss (im, update, verbose) == ERR) {
	            #if (update || verbose)
	            if (verbose)
		        call printf (
			    "    Error converting DSS wcs to FITS wcs\n")
	        }

	    default:
	        if (at == NULL)
		    ;
	        else if (at_parwcs (im, at, update, verbose) == ERR) {
	            #if (update || verbose)
	            if (verbose)
		        call printf (
		       "    Error creating FITS wcs using default parameters\n")
	        }
	    }
	}
end
