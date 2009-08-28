include "../lib/daophotdef.h"

# DP_IMKEYS - Set the image name and keyword parameters after an image
# is mapped.

procedure dp_imkeys (dp, im)

pointer dp                      # pointer to the daophot structure
pointer im                      # the image descriptor

pointer mw, ct
int     dp_stati()
pointer	dp_statp(), mw_openim(), mw_sctran()
errchk  mw_openim(), mw_sctran()

include	<nullptr.inc>

begin
        # Set the wcs descriptors.
        mw = dp_statp (dp, MW)
        if (mw != NULL)
            call mw_close (mw)
        iferr {
            mw = mw_openim (im)
        } then {
            call dp_setp (dp, MW, NULLPTR)
            call dp_setp (dp, CTIN, NULLPTR)
            call dp_setp (dp, CTOUT, NULLPTR)
            call dp_setp (dp, CTPSF, NULLPTR)
        } else {
            call dp_setp (dp, MW, mw)
            switch (dp_stati (dp, WCSIN)) {
            case WCS_WORLD:
                iferr (ct = mw_sctran (mw, "world", "logical", 03B))
                    ct = NULL
            case WCS_PHYSICAL:
                iferr (ct = mw_sctran (mw, "physical", "logical", 03B))
                    ct = NULL
            case WCS_TV, WCS_LOGICAL:
                ct = NULL
            default:
                ct = NULL
            }
            call dp_setp (dp, CTIN, ct)
            switch (dp_stati (dp, WCSOUT)) {
            case WCS_PHYSICAL:
                iferr (ct = mw_sctran (mw, "logical", "physical", 03B))
                    ct = NULL
            case WCS_TV, WCS_LOGICAL:
                ct = NULL
            default:
                ct = NULL
            }
            call dp_setp (dp, CTOUT, ct)
            switch (dp_stati (dp, WCSPSF)) {
            case WCS_PHYSICAL:
                iferr (ct = mw_sctran (mw, "logical", "physical", 03B))
                    ct = NULL
            case WCS_TV, WCS_LOGICAL:
                ct = NULL
            default:
                ct = NULL
            }
            call dp_setp (dp, CTPSF, ct)
        }

	# Get the proper values from the image header if an image is defined.
	call dp_padu (im, dp)
	call dp_rdnoise (im, dp)
	call dp_filter (im, dp)
	call dp_airmass (im, dp)
	call dp_otime (im, dp)
end
