##
#  VOTUTIL_SPP.X
#
#  Utility procedures for the libVOTable SPP interface.
#
#  @file	votUtil_spp.x
#  @author	M. Fitzpatrick
#  @date	4/16/2011


include "votParse_spp.h"


# VOTINIT -- Initialize the VOT struct, parse the document and save the
# summary.  We return the VOT struct pointer itself, the caller is
# responsible for accessing the VOT_ROOT to get the raw root handle

pointer procedure votinit (votable)

char	votable[ARB]				#i VOTable file name

pointer	vot, vot_handle

#  Declare the libVOTable functions we'll be using.
int	vx_openVOTABLE(), vx_getRESOURCE(), vx_getTABLE(), vx_getDATA()
int	vx_getTABLEDATA(), vx_getFIELD(), vx_getINFO(), vx_getPARAM()
int	vx_getNCols(), vx_getNRows(), vx_getLength()

begin
	# Allocate the structure.
	call calloc (vot, SZ_VOT_STRUCT, TY_STRUCT)

	# Open and parse the votable.
	vot_handle = vx_openVOTABLE (votable)
	if (vot_handle <= 0) {
	    call eprintf ("Cannot open file: '%s'\n")
		call pargstr (votable)
	    return (NULL)
	}
	VOT_ROOT(vot) = vot_handle

	# Now get various handles from the table.
	VOT_RES(vot)   = vx_getRESOURCE (vot_handle)
	VOT_TAB(vot)   = vx_getTABLE (VOT_RES(vot))
	VOT_DATA(vot)  = vx_getDATA (VOT_TAB(vot))
	VOT_TDATA(vot) = vx_getTABLEDATA (VOT_DATA(vot))

    	VOT_INFO(vot)  = vx_getINFO (VOT_RES(vot))
    	VOT_PARAM(vot) = vx_getPARAM (VOT_RES(vot))
    	VOT_FIELD(vot) = vx_getFIELD (VOT_TAB(vot))

	VOT_NRES(vot)  = vx_getLength (VOT_RES(vot))
	VOT_NCOLS(vot) = vx_getNCols (VOT_TDATA(vot))
	VOT_NROWS(vot) = vx_getNRows (VOT_TDATA(vot))

	return (vot) 				# return the struct pointer
end


# VOTCLOSE -- Close the VOT struct and free any resources.

procedure votclose (vot)

pointer	vot					#i VOT struct pointer

begin
	call vx_closeVOTABLE (VOT_ROOT(vot))
	call mfree (vot, TY_STRUCT)
end
