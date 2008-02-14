# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <ctype.h>
include "iis.h"
include "zdisplay.h"

.help imd_setmapping, imd_getmapping, imd_query_map
.nf ____________________________________________________________________________

	Interface routines for setting and getting display server mappings.

	  imd_setmapping  (region, sx,sy,snx,sny, dx,dy,dnx,dny, objref)
 status = imd_getmapping  (region, sx,sy,snx,sny, dx,dy,dnx,dny, objref)
  status = imd_query_map  (wcs, region, sx,sy,snx,sny, dx,dy,dnx,dny, objref)

The imd_setmapping() procedure should be called prior to an imd_putwcs()
if the mapping information is to be sent with the next WCS write.  The
imd_getmapping() function returns a non-zero status if the last WCS query
returned valid mapping information during the read.  Both routines depend
upon a previous call to imd_wcsver() (imdmapping.x) to initialize the common
to query the server for this new capability.  The imd_query_map() function
returns a non-zero status if a valid mapping is available for the given WCS
number (e.g. the wcs number returned by a cursor read can be entered and
information such as the image name can be returned for the associated mapping).

.endhelp _______________________________________________________________________


# IMD_SETMAPPING --  Set the mapping information to be sent with the next
# SETWCS command.

procedure imd_setmapping (reg, sx, sy, snx, sny, dx, dy, dnx, dny, objref)

char	reg[SZ_FNAME]				#i region name
real	sx, sy					#i source raster
int	snx, sny
int	dx, dy					#i destination raster
int	dnx, dny
char	objref[SZ_FNAME]			#i object reference

bool	streq()

include "iis.com"

begin
	call strcpy (reg, iis_region, SZ_FNAME)
	iis_sx  = sx
	iis_sy  = sy
	iis_snx = snx
	iis_sny = sny
	iis_dx  = dx
	iis_dy  = dy
	iis_dnx = dnx
	iis_dny = dny

        if (streq (objref, "dev$pix"))
	    call fpathname ("dev$pix.imh", iis_objref, SZ_FNAME)
        else
            call strcpy (objref, iis_objref, SZ_FNAME)

	iis_valid = YES
end


# IMD_GETMAPPING --  Get the mapping information returned with the last
# GETWCS command.

int procedure imd_getmapping (reg, sx, sy, snx, sny, dx, dy, dnx, dny, objref)

char	reg[SZ_FNAME]				#o region name
real	sx, sy					#o source raster
int	snx, sny
int	dx, dy					#o destination raster
int	dnx, dny
char	objref[SZ_FNAME]			#o object reference

include "iis.com"

begin
	if (iis_valid == YES) {
	    call strcpy (iis_region, reg, SZ_FNAME)
	    sx  = iis_sx
	    sy  = iis_sy
	    snx = iis_snx
	    sny = iis_sny
	    dx  = iis_dx
	    dy  = iis_dy
	    dnx = iis_dnx
	    dny = iis_dny
	    call strcpy (iis_objref, objref, SZ_FNAME)
	}
	return (iis_valid)
end


# IMD_QUERY_MAP --  Return the mapping information in the server for the
# specified WCS number.

int procedure imd_query_map (wcs, reg, sx,sy,snx,sny, dx,dy,dnx,dny, objref)

int	wcs					#i WCS number of request
char	reg[SZ_FNAME]				#o region name
real	sx, sy					#o source raster
int	snx, sny
int	dx, dy					#o destination raster
int	dnx, dny
char	objref[SZ_FNAME]			#o object reference

pointer	sp, wcstext, ip, ds
int	fd, frame, chan, status, wcs_status, nl

int	fscan(), stropen(), iisflu()
pointer	imd_mapframe1()

include "iis.com"
define	done_	91

begin
	call smark (sp)
	call salloc (wcstext, SZ_WCSTEXT, TY_CHAR)
	call aclrc (Memc[wcstext], SZ_WCSTEXT)

	wcs_status = ERR
        iis_valid = NO
	frame = wcs / 100
	ds = NULL

	if (iis_version > 0) {

	    # If the channel isn't currently open, map the frame temporarily
	    # so we get a valid read.
	    if (iisnopen == 0)
		ds = imd_mapframe1 (frame, READ_ONLY, NO, NO)

            # Retrieve the WCS information from a display server.
            chan = iisflu(FRTOCHAN(frame))

	    # Query the server using the X register to indicate this is 	
	    # a "new form" of the WCS query, and pass the requested WCS in
	    # the T register (which is normally zero).

            call iishdr (IREAD+PACKED, SZ_WCSTEXT, WCS, 1, 0, chan, wcs)
            call iisio (Memc[wcstext], SZ_WCSTEXT, status)
            if (status > 0)
                call strupk (Memc[wcstext], Memc[wcstext], SZ_WCSTEXT)
            else
		goto done_


	    # Skip the wcs part of the string, we only want the mapping.
	    nl = 0
	    for (ip=wcstext ; Memc[ip] != NULL; ip=ip+1) {
		if (Memc[ip] == '\n')
		    nl = nl + 1
		if (nl == 2)
		    break
	    }
	    ip = ip + 1

	    # Open the string for reading.
            iferr (fd = stropen (Memc[ip], SZ_WCSTEXT, READ_ONLY))
                fd = NULL

            # Decode the Mapping from the WCS text.
            if (fd != NULL) {
                if (fscan (fd) != EOF) {
                    call gargwrd (reg, SZ_FNAME)
                    call gargr (sx)
                    call gargr (sy)
                    call gargi (snx)
                    call gargi (sny)
                    call gargi (dx)
                    call gargi (dy)
                    call gargi (dnx)
                    call gargi (dny)

                    if (fscan (fd) != EOF) {
                        call gargstr (objref, SZ_FNAME)
			wcs_status = OK
            		iis_valid = YES
                    }
                }
	    }

	    # Close any temporary connection to the server.
	    if (ds != NULL)
		call imunmap (ds)
	}

done_   if (fd != NULL)
            call close (fd)
        call sfree (sp)
	return (wcs_status)
end
