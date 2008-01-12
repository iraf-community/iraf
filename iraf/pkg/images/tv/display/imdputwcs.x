include <imhdr.h>
include <error.h>
include <imset.h>
include <fset.h>
include	"display.h"
include	"iis.h"


# IMD_PUTWCS -- Write WCS.

procedure imd_putwcs (ds, frame, str1, str2, a, b, c, d, tx, ty, z1, z2, ztr)
pointer	ds			#I IMIO descriptor for image display.
int	frame			#I Frame number for which WCS is to be set.
char	str1[ARB]		#I First title string (image name).
char	str2[ARB]		#I Second title string (image title).
real    a, d                    #I x, y scale factors.
real    b, c                    #I cross terms (rotations).
real    tx, ty                  #I x, y offsets.
real	z1, z2			#I min and maximum grey scale values.
int	ztr			#I greyscale transformation code.

pointer	sp, old_wcs, mapping, wcstext, dir, fname, ftemp, device
int	wcsfile, server, chan[MAXCHAN]
int	fstati(), imstati(), envfind(), open(), strncmp()

include "iis.com"

begin
	call smark (sp)
	call salloc (old_wcs, SZ_WCSTEXT, TY_CHAR)
	call salloc (mapping, SZ_WCSTEXT, TY_CHAR)
	call salloc (wcstext, SZ_WCSTEXT, TY_CHAR)

        # Format the WCS text.
        call sprintf (Memc[old_wcs], SZ_WCSTEXT,
            "%s - %s\n%g %g %g %g %g %g %g %g %d\n")
            call pargstr (str1)
            call pargstr (str2)
            call pargr (a)
            call pargr (b)
            call pargr (c)
            call pargr (d)
            call pargr (tx)
            call pargr (ty)
            call pargr (z1)
            call pargr (z2)
            call pargi (ztr)

        # Add the mapping information if it's valid and we have a capable
	# server.
	if (iis_version > 0 && iis_valid == YES) {
	    call sprintf (Memc[mapping], SZ_WCSTEXT,
	        "%s %g %g %d %d %d %d %d %d\n%s\n")
		call pargstr (iis_region)
		call pargr (iis_sx)
		call pargr (iis_sy)
		call pargi (iis_snx)
		call pargi (iis_sny)
		call pargi (iis_dx)
		call pargi (iis_dy)
		call pargi (iis_dnx)
		call pargi (iis_dny)
		call pargstr (iis_objref)

	    call sprintf (Memc[wcstext], SZ_WCSTEXT, "%s%s")
		call pargstr (Memc[old_wcs])
		call pargstr (Memc[mapping])
        } else
            call strcpy (Memc[old_wcs], Memc[wcstext], SZ_OLD_WCSTEXT)


        # If we are writing to a display server (device has the logical
        # cursor capability), output the WCS text via the datastream,
        # else use a text file.  The datastream set-WCS is also used to
        # pass the frame buffer configuration to server devices.

	server = IM_LEN (ds, 4)

	if (server == YES) {
            chan[1] = fstati (imstati (ds, IM_PIXFD), F_CHANNEL)
	    chan[2] = MONO
	    call imd_setwcs (chan, Memc[wcstext])

	    # Invalidate the mapping once it's been sent.
	    iis_valid = NO

        } else {
	    # Construct the WCS filename, "dir$device_frame.wcs".
	    call salloc (dir,    SZ_PATHNAME, TY_CHAR)
	    call salloc (fname,  SZ_PATHNAME, TY_CHAR)
	    call salloc (ftemp,  SZ_PATHNAME, TY_CHAR)
	    call salloc (device, SZ_FNAME,    TY_CHAR)

	    if (envfind ("wcsdir", Memc[dir], SZ_PATHNAME) <= 0)
		if (envfind ("WCSDIR", Memc[dir], SZ_PATHNAME) <= 0)
		    if (envfind ("uparm", Memc[dir], SZ_PATHNAME) <= 0)
			call strcpy ("tmp$", Memc[dir], SZ_PATHNAME)

	    if (envfind ("stdimage", Memc[device], SZ_FNAME) <= 0)
		call strcpy ("display", Memc[device], SZ_FNAME)

	    # Get a temporary file in the WCS directory.
	    call sprintf (Memc[ftemp], SZ_PATHNAME, "%swcs")
		call pargstr (Memc[dir])
	    call mktemp (Memc[ftemp], Memc[ftemp], SZ_PATHNAME)

	    # Make the final WCS file filename.
	    call sprintf (Memc[fname], SZ_PATHNAME, "%s%s_%d.wcs")
		call pargstr (Memc[dir])
		if (strncmp (Memc[device], "imt", 3) == 0)
		    call pargstr ("imtool")
		else
		    call pargstr (Memc[device])
		call pargi (frame)

            # Update the WCS file.
            iferr (wcsfile = open (Memc[ftemp], TEMP_FILE, TEXT_FILE))
                call erract (EA_WARN)
            else {
                # Now delete the old file, if any, and write the new one.
                # To avoid process race conditions, create the new file as an
                # atomic operation, first writing a new file and then renaming
                # it to create the WCS file.

                iferr (call delete (Memc[fname]))
                    ;

                # Output the file version.
                call putline (wcsfile, Memc[wcstext])
                call close (wcsfile)

                # Install the new file.
                iferr (call rename (Memc[ftemp], Memc[fname]))
                    call erract (EA_WARN)
            }
        }

	call sfree (sp)
end
