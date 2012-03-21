##
#  SPPINFO -- SPP test program to print the structure of a votable.
#
#  @file        votget_spp.x
#  @author      M. Fitzpatrick
#  @date        4/16/2011


task  info	= t_info

include "sppvotable.h"


define DEF_FILE     "sample.xml"




#  INFO -- Trivial SPP task to print the structure of a VOTable.  This is
#  similar to the C program of the same name.

procedure t_info ()

char	fname[SZ_FNAME]
char 	name[SZ_FNAME], ucd[SZ_FNAME], desc[SZ_LINE], value[SZ_LINE]

int	vot, res, tab, data, tdata, field, handle
int	i, len, ncols, nrows

bool	verbose, clgetb()


int	vx_openVOTABLE()
int	vx_getRESOURCE(), vx_getTABLE(), vx_getDATA(), vx_getPARAM()
int	vx_getTABLEDATA(), vx_getFIELD(), vx_getINFO(), vx_getNext()
int	vx_getDESCRIPTION(), vx_getCOOSYS()
int	vx_getNCols(), vx_getNRows(), vx_getLength()

begin
	# Get the parameters.
	call clgstr ("fname", fname, SZ_FNAME)
	verbose = clgetb ("verbose")


	# Open and parse the votable.
	vot = vx_openVOTABLE (fname)
	if (vot <= 0) {
	    call eprintf ("Cannot open file: '%s'\n")
		call pargstr (fname)
	    return
	}

	# Now get various handles from the table.
	res   = vx_getRESOURCE (vot)
	tab   = vx_getTABLE (res)
	data  = vx_getDATA (tab)
	tdata = vx_getTABLEDATA (data)

	ncols = vx_getNCols (tdata)
	nrows = vx_getNRows (tdata)

	# Print a table summary.
	call printf ("%s\n\n")
	    call pargstr (fname)

	call printf ("    Resources:  %d\t Table Size:  %d x %d\n")
            call pargi (vx_getLength (res))
	    call pargi (ncols)
	    call pargi (nrows)

    	handle = vx_getINFO (res)
	call printf ("         INFO:  %d\n")
	    call pargi (vx_getLength (handle))

	#  Print the table PARAMs.
    	handle = vx_getPARAM (res)
	len    = vx_getLength (handle)
	call printf ("        PARAM:  %d\t")
	    call pargi (len)
    	if (verbose) {
            while (handle > 0) {
                call vx_getAttr (handle, "id", name, SZ_FNAME)
                call vx_getAttr (handle, "value", value, SZ_LINE)

                call printf ("%s = %s  ")
		    call pargstr (name)
		    call pargstr (value)

                if (len > 1)
                    call printf ("\n\t\t\t")

		len = len - 1
		handle = vx_getNext (handle)
            }
    	}
    	call printf ("\n")

	# Print and table desccription.
    	call vx_getValue (vx_getDESCRIPTION (res), desc, SZ_LINE)
    	call printf ("  Description: %s\n\n ")
            call pargstr (desc)


        #  Print the column info in verbose mode.
        if (verbose) {
            call printf ("\n\t\t\tName\t\t\tUCD\n\n")

            i = 0
            for (field=vx_getFIELD(tab); field > 0; field=vx_getNext (field)) {
                call vx_getAttr (field, "name", name, SZ_FNAME)
                call vx_getAttr (field, "ucd", ucd, SZ_FNAME)

                call printf ("      Field %2d:  %-20.20s\t%-30.30s\n")
                    call pargi (i)
                    call pargstr (name)
                    call pargstr (ucd)

                handle = vx_getDESCRIPTION (field)
		call vx_getValue (handle, desc, SZ_LINE)
                call printf ("\t  Desc:  %-s\n\n")
		    call pargstr (desc)

		i = i + 1
            }
        }

	# Clean up.
	call vx_closeVOTABLE (vot)
end
