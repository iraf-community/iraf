##
#  VOTGET
#
#  Example program to download all access references in a VOTable using
#  the IRAF/SPP interface.
#
#  Usage:
#	    votget <votable> [<params>]
#
#  Where task parameters are:
#
#	    base=<str>		base output filename
#           col=N            	col number to use as acref column (0-indexed)
#           ucd=<str>          	use ucd to identify acref column
#           verbose[+-]         verbose output
#	    <votable>		Name of file to dump, or '-' for stdin
#
#  @file	votget_spp.x
#  @author	M. Fitzpatrick
#  @date	4/16/2011


include "votParse_spp.h"

define DEF_BASE   	"file"


task	votget	= t_votget			# task declaration


#  T_VOTGET -- Task entry point code.

procedure t_votget ()

char	base[SZ_FNAME], ucd[SZ_FNAME], votable[SZ_PATHNAME]
int	col
bool	verbose

int	clgeti(), votget()
bool	clgetb()

begin
	# Get the task parameters.
	call clgstr ("votable", votable, SZ_PATHNAME)
	call clgstr ("base",  base, SZ_FNAME)
	call clgstr ("ucd", ucd, SZ_FNAME)
	col = clgeti ("col")
	verbose = clgetb ("verbose")

	# Call the application part of the task.
	if (votget (votable, "fits", base, col, ucd, verbose) != OK) {
	    if (verbose)
	        call eprintf ("Error calling votget()\n")
	}
end


# VOTGET -- Application-level interface entry point.

int procedure votget (votable, fmt, base, col, ucd_col, verbose)

char	votable[SZ_FNAME]			#i votable to parse
char	fmt[SZ_FNAME]				#i requested format
char	base[SZ_FNAME]				#i base filename
int	col					#i col number to use
char	ucd_col[SZ_FNAME]			#i UCD to use
bool	verbose					#i verbose output

pointer	vot
char 	fname[SZ_FNAME], acref_ucd[SZ_FNAME], imfmt[SZ_FNAME]
char	acref[SZ_LINE], ucd[SZ_FNAME]
int	i, field, acref_col, acfmt_col, nread

#  Declare the libVOTable functions we'll be using.
pointer	votinit()
int	strcmp(), strsearch(), url_get(), vx_getNext()

begin
	# Initialize the VOT struct and parse the table.
	vot = votinit (votable)

	# Figure out which table column we want.  Note that we assume there
	# is only one <RESOURCE> element.  The caller may pass in a specific
	# column to be used, otherwise look for for the named UCD.

	call aclrc (acref_ucd, SZ_FNAME)
	if (col > 0) {
	    acref_col = col
	} else {
	    if (ucd_col[1] != EOS)
		call strcpy (ucd_col, acref_ucd, SZ_FNAME)
	    else
		call strcpy (DEF_ACREF_UCD, acref_ucd, SZ_FNAME)

	    # Find the access reference column number.
            i = 0
            for (field=VOT_FIELD(vot); field > 0; field=vx_getNext (field)) {
                call vx_getAttr (field, "ucd", ucd, SZ_FNAME)
		if (strcmp (ucd, acref_ucd) == 0) {
		    acref_col = i
		    break
		} else if (strcmp (ucd, DEF_FORMAT_UCD) == 0)
		    acfmt_col = i
	        i = i + 1
	    }
	}

	# Download the files.
	for (i=0; i < VOT_NROWS(vot); i=i+1) {
	    call vx_getTableCell (VOT_TDATA(vot), i, acfmt_col, imfmt, SZ_FNAME)

	    if (fmt[1] != EOS && strsearch (imfmt, fmt) > 0) {
	        call vx_getTableCell (VOT_TDATA(vot), i, acref_col, 
		    acref, SZ_LINE)

		# Create the local filename.
		call sprintf (fname, SZ_FNAME, "%s%03d.%s")
		    call pargstr (base)
		    call pargi (i)
		    call pargstr (fmt)

		if (verbose) {
		    call eprintf ("Downloading row %d\n")
			call pargi (i+1)
		}
		nread = url_get (acref, fname, NULL)
	    }
	}

	# Clean up.
	call votclose (vot)
end
