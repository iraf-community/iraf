#
# VOTGET -- Download Access References from a VOTable

include <ctype.h>
include <votParse_spp.h>


define DEF_BASE   	"file"
define DEF_EXTN   	"fits"


#    input	input list of VOTables
#
#    base	base output filename
#    extn	output filename extension
#    sequential	use sequential file numbers
#
#    list	list, rather than download, access references
#    verbose	verbose output?
#    nthreads	Number of simultaneous downloads
#
#    col	column number to use for acred (0-indexed)
#    acref	ucd to identify the access reference column
#    rows	table rows to select
#    format	format to download (e.g. 'fits' or 'jpeg')


procedure t_votget ()

char	in[SZ_LINE], base[SZ_LINE], extn[SZ_LINE]
char    acref[SZ_FNAME], rows[SZ_FNAME], format[SZ_FNAME]
char    in_osfn[SZ_PATHNAME], nth[SZ_FNAME]
char    seq[SZ_FNAME], extract[SZ_FNAME]
int	inlist, nthreads, col
bool	sequential, list, verbose

int	clgeti(), clpopni(), clgfil(), votget()
bool	clgetb()

begin
	# Get the task parameters.
	inlist = clpopni ("input")

	call clgstr ("base", base, SZ_LINE)
	call clgstr ("extn", extn, SZ_LINE)
	call clgstr ("format", format, SZ_FNAME)
	call clgstr ("acref", acref, SZ_FNAME)
	call clgstr ("rows", rows, SZ_FNAME)

	nthreads   = clgeti ("nthreads")
	col        = clgeti ("col")
	list       = clgetb ("list")
	verbose    = clgetb ("verbose")
	sequential = clgetb ("sequential")


	call sprintf (nth, SZ_FNAME, "%d")
	    call pargi (nthreads)

	if (list)
	    call strcpy ("-x", extract, SZ_FNAME)
	else
	    call strcpy ("+n", seq, SZ_FNAME)

	if (sequential)
	    call strcpy ("-s", seq, SZ_FNAME)
	else
	    call strcpy ("+n", seq, SZ_FNAME)


	# Loop over the files,
	while (clgfil (inlist, in, SZ_LINE) != EOF) {

	    # Call the application part of the task.
	    #call imx_fetch (in, false)

            call fmapfn (in, in_osfn, SZ_PATHNAME)
            call strupk (in_osfn, in_osfn, SZ_PATHNAME)

            # voget -b <base> -N <N> [-t] <infile>
            call vx_voget (13, "-b", base, "-N", nth, seq, extract, 
		"-e", extn, "-f", format, "-u", acref, in_osfn)


#	    if (votget (in, extn, base, col, list, acref, verbose) != OK) {
#	        if (verbose)
#	            call eprintf ("Error calling votget()\n")
#	    }
	}

	call clpcls (inlist)

	call clpstr ("input", "")
end


# VOTGET -- Application-level interface entry point.

int procedure votget (votable, fmt, base, col, list, ucd_col, verbose)

char	votable[SZ_FNAME]			#i votable to parse
char	fmt[SZ_FNAME]				#i requested format
char	base[SZ_FNAME]				#i base filename
int	col					#i col number to use
bool	list					#i list access references?
char	ucd_col[SZ_FNAME]			#i UCD to use
bool	verbose					#i verbose output?

pointer	vot
char 	fname[SZ_FNAME], acref_ucd[SZ_FNAME], imfmt[SZ_FNAME]
char	acref[SZ_LINE], ucd[SZ_FNAME]
int	i, field, acref_col, acfmt_col, nread

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

		if (list) {
		    call eprintf ("%d: %s\n")
			call pargi (i+1)
			call pargstr (acref)
		} else {
		    if (verbose) {
		        call eprintf ("Downloading row %d\n")
			    call pargi (i+1)
		    }

		    nread = url_get (acref, fname, NULL)
		}
	    }
	}

	# Clean up.
	call votclose (vot)

	return (OK)
end
