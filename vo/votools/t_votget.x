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
#    nthreads	Number of simultaneous downloads
#
#    col	column number to use for acred (0-indexed)
#    acref	ucd to identify the access reference column
#    format	format to download (e.g. 'fits' or 'jpeg')


procedure t_votget ()

pointer	sp, in, base, extn, efname, acref, format, in_osfn, line
char    nth[SZ_FNAME], cnum[SZ_FNAME], seq[SZ_FNAME]
int	efd, inlist, nthreads, col
bool	sequential, list

int	open(), getline(), clgeti(), clpopni(), clgfil()
bool	clgetb()

begin
	call smark (sp)
	call salloc (in, SZ_FNAME, TY_CHAR)
	call salloc (base, SZ_FNAME, TY_CHAR)
	call salloc (extn, SZ_FNAME, TY_CHAR)
	call salloc (efname, SZ_FNAME, TY_CHAR)
	call salloc (acref, SZ_FNAME, TY_CHAR)
	call salloc (format, SZ_FNAME, TY_CHAR)
	call salloc (line, SZ_LINE, TY_CHAR)

	call strcpy ("", Memc[base], SZ_LINE)

	# Get the task parameters.
	inlist = clpopni ("input")

	list       = clgetb ("list")
	if (!list)
	    call clgstr ("base", Memc[base], SZ_LINE)
	call clgstr ("extn", Memc[extn], SZ_LINE)
	call clgstr ("format", Memc[format], SZ_FNAME)
	call clgstr ("acref", Memc[acref], SZ_FNAME)

	nthreads   = clgeti ("nthreads")
	col        = clgeti ("col")
	list       = clgetb ("list")
	sequential = clgetb ("sequential")


	# Convert integer args to strings for passing to the task.
	call sprintf (nth, SZ_FNAME, "%d")
	    call pargi (nthreads)
	if (!IS_INDEFI(col)) {
	    call sprintf (cnum, SZ_FNAME, "%d")
	        call pargi (col)
	}

	if (list)
	    call mktemp ("/tmp/ex", Memc[efname], SZ_FNAME)

	if (sequential)
	    call strcpy ("-s", seq, SZ_FNAME)
	else
	    call strcpy ("+n", seq, SZ_FNAME)


	# Loop over the files,
	while (clgfil (inlist, Memc[in], SZ_LINE) != EOF) {

            call fmapfn (Memc[in], Memc[in_osfn], SZ_PATHNAME)
            call strupk (Memc[in_osfn], Memc[in_osfn], SZ_PATHNAME)

	    # Call the application part of the task.
	    if (IS_INDEFI(col)) {
		if (list) {
                    call vx_voget (7, "-o", Memc[efname], "-f", Memc[format], 
			"-u", Memc[acref], Memc[in_osfn])
		} else {
                    call vx_voget (12, "-b", Memc[base], "-N", nth, seq, 
		        "-e", Memc[extn], "-f", Memc[format], 
			"-u", Memc[acref], Memc[in_osfn])
		}
	    } else {
		if (list) {
                    call vx_voget (7, "-o", Memc[efname], "-f", Memc[format], 
			"-F", cnum, Memc[in_osfn])
		} else {
                    call vx_voget (12, "-b", Memc[base], "-N", nth, seq,
		        "-e", Memc[extn], "-f", Memc[format], 
			"-F", cnum, Memc[in_osfn])
		}
	    }
	}

	# Print the extracted access references from the concatenated file.
	if (list) {
            efd = open (Memc[efname], READ_ONLY, TEXT_FILE)
            while (getline (efd, Memc[line]) != EOF) {
		call printf ("%s")
		    call pargstr (Memc[line])
	    }
	    call close (efd)
	    call delete (Memc[efname])
	}

	call clpcls (inlist)
	call sfree (sp)
end
