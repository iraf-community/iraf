include <pkg/cq.h>
include "../../lib/acatalog.h"

# AT_GCATHDR -- Read a standard ASTROMZ catalog header.

int procedure at_gcathdr (fd, hdrtext, maxch)

int	fd			#I the input file descriptor
char	hdrtext[ARB]		#O the output catalog description
int	maxch			#I the maximum size of the catalog description

pointer	sp, line
int	nlines, strfd, nchars
bool	first_line
int	stropen(), getline(), strncmp()

begin
	call smark (sp)
	call salloc (line, SZ_LINE, TY_CHAR)

	# Go to the beginning of the file.
	call seek (fd, BOF)

	# Initialize the number of lines  in the catalog description.
	nlines = 0
	first_line = true

	# Open the output text as a string.
	hdrtext[1] = EOS
	strfd = stropen (hdrtext, maxch, NEW_FILE)

	# Read in the catalog header as delimited by BEGIN CATALOG HEADER
	# and END CATALOG HEADER strings. Skip leading blank lines.
	repeat {
	    nchars = getline (fd, Memc[line])
	    if (nchars == EOF)
		break
	    if (first_line) {
	        if (Memc[line] == '\n')
		    next
	        if (strncmp (Memc[line], "#\n", 2) == 0)
		    next
	        if (strncmp (Memc[line], "# \n", 3) == 0)
		    next
	        if (strncmp (Memc[line], "# BEGIN CATALOG HEADER", 22) != 0)
		    break
		first_line = false
		next
	    }
	    if (strncmp (Memc[line], "# END CATALOG HEADER", 20) == 0)
		break
	    call fprintf (strfd, "%s")
		call pargstr (Memc[line+2])
	    nlines = nlines + 1
	}
	call close (strfd)

	# Return to the beginning of the file if no header was found.
	if (nlines == 0)
	    call seek (fd, BOF)

	call sfree (sp)

	return (nlines)
end


# AT_PCATHDR -- Read in the catalog format from a parameter set and create
# a standard ASTROMZ catalog header suitable for input to the catalog
# query routines.

int procedure at_pcathdr (pset, hdrtxt, maxch)

char	pset[ARB]		#I the name of the catalog description pset
char	hdrtxt[ARB]		#O the standard output catalog description
int	maxch			#I the maximum size of the header text

pointer	sp, fname, fval, funits, fmts, findex, ranges, pp
int	i, j, nfields, ncols, nvals, nlines, number, type, fsize, fd
char	cdtype
pointer	clopset()
int	at_wrdstr(), decode_ranges(), stropen(), strdic(), nscan()
int	get_next_number

begin
	# Get working space.
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (fval, SZ_FNAME, TY_CHAR)
	call salloc (funits, SZ_FNAME, TY_CHAR)
	call salloc (fmts, SZ_FNAME, TY_CHAR)
	call salloc (findex, AT_NSTDCOLS, TY_INT)
	call salloc (ranges, 3 * AT_MAX_NRANGES + 1, TY_INT)

	# Open the pset
	pp = clopset (pset)

	# Get the file type.
	call clgpset (pp, "ftype", Memc[fval], SZ_FNAME) 
	type = strdic (Memc[fval], Memc[fval], SZ_FNAME, CQ_RTYPESTR)
	if (type <= 0)
	    type = CQ_STEXT

	# Count the fields.
	nfields = 0; ncols = 0
	do i = 1, AT_NSTDCOLS {
	    if (at_wrdstr (i, Memc[fname], SZ_FNAME, AT_CATNAMES) <= 0)
		next
	    if (Memc[fname] == EOS)
		next
	    call clgpset (pp, Memc[fname], Memc[fval], SZ_FNAME) 
	    if (Memc[fval] == EOS)
		next
	    call sscan (Memc[fval])
		call gargwrd (Memc[fval], SZ_FNAME)
	    if (nscan() < 1)
		next
	    if (decode_ranges (Memc[fval], Memi[ranges], AT_MAX_NRANGES,
	        nvals) == ERR)
		next
	    if (nvals <= 0)
		next
	    if (type == CQ_BTEXT) {
		call gargi (j)
		if (nscan() < 2)
		    next
	    }
	    Memi[findex+nfields] = i
	    nfields = nfields + 1
	    ncols = ncols + nvals
	}

	# Write the header description.
	nlines = 0
	if (nfields > 0) {

	    # Open the string as a file.
	    hdrtxt[1] = EOS
	    fd = stropen (hdrtxt, maxch, NEW_FILE)

	    # Write the catalog type.
	    call clgpset (pp, "ftype", Memc[fval], SZ_FNAME) 
	    type = strdic (Memc[fval], Memc[fval], SZ_FNAME, CQ_RTYPESTR)
	    call fprintf (fd, "%s %s\n")
		call pargstr ("type")
		call pargstr (Memc[fval])
	    nlines = nlines + 1

	    # Write out the header parameters. At present there is only one
	    # the catalog coordinate system.
	    call fprintf (fd, "nheader 1\n")
	    nlines = nlines + 1
	    call clgpset (pp, "csystem", Memc[fval], SZ_FNAME) 
	    call fprintf (fd, "    %s %s\n")
		call pargstr ("csystem")
		call pargstr (Memc[fval])
	    nlines = nlines + 1

	    # Write out the legal fields.
	    call fprintf (fd, "nfields %d\n")
		call pargi (ncols)
	    nlines = nlines + 1
	    do i = 1, nfields {
	        if (at_wrdstr (Memi[findex+i-1], Memc[fname], SZ_FNAME,
		    AT_CATNAMES) <= 0)
		    next
	        if (Memc[fname] == EOS)
		    next
	        call clgpset (pp, Memc[fname], Memc[fval], SZ_FNAME) 
	        if (Memc[fval] == EOS)
		    next
	    	call sscan (Memc[fval])
		switch (type) {
		case CQ_BTEXT:
		    call gargwrd (Memc[fval], SZ_FNAME)
		    call gargi (fsize)
		    call gargwrd (Memc[funits], SZ_FNAME)
		    call gargwrd (Memc[fmts], SZ_FNAME)
		default:
		    call gargwrd (Memc[fval], SZ_FNAME)
		    call gargwrd (Memc[funits], SZ_FNAME)
		    call gargwrd (Memc[fmts], SZ_FNAME)
		}
	        if (decode_ranges (Memc[fval], Memi[ranges], AT_MAX_NRANGES,
	            nvals) == ERR)
		    next
	        if (nvals <= 0)
		    next
		if (at_wrdstr (Memi[findex+i-1], cdtype, 1, AT_CATTYPES) <= 0)
		    cdtype = 'c'
		if (Memc[funits] == EOS) {
		    if (at_wrdstr (Memi[findex+i-1], Memc[funits], SZ_FNAME,
		        AT_CATUNITS) <= 0)
			call strcpy ("INDEF", Memc[fmts], SZ_FNAME)
		}
		if (Memc[fmts] == EOS) {
		    if (at_wrdstr (Memi[findex+i-1], Memc[fmts], SZ_FNAME,
		        AT_CATFORMATS) <= 0)
			call strcpy ("%s", Memc[fmts], SZ_FNAME)
		}
		switch (type) {
		case CQ_BTEXT:
		    if (nvals == 1) {
			call fprintf (fd, "    %s %s %d %c %s %s\n")
			    call pargstr (Memc[fname])
			    call pargstr (Memc[fval])
			    call pargi (fsize)
			    call pargc (cdtype)
			    call pargstr (Memc[funits])
			    call pargstr (Memc[fmts])
			nlines = nlines + 1
		    } else {
			number = 0
			do j = 1, nvals {
			    call fprintf (fd, "    %s%d %d %d %c %s %s\n")
			        call pargstr (Memc[fname])
				call pargi (j)
			        call pargi (get_next_number (Memi[ranges],
				    number))
				call pargi (fsize)
			        call pargc (cdtype)
			        call pargstr (Memc[funits])
			        call pargstr (Memc[fmts])
			    nlines = nlines + 1
			}
		    }
		default:
		    if (nvals == 1) {
			call fprintf (fd, "    %s %s 0 %c %s %s\n")
			    call pargstr (Memc[fname])
			    call pargstr (Memc[fval])
			    call pargc (cdtype)
			    call pargstr (Memc[funits])
			    call pargstr (Memc[fmts])
			nlines = nlines + 1
		    } else {
			number = 0
			do j = 1, nvals {
			    call fprintf (fd, "    %s%d %d 0 %c %s %s\n")
			        call pargstr (Memc[fname])
				call pargi (j)
			        call pargi (get_next_number (Memi[ranges],
				    number))
			        call pargc (cdtype)
			        call pargstr (Memc[funits])
			        call pargstr (Memc[fmts])
			    nlines = nlines + 1
			}
		    }
		}
	    }

	    call close (fd)
	}

	# Close the pset
	call clcpset (pp)

	call sfree (sp)

	return (nlines)
end
