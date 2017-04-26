include	<tbset.h>

#* HISTORY *
#* B.Simon	25-Aug-94	original

# INFOMATCH -- Print diagnostic information for tmatch

procedure infomatch (diagfile, in1, in2, nmcol1, nmcol2, maxnorm, 
		     nclosest, closest, dist)

char	diagfile[ARB]	# i: diagnostic output file
pointer	in1		# i: first table's descriptor
pointer	in2		# i: second table's descriptor
char	nmcol1[ARB]	# i: name columns in first table
char	nmcol2[ARB]	# i: name columns in second table
double	maxnorm		# i: maximum allowed distance between matched rows
int	nclosest	# i: length of closest array
int	closest[ARB]	# i: array of closest matches between tables
double	dist[ARB]	# i: distance between matched rows
#--
bool	first, same
int	fd, namelen, mxcol1, mxcol2, ncol1, ncol2, idx, jdx, irow, jrow
pointer	sp, index, name, col1, col2

string	ziptitle  "\nThe following objects were not matched:\n"
string	duptitle  "\nThe following objects matched the same object:\n"
string	bigtitle  "\nThe following objects have the largest norms:\n"
string	normfmt   "Norm = %0.7g\n"
string	rowformat "%d:%d %s\n"

bool	is_blank()
int	open(), envgeti(), tbpsta()

begin
	# Open the diagnostics file

	if (is_blank (diagfile))
	    return

	fd = open (diagfile, WRITE_ONLY, TEXT_FILE)

	# Get maximum length of diagnostic string

	iferr {
	    namelen = envgeti ("ttyncols") - 10
	} then {
	    namelen = 70
	}

	# Allocate dynamic memory

	call smark (sp)
	call salloc (index, nclosest, TY_INT)
	call salloc (name, namelen, TY_CHAR)

	# Get column descriptors for name columns

	mxcol1 = tbpsta (in1, TBL_NCOLS)
	mxcol2 = tbpsta (in2, TBL_NCOLS)

	call salloc (col1, mxcol1, TY_INT)
	call salloc (col2, mxcol2, TY_INT)

	if (is_blank (nmcol1)) {
	    ncol1 = 0
	} else {
	    call tctexp (in1, nmcol1, mxcol1, ncol1, Memi[col1])
	}

	if (is_blank (nmcol2)) {
	    ncol2 = 0
	} else {
	    call tctexp (in2, nmcol2, mxcol2, ncol2, Memi[col2])
	}

	# Sort the closest array

	call setindex (Memi[index], nclosest)
	call sortclose (nclosest, closest, Memi[index])

	# Print the objects that were not matched

	first = true
	do idx = 1, nclosest {
	    irow = Memi[index+idx-1]
	    if (closest[irow] != 0)
		break

	    if (first) {
		first = false
		call fprintf (fd, ziptitle)
	    }

	    call rowname (in1, irow, ncol1, Memi[col1], Memc[name], namelen)
	    call fprintf (fd, rowformat)
	    call pargi (1)
	    call pargi (irow)
	    call pargstr (Memc[name])
	}

	# Print the objects which are matched more than once

	same = false
	first = true
	do idx = 2, nclosest {
	    irow = Memi[index+idx-1]
	    jrow = Memi[index+idx-2]

	    if (closest[irow] == 0)
		next

	    if (closest[irow] == closest[jrow]) {
		same = true

		if (first) {
		    first = false
		    call fprintf (fd, duptitle)
		}

		call rowname (in1, jrow, ncol1, Memi[col1], 
			      Memc[name], namelen)

		call fprintf (fd, rowformat)
		call pargi (1)
		call pargi (jrow)
		call pargstr (Memc[name])

	    } else if (same) {
		same = false

		call rowname (in1, jrow, ncol1, Memi[col1], 
			      Memc[name], namelen)

		call fprintf (fd, rowformat)
		call pargi (1)
		call pargi (jrow)
		call pargstr (Memc[name])

		call rowname (in2, closest[jrow], ncol2, Memi[col2],
			      Memc[name], namelen)

		call fprintf (fd, rowformat)
		call pargi (2)
		call pargi (closest[jrow])
		call pargstr (Memc[name])

		call fprintf (fd, "\n")
	    }
	}

	if (same) {
	    same = false
	    irow = Memi[index+nclosest-1]

	    call rowname (in1, irow, ncol1, Memi[col1], 
			  Memc[name], namelen)

	    call fprintf (fd, rowformat)
	    call pargi (1)
	    call pargi (irow)
	    call pargstr (Memc[name])

	    call rowname (in2, closest[irow], ncol2, Memi[col2],
			  Memc[name], namelen)

	    call fprintf (fd, rowformat)
	    call pargi (2)
	    call pargi (closest[irow])
	    call pargstr (Memc[name])

	    call fprintf (fd, "\n")
	}

	# Sort the dist array

	call setindex (Memi[index], nclosest)
	call sortdist (nclosest, dist, Memi[index])

	# Print the ten objects with the largest norms

	jdx = 0
	do idx = nclosest, 1, -1 {
	    irow = Memi[index+idx-1]
	    if (dist[irow] == maxnorm)
		next

	    if (jdx == 0)
		call fprintf (fd, bigtitle)

	    jdx = jdx + 1
	    if (jdx > 10)
		break

	    call fprintf (fd, normfmt)
	    call pargd (dist[irow])

	    call rowname (in1, irow, ncol1, Memi[col1], 
			  Memc[name], namelen)

	    call fprintf (fd, rowformat)
	    call pargi (1)
	    call pargi (irow)
	    call pargstr (Memc[name])

	    call rowname (in2, closest[irow], ncol2, Memi[col2],
			  Memc[name], namelen)

	    call fprintf (fd, rowformat)
	    call pargi (2)
	    call pargi (closest[irow])
	    call pargstr (Memc[name])

	    call fprintf (fd, "\n")

	}

	call close (fd)
	call sfree (sp)
end
