include <fset.h>
include <ctotok.h>
include <ctype.h>
include "../lib/io.h"

# T_CATALOG -- The CATALOG task is a simple tool designed to aid the user
# in creating and formating  catalogs of standard stars. CATALOG queries
# the user for the name of the output catalog, a catalog title,
# and then prompts the user for input. If the catalog already exists
# CATALOG enters append mode and permits the user to add new records to
# the end of the catalog. Options exist to review old items in the catalog
# and to verify new ones.

procedure t_catalog ()

pointer	output		# pointer to the name of the output catalog
pointer	title		# pointer to the title of the output catalog
pointer	format		# pointer to the name of the output format file
int	review		# review the data in a pre-existing catalog ?
int	verify		# verify all new user input ?

int	i, sz_record, ncols, newfile, fd, fdfmt, tfd, imin, imax
pointer	sp, otitle, toutput, names, widths
bool	clgetb()
int	access(), open(), btoi(), ph_open(), ph_rhdr(), strmatch(), ph_setcols()

begin
	# Set up the standard output to flush on a newline.
	call fseti (STDOUT, F_FLUSHNL, YES)

	# Allocate working space.
	call smark (sp)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (toutput, SZ_FNAME, TY_CHAR)
	call salloc (title, SZ_FNAME, TY_CHAR)
	call salloc (otitle, SZ_FNAME, TY_CHAR)
	call salloc (format, SZ_FNAME, TY_CHAR)

	# Get the parameters.
	call clgstr ("catalog", Memc[output], SZ_FNAME)
	call sprintf (Memc[format], SZ_FNAME, "f%s.dat")
	    call pargstr (Memc[output])
	verify = btoi (clgetb ("verify"))

	# Determine whether the catalog is a new catalog or an existing
	# catalog. If the catalog is a new catalog get the number of columns,
	# the column names and column widths from the user. If the catalog
	# is an old catalog open the catalog, read the number of columns, the
	# column names and the column widths from the catalog header and
	# match the catalog title with the catalog file name.

	names = NULL
	widths = NULL
	call strcpy (Memc[output], Memc[title], SZ_FNAME)

	if (access (Memc[output], 0, 0) == NO) {

	    fd = open (Memc[output], NEW_FILE, TEXT_FILE)
	    if (access (Memc[format], 0, 0) == YES)
		call delete (Memc[format])
	    fdfmt = open (Memc[format], NEW_FILE, TEXT_FILE)
	    ncols = ph_setcols (names, widths, MKCAT_MAXCOLWIDTH, MKCAT_NCOLS,
	        verify)

	    if (ncols == 0) {
		call eprintf (
		    "\n<Error> Catalog %s in file %s has no id column\n\n")
		    call pargstr (Memc[title])
		    call pargstr (Memc[output])

	    } else if (ncols == 1) {
		call eprintf (
		   "\n<Error> Catalog %s in file %s has no data columns\n\n")
		    call pargstr (Memc[title])
		    call pargstr (Memc[output])
		ncols = 0

	   } else {
	       call printf ("\nCatalog %s in file %s has %d columns\n") 
		    call pargstr (Memc[title])
		    call pargstr (Memc[output])
		    call pargi (ncols)
	       do i = 1, ncols {
		   call printf ("\tColumn %d:  %*.*s\n")
		       call pargi (i)
		       call pargi (-Memi[widths+i-1])
		       call pargi (Memi[widths+i-1])
		       call pargstr (Memc[names+(i-1)* (MKCAT_MAXCOLWIDTH+1)])
	       }
	       call printf ("\n")
	   }

	    newfile = YES
	    review = NO

	} else {

	    fd = open (Memc[output], READ_WRITE, TEXT_FILE)
	    ncols = ph_open (fd, sz_record, Memc[otitle], SZ_FNAME) 

	    # The catalog was not created with MKCATALOG.
	    if (ncols <= 1) {
		call eprintf (
		   "\n<Warning> File %s was not created with MKCATALOG\n")
		    call pargstr (Memc[output])
		ncols = 0

	    # The user title does not match the catalog title.
	    } else if (strmatch (Memc[otitle], Memc[title]) <= 0) {
		call eprintf ("\n<Error> The input title %s does not match ")
		    call pargstr (Memc[title])
		call eprintf ("the catalog title %s.\n")
		    call pargstr (Memc[otitle])
		ncols = 0

	    # Read in the column names and widths.
	    } else {
		call malloc (names, MKCAT_MAXCOLWIDTH * ncols, TY_CHAR)
		call malloc (widths, ncols, TY_INT)
	        if (ph_rhdr (fd, Memc[names], MKCAT_MAXCOLWIDTH, Memi[widths],
		    ncols, sz_record) < ncols) {
		    call eprintf (
		        "\n<Error> Error reading catalog %s in file %s\n")
		        call pargstr (Memc[title])
			call pargstr (Memc[output])
		    ncols = 0
		} else {
	           call printf ("\nCatalog %s in file %s has %d columns\n") 
		        call pargstr (Memc[title])
		        call pargstr (Memc[output])
		        call pargi (ncols)
	           do i = 1, ncols {
		       call printf ("\tColumn %d:  %s\n")
		           call pargi (i)
		           call pargstr (Memc[names+(i-1)*(MKCAT_MAXCOLWIDTH+
			       1)])
	           }
	           call printf ("\n")
		}
	    }

	    newfile = NO
	    review = btoi (clgetb ("review"))
	}

	# Exit CATALOG if the output file exists but is not an IRAF
	# photometry catalog; if it has fewer than two columns, since
	# the first column must contain an identifier; or in the case of
	# a prexisting catalog if the user supplied title does not match
	# the catalog title.

	if (ncols <= 1) {
	    call sfree (sp)
	    return
	}

	# Create/edit the catalog. If the output catalog is a new file
	# write out the catalog header and query the user for the catalog
	# data. If the output catalog is a prexisting catalog read the
	# catalog header. If review is no then move to the end of the
	# catalog and query the user for new records. If review is yes
	# then open a temporary catalog, verify and copy each record to the
	# temporary file, query the user for new data, delete the original
	# catalog and rename the temporary catalog to the same name
	# as the original one.

	# Create a new catalog and accompanying format file.
	if (newfile == YES) {

	    # Read in the catalog data and write it to a file.
	    call ph_whdr (fd, Memc[title], Memc[names], MKCAT_MAXCOLWIDTH,
	        Memi[widths], ncols)
	    call ph_rdata (fd, Memc[names], MKCAT_MAXCOLWIDTH, Memi[widths],
	        ncols, verify)
	    call close (fd)

	    # Write out the format file.
	    call alimi (Memi[widths], ncols, imin, imax)
	    call fprintf (fdfmt,
	        "# Declare the catalog variables\n\n")
	    call fprintf (fdfmt, "catalog\n\n")
	    do i = 2, ncols {
		call fprintf (fdfmt, "%*.*s  %d\n")
		   call pargi (-imax)
		   call pargi (imax)
		   call pargstr (Memc[names+(i-1)*(MKCAT_MAXCOLWIDTH+1)])
		   call pargi (i)
	    }
	    call fprintf (fdfmt, "\n")
	    call close (fdfmt)
  
	# Review the existing records and enter new ones.
	} else if (review == YES) {

	    call mktemp ("tmp", Memc[toutput], SZ_FNAME)
	    tfd = open (Memc[toutput], NEW_FILE, TEXT_FILE)
	    call ph_whdr (tfd, Memc[title], Memc[names], MKCAT_MAXCOLWIDTH,
	        Memi[widths], ncols)
	    call ph_review (fd, tfd, Memc[names], MKCAT_MAXCOLWIDTH,
	        Memi[widths], ncols)
	    call ph_rdata (tfd, Memc[names], MKCAT_MAXCOLWIDTH, Memi[widths],
	        ncols, verify)
	    call close (fd)
	    call close (tfd)
	    call delete (Memc[output])
	    call rename (Memc[toutput], Memc[output])

	# Add new records to the end of an an existing catalog.
	} else {

	    call seek (fd, EOF)
	    call ph_rdata (fd, Memc[names], MKCAT_MAXCOLWIDTH, Memi[widths],
	        ncols, verify)
	    call close (fd)

	}

	# Release memory.
	if (names != NULL)
	    call mfree (names, TY_CHAR)
	if (widths != NULL)
	    call mfree (widths, TY_INT)
	call sfree (sp)
end


# PH_SETCOLS -- Decode the list of column names and widths into an
# array of column names and an array of column lengths.

int procedure ph_setcols (names, widths, max_lencolid, def_ncols, verify)

pointer	names		# pointer to array of column names
pointer	widths		# pointer to array of column widths
int	max_lencolid	# maximum length of column name not including EOS
int	def_ncols	# default initial number of columns
int	verify		# verify the input

int	i, bufsize, errcol, ncols, ival
pointer	sp, str, pnptr, nptr
bool	ph_isident()
int	scan(), nscan(), strmatch()

begin
	# Allocate temporary working space.
	call smark (sp)
	call salloc (str, max_lencolid, TY_CHAR)

	# Allocate space for the column name and width buffers.
	bufsize = def_ncols
	call malloc (names, (max_lencolid + 1) * bufsize, TY_CHAR)
	call malloc (widths, bufsize, TY_INT)

	call printf ("\n")

	ncols = 0
	errcol = NO
	pnptr = names
	nptr = names
	repeat {

	    # Print the column name prompt.
	    if (ncols == 0) {
		call printf ("Enter the id column name ")
		call printf ("(name, <CR>=ID, <EOF>=quit entry): ")
	    } else if (errcol == NO) {
		call printf ("Enter a name for column %d ")
		    call pargi (ncols+1)
		call printf ("(name, <CR>=COL%d, <EOF>=quit entry): ")
		    call pargi (ncols+1)
	    } else if (errcol == YES) {
		call printf ("Enter a name for error column %d ")
		    call pargi (ncols+1)
		call printf ("(name, <CR>=error(%s), <->=skip): ")
		    call pargstr (Memc[pnptr])
	    }
	    call flush (STDOUT)

	    # Terminate column definition on EOF.
	    if (scan () == EOF) {
		call printf ("\n")
		break
	    }

	    # Read the column name.
	    call gargwrd (Memc[nptr], max_lencolid)
	    if (Memc[nptr] == EOS) {
		if (ncols == 0) {
	    	    call strcpy ("ID", Memc[nptr], max_lencolid)
		} else if (errcol == NO) {
		    call sprintf (Memc[nptr], max_lencolid, "COL%d")
			call pargi (ncols+1)
		} else if (errcol == YES) {
		    call sprintf (Memc[nptr], max_lencolid, "error(%s)")
			call pargstr (Memc[pnptr])
		}
	    } else if (! ph_isident (Memc[nptr]) &&
	        strmatch (Memc[nptr], "error(") == 0) {
		if (Memc[nptr] == '-')
		    errcol = NO
		else {
		    call printf ("%s is not a legal column name - skipping\n")
		        call pargstr (Memc[nptr])
		}
		next
	    }

	    # Print the column width prompt.
	    if (ncols == 0) {
	        call printf ("\tEnter width of id column (width, <CR>=%d): ")
		    call pargi (MKCAT_IDCOLWIDTH)
	    } else {
	        call printf ("\tEnter width of column %d (width, <CR>=%d): ")
		    call pargi (ncols+1)
		    call pargi (MKCAT_COLWIDTH)
	    }
	    call flush (STDOUT)

	    # Read the column width.
	    if (ncols == 0)
		Memi[widths+ncols] = MKCAT_IDCOLWIDTH
	    else
		Memi[widths+ncols] = MKCAT_COLWIDTH
	    if (scan () != EOF) {
	        call gargi (ival)
	        if (nscan() == 1)
		    Memi[widths+ncols] = ival
	    }
	    
	    # Increment the buffer pointers.
	    ncols = ncols + 1
	    pnptr = nptr
	    nptr = names + ncols * (max_lencolid + 1)

	    # Is the next column an error column ?
	    if (ncols <= 1)
		errcol = NO
	    else if (errcol == YES)
		errcol = NO
	    else
		errcol = YES

	    # Expand the buffer space if necessary.
	    if (ncols < bufsize)
		next
	    bufsize = bufsize + def_ncols
	    call realloc (names, (max_lencolid + 1) * bufsize, TY_CHAR)
	    call realloc (widths, bufsize, TY_INT)
	} 

	call printf ("\n")

	# Do not verify if the verify switch is off or if there are fewer
	# than 2 columns in the catalog.

	if ((verify == NO) || (ncols < 2)) {
	    call sfree (sp)
	    return (ncols)
	}

	# Verify the name and width for each column.
	do i = 1, ncols {

	    nptr = names + (i - 1) * (max_lencolid + 1)

	    # Issue the verify prompt for the column name.
	    call printf ( "Verify name of column %d (%s) (name, <CR>=ok): ")
		call pargi (i)
		call pargstr (Memc[nptr])
	    call flush (STDOUT)

	    # Confirm old or read new column name value.
	    call strcpy (Memc[nptr], Memc[str], max_lencolid)
	    if (scan() == EOF)
		call printf ("\n")
	    else
		call gargwrd (Memc[nptr], max_lencolid)
	    if (Memc[nptr] == EOS)
		call strcpy (Memc[str], Memc[nptr], max_lencolid)
	    else if (! ph_isident (Memc[nptr]) && strmatch (Memc[nptr],
	        "error(") == 0)
		call strcpy (Memc[str], Memc[nptr], max_lencolid)
	    call printf ("\tOld name: %s  New name: %s\n")
		call pargstr (Memc[str])
		call pargstr (Memc[nptr])

	    # Issue the verify prompt for the column width.
	    call printf (
		"Verify the width of column %d (%d) (width, <CR>=ok): ")
		    call pargi (i)
		    call pargi (Memi[widths+i-1])
	    call flush (STDOUT)

	    # Confirm the old or read the new column width value.
	    ival = Memi[widths+i-1]
	    if (scan() != EOF) {
		call gargi (Memi[widths+i-1])
		if (nscan() != 1)
		    Memi[widths+i-1] = ival
	    }

	    call printf ("\tOld width: %d  New width: %d\n")
		call pargi (ival)
		call pargi (Memi[widths+i-1])

	}

	call printf ("\n")

	call sfree (sp)
	return (ncols)
end


# PH_OPEN -- Open an existing catalog, check that it is a valid
# photometry catalog, and return the title of the catalog and the number
# of columns in the catalog.

int procedure ph_open (fd, sz_record, otitle, max_lentitle)

int	fd		# the catalog file descriptor
int	sz_record	# the length of the catalog record
char	otitle[ARB]	# the original catalog title
int	max_lentitle	# the maximum length of the title

int	ncols
pointer	sp, str
int	fscan(), nscan(), strmatch()

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	otitle[1] = EOS
	ncols = 0
	sz_record = 0

	# Decode the title and the number of columns.
	while (fscan (fd) != EOF) {

	    # Check for the leading # sign. Quit if a blank line is
	    # encountered or the line does not begin with # followed
	    # by a blank.
	    call gargwrd (Memc[str], SZ_LINE) 
	    if (Memc[str] == EOS)
		break
	    if (Memc[str] != MKCAT_COMMENTCHAR)
		break

	    # Get the keyword.
	    call gargwrd (Memc[str], SZ_LINE) 
	    if (Memc[str] == EOS)
		next

	    # Decode the title and the number of columns.
	    if (strmatch (Memc[str], MKCAT_KYCATALOG) > 0) {
		call gargstr (otitle, max_lentitle)
	    } else if (strmatch (Memc[str], MKCAT_KYNCOLS) > 0) {
		call gargi (ncols)
	        if (nscan() != 3)
		    ncols = 0
	    } else if (strmatch (Memc[str], MKCAT_KYHDRLENGTH) > 0) {
		call gargi (sz_record)
	        if (nscan() != 3)
		    sz_record = 0
		break
	    }
	}

	call sfree (sp)
	return (ncols)
end


# PH_RHDR -- Read the names and widths of the catalog columns from
# the catalog header.

int procedure ph_rhdr (fd, names, max_lencolid, widths, ncols, sz_record)

int	fd			# file descriptor of the catalog
char	names[max_lencolid,ARB]	# the list of column names
int	max_lencolid		# the maximum length of the column names
int	widths[ARB]		# the list of column widths
int	ncols			# the number of catalog columns
int	sz_record		# the size of the catalog record

int	nc, ip
pointer	sp, record, wrd
int	ph_ghdr_record(), ctoi()

begin
	call smark (sp)
	call salloc (record, MAX_CONT * SZ_LINE, TY_CHAR)
	call salloc (wrd, max_lencolid, TY_CHAR)

	# Initialize the column names and widths.
	do nc = 1, ncols {
	    names[1,nc] = EOS 
	    widths[nc] = 0
	}

	# Decode the column names and widths.
	while (ph_ghdr_record (fd, Memc[record], sz_record) != EOF) {

	    # Scan the string.
	    call sscan (Memc[record])

	    # Check for the leading pound sign.
	    call gargwrd (Memc[wrd], max_lencolid) 
	    if (Memc[wrd] != MKCAT_COMMENTCHAR)
		break

	    # Skip lines that contain only a # sign.
	    call gargwrd (Memc[wrd], max_lencolid) 
	    if (Memc[wrd] == EOS)
		next

	    # Decode the columns widths and the column names.
	    do nc = 1, ncols {
		ip = 1
		if (names[1,nc] == EOS)
		    call strcpy (Memc[wrd], names[1,nc], max_lencolid)
		else if (ctoi (Memc[wrd], ip, widths[nc]) <= 0)
		    widths[nc] = 0
	        call gargwrd (Memc[wrd], max_lencolid) 
	        if (Memc[wrd] == EOS)
		    break
	    }
	}

	call sfree (sp)

	return (nc)
end


# PH_WHDR -- Write the header of the output catalog. All the header records
# begin with a # sign. A blank line signifies the end of the header block
# and the beginning of the data records.  The header block consists of
# two lines which define the title of the catalog and the number of columns
# followed by the column names and widths. 

procedure ph_whdr (fd, title, names, max_lencolid, widths, ncols)

int	fd			# output catalog file descriptor
char	title[ARB]		# title of the catalog
char	names[max_lencolid,ARB]	# array of column names
int	max_lencolid		# maximum width of the column id field
int	widths[ARB]		# array of column widths
int	ncols			# number of columns

int	i, ip, sz_hdr_record
pointer	sp, twidths, record

begin
	# Allocate space for an output record.
	call smark (sp)
	call salloc (twidths, ncols, TY_INT)
	call salloc (record, MAX_CONT * SZ_LINE, TY_CHAR)

	# Compute the size of a record.
	sz_hdr_record = 0
	do i = 1, ncols {
	    if (i == ncols)
		Memi[twidths+i-1] = widths[i]
	    else
		Memi[twidths+i-1] = widths[i] + MKCAT_SZGAP
	    sz_hdr_record = sz_hdr_record + Memi[twidths+i-1]
	}

	# Write out the header banner.
	call fprintf (fd, "%s%s %s\n")
	    call pargstr (MKCAT_COMMENTSTR)
	    call pargstr (MKCAT_KYCATALOG)
	    call pargstr (title)
	call fprintf (fd, "%s%s %d\n")
	    call pargstr (MKCAT_COMMENTSTR)
	    call pargstr (MKCAT_KYNCOLS)
	    call pargi (ncols)
	call fprintf (fd, "%s%s %d\n")
	    call pargstr (MKCAT_COMMENTSTR)
	    call pargstr (MKCAT_KYHDRLENGTH)
	    call pargi (sz_hdr_record)
	call fprintf (fd, "%s\n")
	    call pargstr (MKCAT_COMMENTSTR)

	# Construct the column labels record.
	ip = 1
	do i = 1, ncols {
	    call sprintf (Memc[record+ip-1], max_lencolid, "%*.*s")
		call pargi (-Memi[twidths+i-1])
		call pargi (widths[i])
		call pargstr (names[1,i])
	    ip = ip + Memi[twidths+i-1]
	}

	# Write the label record to the output file.
	call ph_pdrecord (fd, Memc[record], sz_hdr_record, Memi[twidths], ncols,
	    MKCAT_COMMENTSTR,  SZ_LINE - 1)

	# Construct the column widths record.
	ip = 1
	do i = 1, ncols {
	    call sprintf (Memc[record+ip-1], max_lencolid, "%*.*d")
		call pargi (-Memi[twidths+i-1])
		call pargi (widths[i])
		call pargi (widths[i])
	    ip = ip + Memi[twidths+i-1]
	}

	# Write the column widths record to the output file.
	call ph_pdrecord (fd, Memc[record], sz_hdr_record, Memi[twidths], ncols,
	    MKCAT_COMMENTSTR, SZ_LINE - 1)

	# Append a trailing blank line to indicate the end of the header.
	call fprintf (fd, "\n")

	call sfree (sp)
end


# PH_RDATA -- Read in new catalog data from the standard input.

procedure ph_rdata (fd, names, max_lencolid, widths, ncols, verify)

int	fd			# pointer to the output catalog
char	names[max_lencolid,ARB]	# array of column names
int	max_lencolid		# maximum length of a column name
int	widths[ARB]		# array of column widths
int	ncols			# number of columns
int	verify			# verify each new entry

int	ip, i, rip, stat, sz_record
pointer	sp, twidths, wrd, twrd, record, trecord
int	scan(), ctowrd()

begin
	# Allocate working memory.
	call smark (sp)
	call salloc (twidths, ncols, TY_INT)
	call salloc (wrd, max_lencolid, TY_CHAR)
	call salloc (twrd, max_lencolid, TY_CHAR)
	call salloc (record, MAX_CONT * SZ_LINE, TY_CHAR)
	call salloc (trecord, MAX_CONT * SZ_LINE, TY_CHAR)

	# Compute the size of a record.
	sz_record = 0
	do i = 1, ncols {
	    if (i == ncols)
		Memi[twidths+i-1] = widths[i]
	    else
		Memi[twidths+i-1] = widths[i] + MKCAT_SZGAP
	    sz_record = sz_record + Memi[twidths+i-1]
	}

	# Read in records from the standard input.
	repeat {

	    # Intialize the record.
	    ip = 1
	    Memc[record] = EOS

	    # Read in each record.
	    do i = 1, ncols {

	        # Issue the column values prompt.
	        if (i == 1) {
		    call printf (
		    "\nEnter %s (value, <CR>=blank line, <EOF>=quit entry): ")
	        } else {
		    call printf ("\tEnter %s (value, <CR>=INDEF): ")
	        }
		        call pargstr (names[1,i])
	        call flush (STDOUT)

	        # Read in the new value.
	        stat = scan()
		if (stat == EOF) {
		    if (i == 1)
		        break
		    else
			Memc[wrd] = EOS
		} else
	            call gargstr (Memc[wrd], max_lencolid)

		# Fill in unknown values with INDEF.
	        if (Memc[wrd] == EOS) {
		    if (i == 1)
			break
		    else
		        call strcpy ("INDEF", Memc[wrd], max_lencolid)
		}

		# Format the output record.
		call sprintf (Memc[record+ip-1], max_lencolid, "%*.*s")
		    call pargi (-Memi[twidths+i-1])
		    call pargi (widths[i])
		    call pargstr (Memc[wrd])
		ip = ip + Memi[twidths+i-1]
	    }

	    # EOF terminates input.
	    if (stat == EOF)
		break

	    # Write out blank lines.
	    if (Memc[record] == EOS) {
		call fprintf (fd, "\n")
		    next
	    }

	    # Write out the record at this point if verify is off.
	    if (verify == NO) {
		call ph_pdrecord (fd, Memc[record], sz_record, Memi[twidths],
		    ncols, MKCAT_BLANKSTR, SZ_LINE - 1)
		next
	    }

	    # Verify.
	    ip = 1
	    rip = 1
	    Memc[trecord] = EOS

	    do i = 1, ncols {

		if (ctowrd (Memc[record], rip, Memc[twrd], max_lencolid) <= 0)
		    break

		# Issue a prompt to the user.
	        if (i == 1)
		    call printf ("\nVerify %s (%s) (value, <CR>=ok): ")
	        else
		    call printf ("\tVerify %s (%s) (value, <CR>=ok): ")
		        call pargstr (names[1,i])
		        call pargstr (Memc[twrd])
	        call flush (STDOUT)

		# Scan the standard input for a new value.
		if (scan() == EOF) {
		    call printf ("\n")
		    Memc[wrd] = EOS
		} else
	            call gargstr (Memc[wrd], max_lencolid)
		if (Memc[wrd] == EOS)
		    call strcpy (Memc[twrd], Memc[wrd], max_lencolid)
		else {
		    call printf ("\t%s old: %s new: %s\n")
			call pargstr (names[1,i])
			call pargstr (Memc[twrd])
			call pargstr (Memc[wrd])
		}

		# Format the verified output record.
		call sprintf (Memc[trecord+ip-1], max_lencolid, "%*.*s")
		    call pargi (-Memi[twidths+i-1])
		    call pargi (widths[i])
		    call pargstr (Memc[wrd])
		ip = ip + Memi[twidths+i-1]
	    }
	    
	    # Write out the verified record.
	    call ph_pdrecord (fd, Memc[trecord], sz_record, Memi[twidths],
	        ncols, MKCAT_BLANKSTR, SZ_LINE - 1)
	}

	call printf ("\n")
	call sfree (sp)
end


# PH_REVIEW -- Review the previous catalog entries and make changes.

procedure ph_review (fd, tfd, names, max_lencolid, widths, ncols)

int	fd			# pointer to the original output catalog
int	tfd			# pointer to the temporary output catalog
char	names[max_lencolid,ARB]	# array of column names
int	max_lencolid		# maximum length of a column name
int	widths[ARB]		# array of column widths
int	ncols			# number of columns

int	stat, i, ip, lip, sz_record
pointer	sp, twidths, record, trecord, wrd, twrd
int	ph_gdrecord(), strncmp(), scan(), ctowrd()

begin
	call smark (sp)
	call salloc (twidths, ncols, TY_INT)
	call salloc (record, MAX_CONT * SZ_LINE, TY_CHAR)
	call salloc (trecord, MAX_CONT * SZ_LINE, TY_CHAR)
	call salloc (wrd, max_lencolid, TY_CHAR)
	call salloc (twrd, max_lencolid, TY_CHAR)

	# Compute the size of a record. This number is equal to the sum
	# of all the column widths plus the the inter-column gap which
	# is normally two characters.

	sz_record = 0
	do i = 1, ncols {
	    if (i == ncols)
		Memi[twidths+i-1] = widths[i]
	    else
		Memi[twidths+i-1] = widths[i] + MKCAT_SZGAP
	    sz_record = sz_record + Memi[twidths+i-1]
	}

	# Initialize the catalog data record reading routine.

	call ph_gdrecord_init()

	# Loop over the records in the original output catalog.
	repeat {

	    # Fetch a record from the original output catalog. If the
	    # record is blank output a blank line to the temporary
	    # output catalog.  If the record is a comment simply copy
	    # it to the temporary output catalog.

	    if (ph_gdrecord (fd, Memc[record], MAX_CONT * SZ_LINE) == EOF)
		break
	    if (Memc[record] == EOS || Memc[record] == '\n') {
		call fprintf (tfd, "\n")
		next
	    } else if (strncmp (MKCAT_COMMENTSTR, Memc[record],
	        MKCAT_SZSTR) == 0) {
		call fprintf (tfd, "%s\n")
		    call pargstr (Memc[record])
		next
	    }

	    # Initialize each new record.
	    lip = 1
	    ip = 1
	    Memc[trecord] = EOS

	    # Loop over the columns.
	    do i = 1, ncols {

		# Decode the record skipping blank lines.
		if (ctowrd (Memc[record], lip, Memc[twrd], max_lencolid) <= 0)
		    break

	        # Issue the review prompt.
	        if (i == 1) {
		    call printf (
		"\nReview OBJECT %s (%s) (value, <CR>=ok, <EOF>=quit review): ")
	        } else {
		    call printf ("\tReview %s (%s) (value, <CR>=ok): ")
		}
		    call pargstr (names[1,i])
		    call pargstr (Memc[twrd])
	        call flush (STDOUT)

	        # Scan the standard input for a corrected value.
	        stat = scan()
		if (stat == EOF) {
		    if (i == 1)
		        break
		    else
			Memc[wrd] = EOS
		} else
	            call gargstr (Memc[wrd], max_lencolid)
	        if (Memc[wrd] == EOS)
		    call strcpy (Memc[twrd], Memc[wrd], max_lencolid)
		else {
		    call printf ("\t%s old: %s new: %s\n")
			call pargstr (names[1,i])
			call pargstr (Memc[twrd])
			call pargstr (Memc[wrd])
		}

		# Format the output record.
		call sprintf (Memc[trecord+ip-1], max_lencolid, "%*.*s")
		    call pargi (-Memi[twidths+i-1])
		    call pargi (widths[i])
		    call pargstr (Memc[wrd])
		ip = ip + Memi[twidths+i-1]
	    }

	    # Output the current record.
	    if (stat == EOF) {
		call ph_pdrecord (tfd, Memc[record+MKCAT_SZSTR], sz_record,
		    Memi[twidths], ncols, MKCAT_BLANKSTR, SZ_LINE - 1)
		break
	    } else if (Memc[trecord] == EOS)
	        call fprintf (tfd, "\n")
	    else
		call ph_pdrecord (tfd, Memc[trecord], sz_record, Memi[twidths],
		    ncols, MKCAT_BLANKSTR, SZ_LINE - 1)
	}

	# Copy any remaining records that were not reviewed.
	while (ph_gdrecord (fd, Memc[record], MAX_CONT * SZ_LINE) != EOF) {
	    if (Memc[record] == EOS)
		call fprintf (tfd, "\n")
	    else
	        call ph_pdrecord (tfd, Memc[record+MKCAT_SZSTR], sz_record,
		    Memi[twidths], ncols, MKCAT_BLANKSTR, SZ_LINE - 1)
	}

	call printf ("\n")
	call sfree (sp)
end
