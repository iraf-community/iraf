include <fset.h>
include <ctotok.h>

define	IMG_DEFSZSET	15
define	IMG_DEFSZNAME	79

# Reformat a file containing a list of images into an image set file using
# a set definition field and a list of permitted set definition values.
# The input image list is assumed to have the image name in column 1 and the
# value of the set definition field in column 2. All the rest of the columns
# are ignored.  The image set definition field is checked against the list
# of permitted set defintion field values and the image is rejected if the
# two do not match. The number of permitted values defines the size of each
# set.

procedure t_imgroup ()

pointer	imlist			# pointer to the input image list
pointer	imsetlist		# pointer to the output image set
pointer	setvals			# pointer to the permitted list of set values
int	rename 			# prompt the user for image set names

int	in, out, nvalues
pointer	sp, valnames
bool	clgetb()
int	btoi(), open(), ph_listset()

begin
	call fseti (STDOUT, F_FLUSHNL, YES)

	# Allocate working space.
	call smark (sp)
	call salloc (imlist, SZ_FNAME, TY_CHAR)
	call salloc (imsetlist, SZ_FNAME, TY_CHAR)
	call salloc (setvals, SZ_FNAME, TY_CHAR)

	# Get the parameters.
	call clgstr ("imlist", Memc[imlist], SZ_FNAME)
	call clgstr ("imsetlist", Memc[imsetlist], SZ_FNAME)
	call clgstr ("setvalues", Memc[setvals], SZ_FNAME)
	rename = btoi (clgetb ("rename"))

	# Open the input and output files.
	in = open (Memc[imlist], READ_ONLY, TEXT_FILE)
	out = open (Memc[imsetlist], NEW_FILE, TEXT_FILE)

	# Group the sets.
	valnames = NULL
	nvalues = ph_listset (Memc[setvals], valnames, IMG_DEFSZNAME,
	    IMG_DEFSZSET)
	if (nvalues > 0)
	    call ph_group (in, out, Memc[setvals], Memc[valnames],
		    IMG_DEFSZNAME, nvalues, rename)
	else
	    call eprintf (
	        "<Error>: The list of permitted set values is empty\n")

	# Close files.
	call close (in)
	call close (out)

	if (valnames != NULL)
	    call mfree (valnames, TY_CHAR)
	call sfree (sp)
end


# PH_LISTSET -- Extract the individual set defintion values.

int procedure ph_listset (setvals, values, max_lvalue, def_nvalues)

char	setvals[ARB]	        # the list of set values
pointer	values			# pointer to the array of extracted names
int	max_lvalue		# maximum length of a value
int	def_nvalues		# the default number of set values

int	ip, tp, op, token, nc, bufsize, vptr
pointer	sp, temp
int	ctowrd(), ctotok(), strlen()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (temp, SZ_LINE, TY_CHAR)
	bufsize = def_nvalues
	call malloc (values, (max_lvalue + 1) * bufsize, TY_CHAR)

	# Decode the individual values.
	nc = 0
	ip = 1
	vptr = values
	while (setvals[ip] != EOS) {

	    # Extract whitespace separated strings.
	    if (ctowrd (setvals, ip, Memc[temp], SZ_LINE) <= 0)
		break

	    # Initialize token extraction.
	    tp = 1
	    op = 1

	    # Check the extracted string for imbedded punctuation.
	    # Only commas and semi-colons are recognized.

	    while (Memc[temp+tp-1] != EOS) {
	        token = ctotok (Memc[temp], tp, Memc[vptr+op-1], max_lvalue)
                if (Memc[vptr+op-1] == EOS)
                    next
		if (token == TOK_UNKNOWN || token == TOK_CHARCON)
		    next
		if ((token == TOK_PUNCTUATION) && (Memc[vptr+op-1] == ',' ||
		    Memc[vptr+op-1] == ';')) {
		    Memc[vptr+op-1] = EOS
		    nc = nc + 1
		    op = 1
		} else
		    op = op + strlen (Memc[vptr+op-1])
		vptr = values + nc * (max_lvalue + 1)
	        if (nc < def_nvalues)
		    next
	        bufsize = bufsize + def_nvalues
	        call realloc (values, (max_lvalue + 1) * bufsize, TY_CHAR)
	    }

	    if (op > 1)
	        nc = nc + 1
	    vptr = values + nc * (max_lvalue + 1)
	    if (nc < def_nvalues)
		next
	    bufsize = bufsize + def_nvalues
	    call realloc (values, (max_lvalue + 1) * bufsize, TY_CHAR)
	}

	call sfree (sp)

	# Return the number of values successfully decoded.
	call realloc (values, (max_lvalue + 1) * nc, TY_CHAR)
	return (nc)
end


# PH_GROUP -- Group the data into sets.

procedure ph_group (in, out, filters, values, max_lvalue, nvalues, rename)

int	in			# the input file descriptor
int	out			# the output file descriptor
char	filters[ARB]		# the filter set
char	values[max_lvalue,ARB]	# list of permitted set values
int	max_lvalue		# maximum length of a set value
int	nvalues			# the number of permitted values
int	rename			# prompt for the image set names

int	i, numindex, ns, nsets, op, num
pointer	sp, image, setval, name, tname, index, record
int	fscan(), nscan(), ph_member(), gstrcpy()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (setval, SZ_LINE, TY_CHAR)
	call salloc (tname, SZ_FNAME, TY_CHAR)
	call salloc (name, SZ_FNAME, TY_CHAR)
	call salloc (index, nvalues, TY_INT)
	call salloc (record, SZ_LINE, TY_CHAR)

	# Issue explanatory message.
	if (rename == YES) {
	    call printf ("\n")
	    call printf ("Enter/Confirm Names for Each Image Set\n")
	    call printf ("\n")
	}

	# Initialize.
	ns = 0
	nsets = 0
	op = 1
	call amovki (0, Memi[index], nvalues)

	# Read the input file.
	while (fscan (in) != EOF) {

	    # Get the image name and set definition from columns 1 and 2.
	    call gargwrd (Memc[image], SZ_FNAME)
	    call gargwrd (Memc[setval], SZ_LINE)
	    if (nscan() < 1)
		next
	    if (Memc[image] == '#')
		next
	    if (nscan() < 2)
		Memc[setval] = EOS

	    # Check for set membership.
	    if ((nvalues == 1) && (values[1,1] == EOS))
		num = 1
	    else {
	        num = ph_member (Memc[setval], values, max_lvalue, nvalues)
	        if (num == 0) {
		    call eprintf ("<Warning>: Image %s filter id %s ")
			call pargstr (Memc[image])
			call pargstr (Memc[setval])
		    call eprintf (
		        " does not belong to the filter set %s\n")
			call pargstr (filters)
		    next
		}
	    }

	    # Check to see if a particular slot is already filled.
	    # Terminate the set if a duplicate set member is found.

	    numindex = Memi[index+num-1]
	    if (numindex != 0) {
		call eprintf ("<Warning> Set %d image %s has the same ")
		    call pargi (nsets + 1)
		    call pargstr (Memc[image])
		call eprintf ("filter id %s as image %s\n")
		    call pargstr (Memc[setval])
		    call pargstr (Memc[record+numindex-1])
		do i = 1, nvalues {
		    if (Memi[index+i-1] != 0)
			next
		    Memi[index+i-1] = op
		    op = op + gstrcpy ("INDEF", Memc[record+op-1],
		        SZ_LINE - op + 1) + 1
		    ns = ns + 1
		}
	    } else {
		Memi[index+num-1] = op
	        ns = ns + 1
		op = op + gstrcpy (Memc[image], Memc[record+op-1],
		    SZ_LINE - op + 1) + 1
	    }

	    # Write out the new set and begin working on the next set.
	    if (ns == nvalues) {

		nsets = nsets + 1

		# Get the set name.
		call sprintf (Memc[tname], SZ_LINE, "OBS%d")
		    call pargi (nsets)
		if (rename == YES) {
	            call ph_swrite (STDOUT, Memc[tname], Memc[record],
		        Memi[index], nvalues)
		    call ph_rname (Memc[tname], Memc[name], SZ_LINE)
		} else
		    call strcpy (Memc[tname], Memc[name], SZ_LINE)

		# Write out the new set.
		call ph_swrite (out, Memc[name], Memc[record], Memi[index], 
		    nvalues)

		# Reinitialize.
		ns = 0
		op = 1
		call amovki (0, Memi[index], nvalues)
	    }

	    if (numindex != 0) {
		Memi[index+num-1] = op
	        ns = ns + 1
		op = op + gstrcpy (Memc[image], Memc[record+op-1],
		    SZ_LINE - op + 1) + 1
	    }
	}

	# Write out the last record to the output file.
	if (ns > 0) {

	    # Fill in the missing values.
	    do i = 1, nvalues {
		if (Memi[index+i-1] != 0)
		    next
		Memi[index+i-1] = op
		op = op + gstrcpy ("INDEF", Memc[record+op-1],
		    SZ_LINE - op + 1) + 1
	    }

	    nsets = nsets + 1

	    # Prompt for the image set name.
	    call sprintf (Memc[tname], SZ_LINE, "OBS%d")
		call pargi (nsets)
	    if (rename == YES) {
	        call ph_swrite (STDOUT, Memc[tname], Memc[record], Memi[index],
	            nvalues)
		call ph_rname (Memc[tname], Memc[name], SZ_LINE)
	    } else
		call strcpy (Memc[tname], Memc[name], SZ_LINE)

	    # Write the record.
	    call ph_swrite (out, Memc[name], Memc[record], Memi[index],
	        nvalues)
	}

	# Warn the user if the set is empty.
	if (nsets <= 0)
	    call eprintf ("\n<Warning> The image set file is empty\n\n")
	else
	    call eprintf ("\n")

	call sfree (sp)
end


# PH_MEMBER -- Test whether the string belongs to a permitted list
# of values.

int procedure ph_member (setval, values, max_lvalue, nvalues)

char	setval[ARB]		# the set value
char	values[max_lvalue,ARB]	# the permitted list of possible set values
int	max_lvalue		# maximum length of the set value
int	nvalues			# the number of set values

int	nc, i
bool	streq()

begin
	nc = 0
	do i = 1, nvalues {
	    if (streq (setval, values[1,i]))
		nc = i
	}

	return (nc)
end


# PH_SWRITE -- Write out the finshed set to a file.

procedure ph_swrite (out, name, record, index, nvalues)

int	out		# output file descriptor
char	name[ARB]	# the name of the output field
char	record[ARB]	# the name of the output record
int	index[ARB]	# array of indices
int	nvalues		# number of values

pointer	sp, str
int	i

begin
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)

	call fprintf (out, "%s :")
	    call pargstr (name)

	do i = 1, nvalues {
	    call ph_imroot (record[index[i]], Memc[str], SZ_FNAME)
	    call fprintf (out, "  %s")
		#call pargstr (record[index[i]])
		call pargstr (Memc[str])
	}

	call fprintf (out, "\n")

	call sfree (sp)
end


# PH_RNAME -- Prompt the user for a field name.

procedure ph_rname (tname, name, max_lname)

char	tname[ARB]		# temporary input name
char	name[ARB]		# the final output name
int	max_lname		# the maximum length of the name

int	scan()

begin
	# Issue a prompt to the user
	call printf ("\tEnter new name for field %s (name, <CR>=ok): ")
	    call pargstr (tname)
	call flush (STDOUT)

	# Get the new value
	if (scan () == EOF)
	    name[1] = EOS
	else
	    call gargwrd (name, max_lname)
	if (name[1] == EOS)
	    call strcpy (tname, name, max_lname)

	call printf ("\n")
end


# PH_IMROOT -- Fetch the root image name minus the directory specification
# and the section notation.

procedure ph_imroot (image, root, maxch)

char    image[ARB]              # image specification
char    root[ARB]               # output root name
int     maxch                   # maximum number of characters

pointer sp, imroot, kernel, section, str
int     clindex, clsize, nchars
int     fnldir()

begin
        call smark (sp)
        call salloc (imroot, SZ_PATHNAME, TY_CHAR)
        call salloc (kernel, SZ_FNAME, TY_CHAR)
        call salloc (section, SZ_FNAME, TY_CHAR)
        call salloc (str, SZ_PATHNAME, TY_CHAR)

        call imparse (image, Memc[imroot], SZ_PATHNAME, Memc[kernel], SZ_FNAME,
            Memc[section], SZ_FNAME, clindex, clsize)
        nchars = fnldir (Memc[imroot], Memc[str], SZ_PATHNAME)
	if (clindex >= 0) {
            call sprintf (root, maxch, "%s[%d]%s%s")
                call pargstr (Memc[imroot+nchars])
		call pargi (clindex)
                call pargstr (Memc[kernel])
                call pargstr (Memc[section])
	} else {
            call sprintf (root, maxch, "%s%s%s")
                call pargstr (Memc[imroot+nchars])
                call pargstr (Memc[kernel])
                call pargstr (Memc[section])
	}

        call sfree (sp)
end
