include <ctype.h>
include <fset.h>
include <lexnum.h>

# define some of the working space parameters

define	CFG_MAXLENEQN	SZ_LINE         # maximum length of an expression
define	CFG_MAXLENLINE	78		# maximum length of an output line
define	CFG_MAXLENTRANS	16000		# maximum length of transformation area
define	CFG_MAXLENPARAM	160		# maximum length of parameter area

# define the internal help pages

define	CFG_CATHELP	"photcal$mkconfig/catsection.key"
define	CFG_OBSHELP	"photcal$mkconfig/obsection.key"
define	CFG_TRANSHELP	"photcal$mkconfig/transection.key"

# T_CONFIG -- Create the configuration file.

procedure t_config()

pointer	output		# pointer to the output configuration file
pointer	template	# pointer to template configuration file
pointer	catalog		# pointer to the template catalog section file
pointer	observation	# pointer to the template observation section file
pointer	transform	# pointer to the template transformation section file
pointer	deriv		# pointer to the derivative syntax string
int	verify		# verify each user entry

int	fd, cfd, ofd, tfd
pointer	sp
bool	clgetb()
int	access(), open(), btoi()

begin
	# Set the standard output to flush on a newline.
	call fseti (STDOUT, F_FLUSHNL, YES)

	# Allocate temporary space.
	call smark (sp)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (template, SZ_FNAME, TY_CHAR)
	call salloc (catalog, SZ_FNAME, TY_CHAR)
	call salloc (observation, SZ_FNAME, TY_CHAR)
	call salloc (transform, SZ_FNAME, TY_CHAR)
	call salloc (deriv, SZ_FNAME, TY_CHAR)

	# Get the parameters.
	call clgstr ("config", Memc[output], SZ_FNAME)
	verify = btoi (clgetb ("verify")) 

	# Make the configuration file. If a template configuration file
	# is supplied and exists then simply copy the template into the
	# output configuration file.  If no template is supplied begin
	# prompting user for input. The user may supply a default
	# catalog, observation or transformation section file in place of
	# entering any given section by hand.

	call clgstr ("template", Memc[template], SZ_FNAME)
	if (access (Memc[template], 0, 0) == YES) {
	    call fcopy (Memc[template], Memc[output])
	} else {

	    fd = open (Memc[output], NEW_FILE, TEXT_FILE)

	    call clgstr ("catalog", Memc[catalog], SZ_FNAME)
	    if (access (Memc[catalog], READ_ONLY, TEXT_FILE) == YES)
		cfd = open (Memc[catalog], READ_ONLY, TEXT_FILE)
	    else
		cfd = NULL
	    call clgstr ("observations", Memc[observation], SZ_FNAME)
	    if (access (Memc[observation], READ_ONLY, TEXT_FILE) == YES)
		ofd = open (Memc[observation], READ_ONLY, TEXT_FILE)
	    else
		ofd = NULL
	    call clgstr ("transform", Memc[transform], SZ_FNAME)
	    if (access (Memc[transform], READ_ONLY, TEXT_FILE) == YES)
		tfd = open (Memc[transform], READ_ONLY, TEXT_FILE)
	    else
		tfd = NULL

	    call ph_wconfig (fd, cfd, ofd, tfd, verify)

	    call close (fd)
	    if (cfd != NULL)
		call close (cfd)
	    if (ofd != NULL)
		call close (ofd)
	    if (tfd != NULL)
		call close (tfd)
	}

	call sfree (sp)
end


# PH_WCONFIG -- Write the catalog, observation and transfromation section
# of the configuration file by prompting the user. If the file descriptor
# to the transformation file is not NULL then copy the template
# transformation file into the tranformation section of the configuration
# file.

procedure ph_wconfig (fd, cfd, ofd, tfd, verify)

int	fd		# the output configuration file descriptor
int	cfd		# the input catalog file descriptor
int	ofd		# the input observation file descriptor
int	tfd		# the input transformation file descriptor
int	verify		# verify each user entry

begin
	# Enter the catalog section.
	if ((cfd != NULL) && (cfd != STDIN))
	    call fcopyo (cfd, fd)
	else
	    call ph_rcsection (fd, "catalog", verify)

	# Enter the observation section.
	if ((ofd != NULL) && (ofd != STDIN))
	    call fcopyo (ofd, fd)
	else
	    call ph_rosection (fd, "observation", verify)

	# Enter the transformation section.
	if ((tfd != NULL) && (tfd != STDIN))
	    call fcopyo (tfd, fd)
	else
	    call ph_rtsection (fd, "transformation", verify)
end


# PH_RCSECTION -- Write the catalog section of the configuration file by
# prompting the user.

procedure ph_rcsection (fd, keyword, verify)

int	fd		# file descriptor for the output configuration file
char	keyword[ARB]	# keyword defining the configuration file section
int	verify		# verify each user entry

int	stat, number, newnumber
pointer	sp, column, newcolumn
int	ph_rcolnumber()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (column, SZ_LINE, TY_CHAR)
	call salloc (newcolumn, SZ_LINE, TY_CHAR)

	# Write the section title.
	call fprintf (fd, "\n%s\n\n")
	    call pargstr (keyword)

	# Scan the standard input.
	call printf (
	    "\nENTER THE STANDARD STAR CATALOG FORMAT DESCRIPTION\n\n")
	repeat {

	    # Read the column name and number.
	    stat = ph_rcolnumber (CFG_CATHELP, Memc[column], SZ_LINE, number)
	    if (stat == EOF)
		break
	    if (stat <= 0)
		next

	    # Verify and/or write the definition.
	    if (verify == NO) {

		# Write the definition.
	        call fprintf (fd, "%s  %d\n")
		    call pargstr (Memc[column])
		    call pargi (number)
		next

	    } else {

		call ph_vcolnumber (Memc[column], Memc[newcolumn], SZ_LINE,
		    number, newnumber)
	        call fprintf (fd, "%s  %d\n")
		    call pargstr (Memc[newcolumn])
		    call pargi (newnumber)

	    }
	}
	call printf ("\n")

	call sfree (sp)
end


# PH_ROSECTION -- Write the observation section of the configuration file by
# prompting the user.

procedure ph_rosection (fd, keyword, verify)

int	fd		# file descriptor for the output configuration file
char	keyword[ARB]	# keyword defining the configuration file section
int	verify		# verify each user entry

int	stat, number, newnumber
pointer	sp, column, newcolumn
int	ph_rcolnumber()

begin
	call smark (sp)
	call salloc (column, SZ_LINE, TY_CHAR)
	call salloc (newcolumn, SZ_LINE, TY_CHAR)

	# Write the section title.
	call fprintf (fd, "\n%s\n\n")
	    call pargstr (keyword)

	# Scan the standard input.
	call printf ("\nENTER THE OBSERVATIONS FILE FORMAT DESCRIPTION\n\n")
	repeat {

	    # Read the column name and number.
	    stat = ph_rcolnumber (CFG_OBSHELP, Memc[column], SZ_LINE, number)
	    if (stat == EOF)
		break
	    if (stat <= 0)
		next

	    # Verify and/or write the definition.
	    if (verify == NO) {

	        call fprintf (fd, "%s  %d\n")
		    call pargstr (Memc[column])
		    call pargi (number)
		next

	    } else {

		call ph_vcolnumber (Memc[column], Memc[newcolumn], SZ_LINE,
		    number, newnumber)
	        call fprintf (fd, "%s  %d\n")
		    call pargstr (Memc[newcolumn])
		    call pargi (newnumber)

	    }
	}
	call printf ("\n")

	call sfree (sp)
end


# PH_RTSECTION -- Write the observation section of the configuration file by
# prompting the user.

procedure ph_rtsection (fd, keyword, verify)

pointer	fd		# file descriptor for the output configuration file
char	keyword[ARB]	# keyword defining the section
int	verify		# verify each user entry

int	tfd, ffd, cfd, len_label, len_expr, len_dexpr
int	neq, nparam, stat
pointer	sp, label, param, str, expr, dexpr, trans, fit, const
int	ph_rlabel(), ph_rexpr(), ph_rlist(), stropen()

begin
	# Allocate temporary working space.
	call smark (sp)
	call salloc (label, SZ_LINE, TY_CHAR)
	call salloc (param, SZ_LINE, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (expr, CFG_MAXLENEQN, TY_CHAR)
	call salloc (dexpr, CFG_MAXLENEQN, TY_CHAR)

	# Open the temporary string files.
	call malloc (trans, CFG_MAXLENTRANS, TY_CHAR)
	call malloc (fit, CFG_MAXLENPARAM, TY_CHAR)
	call malloc (const, CFG_MAXLENTRANS, TY_CHAR)
	tfd = stropen (Memc[trans], CFG_MAXLENTRANS, NEW_FILE)
	ffd = stropen (Memc[fit], CFG_MAXLENPARAM, NEW_FILE)
	cfd = stropen (Memc[const], CFG_MAXLENPARAM, NEW_FILE)

	# Write the section title.
	call fprintf (fd, "\n%s\n\n")
	    call pargstr (keyword)

	call printf ("\nENTER THE TRANSFORMATION EQUATIONS\n")

	# Scan the standard input.
	neq =  0
	repeat {

	    # Get the parameters for each equation.
	    call printf (
	        "\nEnter the label and functional form for EQUATION %d\n\n")
		call pargi (neq + 1)

	    # Fetch and verify the equation label.
	    len_label = ph_rlabel ("label (e.g. VFIT)", Memc[label],
	        SZ_LINE, verify)
	    if (len_label == EOF)
		break
	    if (len_label <= 0)
		next

	    # Fetch and verify the transformation equation.
	    len_expr = ph_rexpr ("equation", Memc[expr], CFG_MAXLENEQN, neq + 1,
	        verify)
	    if (len_expr == EOF)
		next
	    if (len_expr <= 0) {
		call printf ("<Error> The function expression is blank\n")
		next
	    }

	    # Write the equation to a temporary string file.
	    iferr {
	        call ph_wequation (tfd, Memc[label], len_label, Memc[expr],
	            len_expr)
	    } then
		break

	    # Get the fitted parameters.
	    call printf ("\nEnter initial values for the ")
	    call printf ("parameters to be fit in EQUATION %d\n\n")
		call pargi (neq + 1)
	    nparam = 1
	    len_dexpr = 1
	    repeat {
		call sprintf (Memc[str], SZ_LINE, "%s %d")
		    call pargstr ("parameter")
		    call pargi (nparam)
		stat = ph_rlist (Memc[dexpr], len_dexpr, CFG_MAXLENEQN,
		    Memc[str],  verify)
	        if (stat == EOF)
	            break
		if (stat <= 0) {
		    call printf ("\n")
	    	    call ph_wequation (STDOUT, Memc[label], len_label,
		        Memc[expr], len_expr)
		    call printf ("\n")
		    next
		} else
		    nparam = nparam + 1
	    } 

	    # Write the fitted parameters.
	    iferr {
	        if (nparam > 1)
		    call ph_wparam (ffd, "fit", 3, Memc[dexpr], len_dexpr - 3)
	    } then
		break

	    # Get the constant parameters. 
	    call printf ("\nEnter initial values for the ")
	    call printf ("parameters to be held constant in EQUATION %d\n\n")
		call pargi (neq + 1)
	    nparam = 1
	    len_dexpr = 1
	    repeat {
		call sprintf (Memc[str], SZ_LINE, "%s%d and value")
		    call pargstr ("parameter")
		    call pargi (nparam)
		stat = ph_rlist (Memc[dexpr], len_dexpr, CFG_MAXLENEQN,
		    Memc[str], verify)
	        if (stat == EOF)
	            break
		if (stat <= 0) {
		    call printf ("\n")
	    	    call ph_wequation (STDOUT, Memc[label], len_label,
		        Memc[expr], len_expr)
		    call printf ("\n")
		    next
		} else
		    nparam = nparam + 1
	    } 

	    # Write the constant parameters.
	    iferr {
	        if (nparam > 1)
		    call ph_wparam (cfd, "const", 5, Memc[dexpr], len_dexpr - 3)
	    } then
		break

	    neq = neq + 1
	}

	call printf ("\n")

	# Close the string files.
	call strclose (tfd)
	call strclose (ffd)
	call strclose (cfd)

	# Write output results to a file
	call ph_wfile (fd, Memc[fit], CFG_MAXLENPARAM, Memc[const],
	    CFG_MAXLENPARAM, Memc[trans], CFG_MAXLENTRANS) 

	# Free memory.
	call mfree (trans, TY_CHAR)
	call mfree (fit, TY_CHAR)
	call mfree (const, TY_CHAR)
	call sfree (sp)
end


# PH_RCOLNUMBER -- Read in the column name and number as an identifier and
# a number respectively.

int procedure ph_rcolnumber (helpfile, column, max_lcolname, number)

char	helpfile[ARB]		# name of the helpfile
char	column[ARB]		# name of the column
int	max_lcolname		# maximum length of the column name
int	number			# column number

bool	streq(), ph_isident()
int	scan(), strmatch(), nscan()

begin
	# Issue the prompt.
	call printf (
	"Enter column definition (name number, ?=help, <EOF>=quit entry): ")
	call flush (STDOUT)

	if (scan () == EOF)
	    return (EOF)
	call gargwrd (column, max_lcolname)
	call gargi (number)

	# Check for errors.
	if (streq (column, "?")) {
	    call pagefile (helpfile, "")
	    return (0)
	} else if (! ph_isident (column) && strmatch (column, "error(") == 0) {
	    call printf (
	        "<Error> %s is not a legal column name\n")
		call pargstr (column)
	    return (0)
	} else if (nscan() != 2) {
	    call printf ("<Error> Cannot decode the column number\n")
	    return (0)
	} else if (number <= 1) {
	    call printf (
	        "<Error> Column 1 is reserved for the object name\n")
	    return (0)
	}

	return (nscan())
end


# PH_VCOLNUMBER -- Verify the column name and number.

procedure ph_vcolnumber (column, newcolumn, max_lcolname, number, newnumber)

char	column[ARB]		# input column name
char	newcolumn[ARB]		# new column name
int	max_lcolname		# maximum length of the column name
int	number			# original column number
int	newnumber		# new column number

bool	ph_isident()
int	scan(), nscan()

begin
	# Issue the verify prompt.
	call printf ("\tVerify (%s  %d) (name number, <CR>=ok): ")
	    call pargstr (column)
	    call pargi (number)
	call flush (STDOUT)

	# Get the new values.
	if (scan () == EOF) {
	    newcolumn[1] = EOS
	    call printf ("\n")
	    call flush (STDOUT)
	} else
	    call gargstr (newcolumn, max_lcolname)

	# If the new input is not valid use the original values.
	if (newcolumn[1] != EOS) {
	    call sscan (newcolumn)
		call gargwrd (newcolumn, max_lcolname)
		call gargi (newnumber)
	    if (! ph_isident(newcolumn))
		call strcpy (column, newcolumn, max_lcolname)
	    if (nscan() != 2)
		newnumber = number
	    else if (newnumber <= 1)
		newnumber = number
	} else {
	    call strcpy (column, newcolumn, max_lcolname)
	    newnumber = number
	}
end


# PH_RLABEL -- Read a legal identifier or label.

int procedure ph_rlabel (keyword, label, max_lenlabel, verify)

char	keyword[ARB]		# the identfier or label keyword
char	label[ARB]		# the output label
int	max_lenlabel		# maximum length of the label
int	verify			# verify the expression

int	len_label
pointer	sp, newlabel
bool    streq(), ph_isident()
int	scan(), strlen()

begin
	# Prompt for the label of the specified equation.
	call printf ("Enter %s (label, ?=help, <EOF>=quit entry): ")
	    call pargstr (keyword)
	call flush (STDOUT)

	# Read the label value.
	if (scan () == EOF) {
	    call printf ("\n")
	    return (EOF)
	} else
	    call gargwrd (label, max_lenlabel)

	# Check for errors.
	if (streq (label, "?")) {
	    call pagefile (CFG_TRANSHELP, "")
	    return (0)
	} else if (! ph_isident (label)) {
	    call printf ("<Error> %s is not a legal label\n")
	        call pargstr (label)
	    return (0)
	} else {
	    len_label = strlen (label)
	}

	if (verify == NO)
	    return (len_label)

	# Verify the label.
	call smark (sp)
	call salloc (newlabel, max_lenlabel, TY_CHAR)

	# Issue the verify prompt.
	call printf ("\tVerify (%s) (label, <CR>=ok): ")
	    call pargstr (label)
	call flush (STDOUT)

	# Read the new value. Restore the old values if the input is invalid.
	if (scan () == EOF) {
	    Memc[newlabel] = EOS
	    call printf ("\n")
	    call flush (STDOUT)
	} else
	    call gargwrd (Memc[newlabel], max_lenlabel)

	# Check the value.
	if (Memc[newlabel] != EOS) {
	    if (! ph_isident(Memc[newlabel]))
		call strcpy (label, Memc[newlabel], max_lenlabel)
	} else
	    call strcpy (label, Memc[newlabel], max_lenlabel)
	len_label = strlen (Memc[newlabel])

	call sfree (sp)

	return (len_label)
end


# PH_REXPR -- Read in an expression. The expression can span multiple lines.

int procedure ph_rexpr (keyword, expr, max_lenexpr, neq, verify)

char	keyword[ARB]		# name of the expression
char	expr[ARB]		# expression
int	max_lenexpr		# the maximum length of an expression
int	neq			# the equation number
int	verify			# verify the expression

int	ip, op, len_expr, sz_expr
pointer	sp, newexpr
bool	streq()
int	strlen(), scan(), gstrcpy()

begin
	# Issue the prompt.
	call printf ("Enter %s (equation, equation\=continue, ?=help, ")
	    call pargstr (keyword)
	call printf ("<EOF>=quit entry):\n")

	# Read in the expression.
	ip = 1
	repeat {

	    # Get a chunk of the equation.
	    if (scan () == EOF) {
		call printf ("\n")
	        return (EOF)
	    } else
	        call gargstr (expr[ip], max_lenexpr)

	    # Decode the expresssion.
	    if (expr[ip] == EOS) {
		break
	    } else if (streq (expr[ip], "?")) {
	        call pagefile (CFG_TRANSHELP, "")
		call printf (
		    "Equation %d enter %s (?=help, \=continue, EOF=quit):\n")
	    	    call pargi (neq)
	    	    call pargstr (keyword)
	        next
	    } else {
	        ip = ip + strlen (expr[ip])
		if (expr[ip-1] == '\\')
		    ip = ip - 1
		else
		    break
	    }
	}

	# Return length of expression if verify is off.
	len_expr = ip - 1
	if (verify == NO)
	    return (len_expr)

	# Allocate working space.
	call smark (sp)
	call salloc (newexpr, max_lenexpr, TY_CHAR)

	# Issue the verify prompt.
	call printf ("Verify (equation, <CR>=ok):\n")
	    call pargstr (keyword)

	# Read the new values.
	ip = 1
	op = 1
	while (ip <= len_expr) {

	    # Print a chunk of the existing equation.
	    sz_expr = min (len_expr - ip + 1, CFG_MAXLENLINE - 2)
	    call printf ("%*.*s\n")
		call pargi (-sz_expr)
		call pargi (sz_expr)
		call pargstr (expr[ip])
	    call flush (STDOUT)

	    # Get a chunk of the new equation.
	    if (scan() == EOF) {
	        Memc[newexpr+op-1] = EOS
		call printf ("\n")
	        call flush (STDOUT)
	    } else
	        call gargstr (Memc[newexpr+op-1], CFG_MAXLENLINE)

	    if (Memc[newexpr+op-1] == EOS) {
	        op = op + gstrcpy (expr[ip], Memc[newexpr+op-1], sz_expr)
	    #} else if (Memc[newexpr+op-1] == '\\') {
	        #if (scan() == EOF)
	            #Memc[newexpr+op-1] = EOS
	        #else
	            #call gargstr (Memc[newexpr+op-1], CFG_MAXLENLINE)
		#op = op + strlen (Memc[newexpr+op-1])
	    } else
		op = op + strlen (Memc[newexpr+op-1])

	    ip = ip + sz_expr
	}

	# Copy the new expression into the output buffer.
	if (Memc[newexpr] != EOS)
	    call strcpy (Memc[newexpr], expr, max_lenexpr)

	call sfree (sp)

	return (op - 1)
end


# PH_RLIST -- Get a list of parameter = value strings.

int procedure ph_rlist (list, op, max_lenlist, keyword, verify)

char	list[ARB]		# list of parameter=value strings
int	op			# pointer into the parameter list
int	max_lenlist		# maximum length of parameter list
char	keyword[ARB]		# prompt keyword
int	verify			# verify the parameter values

int	stat
pointer	sp, param, number
int	ph_rpar(), gstrcpy()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (param, SZ_LINE, TY_CHAR)
	call salloc (number, SZ_LINE, TY_CHAR)

	# Read the parameter and its associated value.
	stat = ph_rpar (CFG_TRANSHELP, keyword,  Memc[param], Memc[number],
	    SZ_LINE, verify)

	# Copy the values into the fitted parameter string.
	if (stat >= 2) {
	    op = op + gstrcpy (Memc[param], list[op], max_lenlist - op + 1)
	    op = op + gstrcpy (" = ", list[op], max_lenlist - op + 1)
	    op = op + gstrcpy (Memc[number], list[op], max_lenlist - op + 1)
	    op = op + gstrcpy (", ", list[op], max_lenlist - op + 1)
	}

	call sfree (sp)

	return (stat)
end


# PH_RPAR -- Read in a user supplied parameter and value.

int procedure ph_rpar (helpfile, keyword, param, number, max_lenparam,
	verify)

char	helpfile[ARB]		# name of the helpfile
char	keyword[ARB]		# name of the keyword
char	param[ARB]		# name of the parameter
char	number[ARB]		# parameter value
int	max_lenparam		# maximum length of parameter name and value
int	verify			# verify the values

int	ip, nchars
pointer	sp, newparam, newnumber
bool	streq(), ph_isident()
int	scan(), nscan(), lexnum()

begin
	# Issue the prompt.
	call printf (
	    "Enter %s (name value, ?=help, <EOF>=quit entry):")
	    call pargstr (keyword)
	call flush (STDOUT)

	# Read the values.
	if (scan () == EOF) {
	    call printf ("\n")
	    return (EOF)
	} else {
	    call gargwrd (param, max_lenparam)
	    call gargwrd (number, max_lenparam)
	}

	# Check for errors.
	ip = 1
	if (streq (param, "?")) {
	    call pagefile (helpfile, "")
	    return (0)
	} else if (! ph_isident (param)) {
	    call printf ("<Error> %s is not a legal parameter name\n")
		call pargstr (param)
	    return (0)
	} else if (nscan() != 2) {
	    call printf ("<Error> Cannot decode the parameter value\n")
	    return (0)
	} else if (lexnum (number, ip, nchars) == LEX_NONNUM) {
	    call printf (
	        "<Error> The parameter value %s is not a legal number\n")
		call pargstr (number)
	    return (0)
	} else if (verify == NO) {
	    return (nscan())
	}

	call smark (sp)
	call salloc (newparam, max_lenparam, TY_CHAR)
	call salloc (newnumber, max_lenparam, TY_CHAR)

	# Issue the verify prompt.
	call printf ("\tVerify (%s %s) (name value, <CR>=ok):")
	    call pargstr (param)
	    call pargstr (number)
	    call flush (STDOUT)

	# Read the new values.
	ip = 1
	if (scan () == EOF)
	    Memc[newparam] = EOS
	else
	    call gargstr (Memc[newparam], max_lenparam)

	if (Memc[newparam] != EOS) {
	    call sscan (Memc[newparam])
		call gargwrd (Memc[newparam], max_lenparam)
		call gargwrd (Memc[newnumber], max_lenparam)
	    if (ph_isident (Memc[newparam]))
		call strcpy (Memc[newparam], param, max_lenparam)
	    if ((nscan() == 2) && (lexnum (Memc[newnumber], ip, nchars) !=
	        LEX_NONNUM))
		call strcpy (number, Memc[newnumber], max_lenparam)
	}

	call sfree (sp)

	return (2)
end


# PH_WEQUATION -- Format and write a transformation equation to the
# configuration file.

procedure ph_wequation (fd, label, len_label, expr, len_expr)

int	fd		# the output configuration file descriptor
char	label[ARB]	# the equation label
int	len_label	# length of the label
char	expr[ARB]	# the left-hand side or function expression
int	len_expr	# length of the function expression

int	op
pointer	sp, line
errchk	ph_write, putline

begin
	# Allocate working space.
	call smark (sp)
	call salloc (line, CFG_MAXLENLINE + 1, TY_CHAR)
	op = 1

	# Initialize the output buffer and copy the label, the colon 
	# character label delimiter, te function expression, the equal
	# sign and the fit expression to the configurationf file.

	call ph_write (fd, label, len_label, Memc[line], CFG_MAXLENLINE, op)
	call ph_write (fd, " : ", 3, Memc[line], CFG_MAXLENLINE, op)
	call ph_write (fd, expr, len_expr, Memc[line], CFG_MAXLENLINE, op)

	# Flush the remainder of the output.
	if (op > 1)
	    call putline (fd, Memc[line])

	call sfree (sp)
end


# PH_WDERIV -- Write the derivative expression for the equation to the
# output file.

procedure ph_wderiv (fd, label, len_label, param, len_param, dexpr, len_dexpr)

int	fd		# pointer to the output file descriptor
char	label[ARB]	# the equation label
int	len_label	# length of the label
char	param[ARB]	# the derivative parameter name
int	len_param	# length of the derivative parameter name
char	dexpr[ARB]	# the derivative expression
int	len_dexpr	# length of the derivative expression

int	op
pointer	sp, line
errchk	ph_write, putline

begin
	call smark (sp)
	call salloc (line, CFG_MAXLENLINE + 1, TY_CHAR)
	op = 1

	# Copy the keyword and opening bracket, the equation label, the
	# separating comma, the parameter name, the closing bracket and
	# equal sign, and the derivative expression to the output file .

	call ph_write (fd, "deriv (", 7, Memc[line], CFG_MAXLENLINE, op)
	call ph_write (fd, label, len_label, Memc[line], CFG_MAXLENLINE, op)
	call ph_write (fd, ",", 1, Memc[line], CFG_MAXLENLINE, op)
	call ph_write (fd, param, len_param, Memc[line], CFG_MAXLENLINE, op)
	call ph_write (fd, ") = ", 4, Memc[line], CFG_MAXLENLINE, op)
	call ph_write (fd, dexpr, len_dexpr, Memc[line], CFG_MAXLENLINE, op)

	# Flush the remainder of the output.
	if (op > 1)
	    call putline (fd, Memc[line])

	call sfree (sp)
end


# PH_WPARAM -- Write the fitted and constant parameters to the configuration
# file.

procedure ph_wparam (fd, keyword, len_keyword, paramlist, len_list)

int	fd		# output file descriptor
char	keyword[ARB]	# statement keyword
int	len_keyword	# length of the keyword
char	paramlist[ARB]	# fitted parameter list
int	len_list	# length of fitted parameter list

int	op
pointer	sp, line
errchk	ph_write, putline

begin
	call smark (sp)
	call salloc (line, CFG_MAXLENLINE + 1, TY_CHAR)

	# Write the keyword, two blanks, and the parameter list.
	op = 1
	call ph_write (fd, keyword, len_keyword, Memc[line], CFG_MAXLENLINE, op)
	call ph_write (fd, "  ", 2, Memc[line], CFG_MAXLENLINE, op)
	call ph_write (fd, paramlist, len_list, Memc[line], CFG_MAXLENLINE, op)

	# Flush the remainder of the output.
	if (op > 1)
	    call putline (fd, Memc[line])

	call sfree (sp)
end


# PH_WFILE -- Write the stored strings files to the output file.

procedure ph_wfile (fd, fit, max_lfit, const, max_lconst, trans, max_ltrans)

int	fd		# the output file descriptor
char	fit[ARB]	# the fitted parameter array
int	max_lfit	# the maximum length of the fit array
char	const[ARB]	# the constant parameter array
int	max_lconst	# the maximum length of the constant array
char	trans[ARB]	# the equation section
int	max_ltrans	# the maximum length of the equation section

int	tfd, ffd, cfd
pointer	sp, line
int	stropen(), getline()

begin
	call smark (sp)
	call salloc (line, SZ_LINE, TY_CHAR)

	ffd = stropen (fit, max_lfit, READ_ONLY)
	cfd = stropen (const, max_lconst, READ_ONLY)
	tfd = stropen (trans, max_ltrans, READ_ONLY)

	while (getline (ffd, Memc[line]) != EOF)
	    call putline (fd, Memc[line])
	call putline (fd, "\n")
	while (getline (cfd, Memc[line]) != EOF)
	    call putline (fd, Memc[line])
	call putline (fd, "\n")
	while (getline (tfd, Memc[line]) != EOF)
	    call putline (fd, Memc[line])

	call strclose (tfd)
	call strclose (ffd)
	call strclose (cfd)

	call sfree (sp)
end


# PH_WRITE -- Write an output record from the input stream.

procedure ph_write (fd, input, max_linput, output, max_loutput, op)

int	fd		# the output file descriptor
char	input[ARB]	# the input array
int	max_linput	# maximum length of the input array
char	output[ARB]	# the output array
int	max_loutput	# maximum length of the output array
int	op		# the output array pointer

int	ip, nchars
errchk	putline

begin
	ip = 1

	repeat {
	    nchars = min (max_linput - ip + 1, max_loutput - op + 1)
	    call amovc (input[ip], output[op], nchars)
	    ip = ip + nchars
	    op = op + nchars
	    if (op > max_loutput) {
		output[op] = '\n'
		output[op+1] = EOS
		call putline (fd, output)
		op = 1
	    }
	} until (ip > max_linput)

	output[op] = '\n'
	output[op+1] = EOS
end


# PH_ISIDENT -- Check to see if a string is a legal identifier.

bool procedure ph_isident (ident)

char	ident[ARB]		# string to be tested

bool	isident
int	ip

begin
	if (ident[1] == EOS)
	    return (false)
	if (! IS_ALPHA(ident[1]))
	    return (false)

	isident = true
	for (ip = 2; isident && (ident[ip] != EOS); ip = ip + 1) {
	    if ((! IS_ALPHA(ident[ip])) && (! IS_DIGIT(ident[ip])))
		isident = false
	}

	return (isident)
end
