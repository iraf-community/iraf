# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<evexpr.h>
include	<imset.h>
include	<ctype.h>
include	<lexnum.h>

define	LEN_USERAREA	28800		# allow for the largest possible header
define	SZ_IMAGENAME	63		# max size of an image name
define	SZ_FIELDNAME	31		# max size of a field name

define	OP_EDIT		1		# hedit opcodes
define	OP_INIT		2		
define	OP_ADD		3
define	OP_DELETE	4


# HEDIT -- Edit or view selected fields of an image header or headers.  This
# editor performs a single edit operation upon a relation, e.g., upon a set
# of fields of a set of images.  Templates and expressions may be used to 
# automatically select the images and fields to be edited, and to compute
# the new value of each field.

procedure t_hedit()

pointer	fields			# template listing fields to be processed
pointer	valexpr			# the value expression (if op=edit|add)

bool	noupdate, quit
int	imlist, flist, nfields, up, min_lenuserarea
pointer	sp, field, sections, s_fields, s_valexpr, im, ip, image, buf
int	operation, verify, show, update

pointer	immap()
bool	clgetb(), streq()
int	btoi(), imtopenp(), imtgetim(), imofnlu(), imgnfn(), getline()
int	envfind(), ctoi()

begin
	call smark (sp)
	call salloc (buf,       SZ_FNAME, TY_CHAR)
	call salloc (image,     SZ_FNAME, TY_CHAR)
	call salloc (field,     SZ_FNAME, TY_CHAR)
	call salloc (s_fields,  SZ_LINE,  TY_CHAR)
	call salloc (s_valexpr, SZ_LINE,  TY_CHAR)
	call salloc (sections, SZ_FNAME, TY_CHAR)

	# Get the primary operands.
	imlist = imtopenp ("images")

	# Determine type of operation to be performed.  The default operation
	# is edit.

	operation = OP_EDIT
	if (clgetb ("add"))
	    operation = OP_ADD
	else if (clgetb ("addonly"))
	    operation = OP_INIT
	else if (clgetb ("delete"))
	    operation = OP_DELETE

	# Get list of fields to be edited, added, or deleted.
	call clgstr ("fields", Memc[s_fields], SZ_LINE)
	for (ip=s_fields;  IS_WHITE (Memc[ip]);  ip=ip+1)
	    ;
	fields = ip

	# The value expression parameter is not used for the delete operation.
	if (operation != OP_DELETE) {
	    call clgstr ("value", Memc[s_valexpr], SZ_LINE)
	    for (ip=s_valexpr;  IS_WHITE (Memc[ip]);  ip=ip+1)
		;
	    valexpr = ip
	    while (Memc[ip] != EOS)
		ip = ip + 1
	    while (ip > valexpr && IS_WHITE (Memc[ip-1]))
		ip = ip - 1
	    Memc[ip] = EOS
	} else {
	    Memc[s_valexpr] = EOS
	    valexpr = s_valexpr
	}

	# Get switches.  If the expression value is ".", meaning print value
	# rather than edit, then we do not use the switches.

	if (operation == OP_EDIT && streq (Memc[valexpr], ".")) {
	    update = NO
	    verify = NO
	    show   = NO
	} else {
	    update = btoi (clgetb ("update"))
	    verify = btoi (clgetb ("verify"))
	    show   = btoi (clgetb ("show"))
	}

	# Main processing loop.  An image is processed in each pass through
	# the loop.

	while (imtgetim (imlist, Memc[image], SZ_FNAME) != EOF) {

	    # set the length of the user area
	    if (envfind ("min_lenuserarea", Memc[sections], SZ_FNAME) > 0) {
		up = 1
		if (ctoi (Memc[sections], up, min_lenuserarea) <= 0)
		    min_lenuserarea = LEN_USERAREA
		else
		    min_lenuserarea = max (LEN_USERAREA, min_lenuserarea)
	    } else
		min_lenuserarea = LEN_USERAREA

	    # Open the image.
	    iferr {
		if (update == YES)
		    im = immap (Memc[image], READ_WRITE, min_lenuserarea)
		else
		    im = immap (Memc[image], READ_ONLY,  min_lenuserarea)
	    } then {
		call erract (EA_WARN)
		next
	    }

	    if (operation == OP_INIT || operation == OP_ADD) {
		# Add a field to the image header.  This cannot be done within
		# the IMGNFN loop because template expansion on the existing
		# fields of the image header would discard the new field name
		# since it does not yet exist.

		nfields = 1
		call he_getopsetimage (im, Memc[image], Memc[field])
		switch (operation) {
		case OP_INIT:
		    call he_initfield (im, Memc[image], Memc[fields],
		        Memc[valexpr], verify, show, update)
		case OP_ADD:
		    call he_addfield (im, Memc[image], Memc[fields],
		        Memc[valexpr], verify, show, update)
		}
	    
	    } else {
		# Open list of fields to be processed.
		flist = imofnlu (im, Memc[fields])

		nfields = 0
		while (imgnfn (flist, Memc[field], SZ_FNAME) != EOF) {
		    call he_getopsetimage (im, Memc[image], Memc[field])

		    switch (operation) {
		    case OP_EDIT:
			call he_editfield (im, Memc[image], Memc[field],
			    Memc[valexpr], verify, show, update)
		    case OP_DELETE:
			call he_deletefield (im, Memc[image], Memc[field],
			    Memc[valexpr], verify, show, update)
		    }
		    nfields = nfields + 1
		}

		call imcfnl (flist)
	    }

	    # Update the image header and unmap the image.

	    noupdate = false
	    quit = false

	    if (update == YES) {
		if (nfields == 0)
		    noupdate = true
		else if (verify == YES) {
		    call eprintf ("update %s ? (yes): ")
			call pargstr (Memc[image])
		    call flush (STDERR)

		    if (getline (STDIN, Memc[buf]) == EOF)
			noupdate = true
		    else {
			# Strip leading whitespace and trailing newline.
			for (ip=buf;  IS_WHITE(Memc[ip]);  ip=ip+1)
			    ;
			if (Memc[ip] == 'q') {
			    quit = true
			    noupdate = true
			} else if (! (Memc[ip] == '\n' || Memc[ip] == 'y'))
			    noupdate = true
		    }
		}

		if (noupdate) {
		    call imseti (im, IM_WHEADER, NO)
		    call imunmap (im)
		} else {
		    call imunmap (im)
		    if (show == YES) {
			call printf ("%s updated\n")
			    call pargstr (Memc[image])
		    }
		}
	    } else
		call imunmap (im)

	    call flush (STDOUT)
	    if (quit)
		break
	}

	call imtclose (imlist)
	call sfree (sp)
end


# HE_EDITFIELD -- Edit the value of the named field of the indicated image.
# The value expression is evaluated, interactively inspected if desired,
# and the resulting value put to the image.

procedure he_editfield (im, image, field, valexpr, verify, show, update)

pointer	im			# image descriptor of image to be edited
char	image[ARB]		# name of image to be edited
char	field[ARB]		# name of field to be edited
char	valexpr[ARB]		# value expression
int	verify			# verify new value interactively
int	show			# print record of edit
int	update			# enable updating of the image

int	goahead, nl
pointer	sp, ip, oldval, newval, defval, o

bool	streq()
pointer	evexpr()
extern	he_getop()
int	getline(), imaccf(), strldxs(), locpr()
errchk	evexpr, getline, imaccf, he_gval

begin
	call smark (sp)
	call salloc (oldval, SZ_LINE, TY_CHAR)
	call salloc (newval, SZ_LINE, TY_CHAR)
	call salloc (defval, SZ_LINE, TY_CHAR)

	# Verify that the named field exists before going any further.
	if (field[1] != '$')
	    if (imaccf (im, field) == NO) {
		call eprintf ("parameter %s,%s not found\n")
		    call pargstr (image)
		    call pargstr (field)
		call sfree (sp)
		return
	    }

	# Get the old value.
	call he_gval (im, image, field, Memc[oldval], SZ_LINE)

	# Evaluate the expression.  Encode the result operand as a string.
	# If the expression is not parenthesized, assume that is is already
	# a string literal.

	if (valexpr[1] == '(') {
	    o = evexpr (valexpr, locpr (he_getop), 0)
	    call he_encodeop (o, Memc[newval], SZ_LINE)
	    call xev_freeop (o)
	    call mfree (o, TY_STRUCT)
	} else
	    call strcpy (valexpr, Memc[newval], SZ_LINE)

	if (streq (Memc[newval], ".")) {
	    # Merely print the value of the field.

	    call printf ("%s,%s = %s\n")
		call pargstr (image)
		call pargstr (field)
		call he_pargstr (Memc[oldval])

	} else if (verify == YES) {
	    # Query for new value and edit the field.  If the response is a
	    # blank line, use the default new value.  If the response is "$"
	    # or EOF, do not change the value of the parameter.

	    call strcpy (Memc[newval], Memc[defval], SZ_LINE)
	    call eprintf ("%s,%s (%s -> %s): ")
		call pargstr (image)
		call pargstr (field)
		call he_pargstr (Memc[oldval])
		call he_pargstr (Memc[defval])
	    call flush (STDERR)

	    if (getline (STDIN, Memc[newval]) != EOF) {
		# Do not skip leading whitespace; may be significant in a
		# string literal.

		ip = newval

		# Do strip trailing newline since it is an artifact of getline.
		nl = strldxs ("\n", Memc[ip]) 
		if (nl > 0)
		    Memc[ip+nl-1] = EOS

		# Decode user response.
		if (Memc[ip] == '\\') {
		    ip = ip + 1
		    goahead = YES
		} else if (streq(Memc[ip],"n") || streq(Memc[ip],"no")) {
		    goahead = NO
		} else if (streq(Memc[ip],"y") || streq(Memc[ip],"yes") ||
		    Memc[ip] == EOS) {
		    call strcpy (Memc[defval], Memc[newval], SZ_LINE)
		    goahead = YES
		} else {
		    if (ip > newval)
			call strcpy (Memc[ip], Memc[newval], SZ_LINE)
		    goahead = YES
		}

		# Edit field if so indicated.
		if (goahead == YES)
		    call he_updatefield (im, image, field, Memc[oldval],
			Memc[newval], show)

		call flush (STDOUT)
	    }

	} else {
	    call he_updatefield (im, image, field, Memc[oldval], Memc[newval],
		show)
	}

	call sfree (sp)
end


# HE_INITFIELD -- Add a new field to the indicated image.  If the field already
# exists do not set its value.  The value expression is evaluated and the
# resulting value used as the initial value in adding the field to the image.

procedure he_initfield (im, image, field, valexpr, verify, show, update)

pointer	im			# image descriptor of image to be edited
char	image[ARB]		# name of image to be edited
char	field[ARB]		# name of field to be edited
char	valexpr[ARB]		# value expression
int	verify			# verify new value interactively
int	show			# print record of edit
int	update			# enable updating of the image

bool	numeric
int	numlen, ip
pointer	sp, newval, o
pointer	evexpr()
int	imaccf(), locpr(), strlen(), lexnum()
extern	he_getop()
errchk	imaccf, evexpr, imaddb, imastr, imaddi, imaddr

begin
	call smark (sp)
	call salloc (newval, SZ_LINE, TY_CHAR)

	# If the named field already exists, this is really an edit operation
	# rather than an add.  Call editfield so that the usual verification
	# can take place.

	if (imaccf (im, field) == YES) {
	    call eprintf ("parameter %s,%s already exists\n")
	        call pargstr (image)
	        call pargstr (field)
	    call sfree (sp)
	    return
	}

	# If the expression is not parenthesized, assume that is is already
	# a string literal.  If the expression is a string check for a simple
	# numeric field.

	ip = 1
	numeric = (lexnum (valexpr, ip, numlen) != LEX_NONNUM)
	if (numeric)
	    numeric = (numlen == strlen (valexpr))

	if (numeric || valexpr[1] == '(')
	    o = evexpr (valexpr, locpr(he_getop), 0)
	else {
	    call malloc (o, LEN_OPERAND, TY_STRUCT)
	    call xev_initop (o, max(1,strlen(valexpr)), TY_CHAR)
	    call strcpy (valexpr, O_VALC(o), ARB)
	}

	# Add the field to the image (or update the value).  The datatype of
	# the expression value operand determines the datatype of the new
	# parameter.

	switch (O_TYPE(o)) {
	case TY_BOOL:
	    call imaddb (im, field, O_VALB(o))
	case TY_CHAR:
	    call imastr (im, field, O_VALC(o))
	case TY_INT:
	    call imaddi (im, field, O_VALI(o))
	case TY_REAL:
	    call imaddr (im, field, O_VALR(o))
	default:
	    call error (1, "unknown expression datatype")
	}

	if (show == YES) {
	    call he_encodeop (o, Memc[newval], SZ_LINE)
	    call printf ("add %s,%s = %s\n")
		call pargstr (image)
		call pargstr (field)
		call he_pargstr (Memc[newval])
	}

	call xev_freeop (o)
	call mfree (o, TY_STRUCT)
	call sfree (sp)
end


# HE_ADDFIELD -- Add a new field to the indicated image.  If the field already
# exists, merely set its value.  The value expression is evaluated and the
# resulting value used as the initial value in adding the field to the image.

procedure he_addfield (im, image, field, valexpr, verify, show, update)

pointer	im			# image descriptor of image to be edited
char	image[ARB]		# name of image to be edited
char	field[ARB]		# name of field to be edited
char	valexpr[ARB]		# value expression
int	verify			# verify new value interactively
int	show			# print record of edit
int	update			# enable updating of the image

bool	numeric
int	numlen, ip
pointer	sp, newval, o
pointer	evexpr()
int	imaccf(), locpr(), strlen(), lexnum()
extern	he_getop()
errchk	imaccf, evexpr, imaddb, imastr, imaddi, imaddr

begin
	call smark (sp)
	call salloc (newval, SZ_LINE, TY_CHAR)

	# If the named field already exists, this is really an edit operation
	# rather than an add.  Call editfield so that the usual verification
	# can take place.

	if (imaccf (im, field) == YES) {
	    call he_editfield (im, image, field, valexpr, verify, show, update)
	    call sfree (sp)
	    return
	}

	# If the expression is not parenthesized, assume that is is already
	# a string literal.  If the expression is a string check for a simple
	# numeric field.

	ip = 1
	numeric = (lexnum (valexpr, ip, numlen) != LEX_NONNUM)
	if (numeric)
	    numeric = (numlen == strlen (valexpr))

	if (numeric || valexpr[1] == '(')
	    o = evexpr (valexpr, locpr(he_getop), 0)
	else {
	    call malloc (o, LEN_OPERAND, TY_STRUCT)
	    call xev_initop (o, max(1,strlen(valexpr)), TY_CHAR)
	    call strcpy (valexpr, O_VALC(o), ARB)
	}

	# Add the field to the image (or update the value).  The datatype of
	# the expression value operand determines the datatype of the new
	# parameter.

	switch (O_TYPE(o)) {
	case TY_BOOL:
	    call imaddb (im, field, O_VALB(o))
	case TY_CHAR:
	    call imastr (im, field, O_VALC(o))
	case TY_INT:
	    call imaddi (im, field, O_VALI(o))
	case TY_REAL:
	    call imaddr (im, field, O_VALR(o))
	default:
	    call error (1, "unknown expression datatype")
	}

	if (show == YES) {
	    call he_encodeop (o, Memc[newval], SZ_LINE)
	    call printf ("add %s,%s = %s\n")
		call pargstr (image)
		call pargstr (field)
		call he_pargstr (Memc[newval])
	}

	call xev_freeop (o)
	call mfree (o, TY_STRUCT)
	call sfree (sp)
end


# HE_DELETEFIELD -- Delete a field from the indicated image.  If the field does
# not exist, print a warning message.

procedure he_deletefield (im, image, field, valexpr, verify, show, update)

pointer	im			# image descriptor of image to be edited
char	image[ARB]		# name of image to be edited
char	field[ARB]		# name of field to be edited
char	valexpr[ARB]		# not used
int	verify			# verify deletion interactively
int	show			# print record of edit
int	update			# enable updating of the image

pointer	sp, ip, newval
int	getline(), imaccf()

begin
	call smark (sp)
	call salloc (newval, SZ_LINE, TY_CHAR)

	if (imaccf (im, field) == NO) {
	    call eprintf ("nonexistent field %s,%s\n")
		call pargstr (image)
		call pargstr (field)
	    call sfree (sp)
	    return
	}
	    
	if (verify == YES) {
	    # Delete pending verification.

	    call eprintf ("delete %s,%s ? (yes): ")
		call pargstr (image)
		call pargstr (field)
	    call flush (STDERR)

	    if (getline (STDIN, Memc[newval]) != EOF) {
		# Strip leading whitespace and trailing newline.
		for (ip=newval;  IS_WHITE(Memc[ip]);  ip=ip+1)
		    ;
		if (Memc[ip] == '\n' || Memc[ip] == 'y') {
		    call imdelf (im, field)
		    if (show == YES) {
			call printf ("%s,%s deleted\n")
			    call pargstr (image)
			    call pargstr (field)
		    }
		}
	    }
	
	} else {
	    # Delete without verification.

	    iferr (call imdelf (im, field))
		call erract (EA_WARN)
	    else if (show == YES) {
		call printf ("%s,%s deleted\n")
		    call pargstr (image)
		    call pargstr (field)
	    }
	}

	call sfree (sp)
end


# HE_UPDATEFIELD -- Update the value of an image header field.

procedure he_updatefield (im, image, field, oldval, newval, show)

pointer	im			# image descriptor
char	image[ARB]		# image name
char	field[ARB]		# field name
char	oldval[ARB]		# old value, encoded as a string
char	newval[ARB]		# old value, encoded as a string
int	show			# print record of update

begin
	iferr (call impstr (im, field, newval)) {
	    call eprintf ("cannot update %s,%s\n")
		call pargstr (image)
		call pargstr (field)
	    return
	}
   
	if (show == YES) {
	    call printf ("%s,%s: %s -> %s\n")
		call pargstr (image)
		call pargstr (field)
		call he_pargstr (oldval)
		call he_pargstr (newval)
	}
end


# HE_GVAL -- Get the value of an image header field and return it as a string.
# The ficticious special field "$I" (the image name) is recognized in this
# context in addition to the actual header fields.

procedure he_gval (im, image, field, strval, maxch)

pointer	im			# image descriptor
char	image[ARB]		# image name
char	field[ARB]		# field whose value is to be returned
char	strval[ARB]		# string value of field (output)
int	maxch			# max chars out

begin
	if (field[1] == '$' && field[2] == 'I')
	    call strcpy (image, strval, maxch)
	else if (field[1] == '$')
	    call imgstr (im, field[2], strval, maxch)
	else
	    call imgstr (im, field, strval, maxch)
end


# HE_GETOP -- Satisfy an operand request from EVEXPR.  In this context,
# operand names refer to the fields of the image header.  The following
# special operand names are recognized:
#
#	.		a string literal, returned as the string "."
#	$		the value of the current field
#	$F		the name of the current field
#	$I		the name of the current image
#	$T		the current time, expressed as an integer
#
# The companion procedure HE_GETOPSETIMAGE is used to pass the image pointer
# and image and field names.

procedure he_getop (operand, o)

char	operand[ARB]		# operand name
pointer	o			# operand (output)

pointer	h_im			# getop common
char	h_image[SZ_IMAGENAME]
char	h_field[SZ_FIELDNAME]
common	/hegopm/ h_im, h_image, h_field
bool	streq()
long	clktime()
errchk	he_getfield

begin
	if (streq (operand, ".")) {
	    call xev_initop (o, 1, TY_CHAR)
	    call strcpy (".", O_VALC(o), 1)

	} else if (streq (operand, "$")) {
	    call he_getfield (h_im, h_field, o)
	
	} else if (streq (operand, "$F")) {
	    call xev_initop (o, SZ_FIELDNAME, TY_CHAR)
	    call strcpy (h_field, O_VALC(o), SZ_FIELDNAME)

	} else if (streq (operand, "$I")) {
	    call xev_initop (o, SZ_IMAGENAME, TY_CHAR)
	    call strcpy (h_image, O_VALC(o), SZ_IMAGENAME)

	} else if (streq (operand, "$T")) {
	    # Assignment of long into int may fail on some systems.  Maybe
	    # should use type string and let database convert to long...

	    call xev_initop (o, 0, TY_INT)
	    O_VALI(o) = clktime (long(0))

	} else
	    call he_getfield (h_im, operand, o)
end


# HE_GETFIELD -- Return the value of the named field of the image header as
# an EVEXPR type operand structure.

procedure he_getfield (im, field, o)

pointer	im			# image descriptor
char	field[ARB]		# name of field to be returned
pointer	o			# pointer to output operand

bool	imgetb()
int	imgeti(), imgftype()
real	imgetr()

begin
	switch (imgftype (im, field)) {
	case TY_BOOL:
	    call xev_initop (o, 0, TY_BOOL)
	    O_VALB(o) = imgetb (im, field)

	case TY_SHORT, TY_INT, TY_LONG:
	    call xev_initop (o, 0, TY_INT)
	    O_VALI(o) = imgeti (im, field)

	case TY_REAL, TY_DOUBLE, TY_COMPLEX:
	    call xev_initop (o, 0, TY_REAL)
	    O_VALR(o) = imgetr (im, field)

	default:
	    call xev_initop (o, SZ_LINE, TY_CHAR)
	    call imgstr (im, field, O_VALC(o), SZ_LINE)
	}
end


# HE_GETOPSETIMAGE -- Set the image pointer, image name, and field name (context
# of getop) in preparation for a getop call by EVEXPR.

procedure he_getopsetimage (im, image, field)

pointer	im			# image descriptor of image to be edited
char	image[ARB]		# name of image to be edited
char	field[ARB]		# name of field to be edited

pointer	h_im			# getop common
char	h_image[SZ_IMAGENAME]
char	h_field[SZ_FIELDNAME]
common	/hegopm/ h_im, h_image, h_field

begin
	h_im = im
	call strcpy (image, h_image, SZ_IMAGENAME)
	call strcpy (field, h_field, SZ_FIELDNAME)
end


# HE_ENCODEOP -- Encode an operand as returned by EVEXPR as a string.  EVEXPR
# operands are restricted to the datatypes bool, int, real, and string.

procedure he_encodeop (o, outstr, maxch)

pointer	o			# operand to be encoded
char	outstr[ARB]		# output string
int	maxch			# max chars in outstr

begin
	switch (O_TYPE(o)) {
	case TY_BOOL:
	    call sprintf (outstr, maxch, "%b")
		call pargb (O_VALB(o))
	case TY_CHAR:
	    call sprintf (outstr, maxch, "%s")
		call pargstr (O_VALC(o))
	case TY_INT:
	    call sprintf (outstr, maxch, "%d")
		call pargi (O_VALI(o))
	case TY_REAL:
	    call sprintf (outstr, maxch, "%g")
		call pargr (O_VALR(o))
	default:
	    call error (1, "unknown expression datatype")
	}
end


# HE_PARGSTR -- Pass a string to a printf statement, enclosing the string
# in quotes if it contains any whitespace.

procedure he_pargstr (str)

char	str[ARB]		# string to be printed
int	ip
bool	quoteit
pointer	sp, op, buf

begin
	call smark (sp)
	call salloc (buf, SZ_LINE, TY_CHAR)

	op = buf
	Memc[op] = '"'
	op = op + 1

	# Copy string to scratch buffer, enclosed in quotes.  Check for
	# embedded whitespace.

	quoteit = false
	for (ip=1;  str[ip] != EOS;  ip=ip+1) {
	    if (IS_WHITE(str[ip])) {		# detect whitespace
		quoteit = true
		Memc[op] = str[ip]
	    } else if (str[ip] == '\n') {	# prettyprint newlines
		Memc[op] = '\\'
		op = op + 1
		Memc[op] = 'n'
	    } else				# normal characters
		Memc[op] = str[ip]

	    if (ip < SZ_LINE)
		op = op + 1
	}

	# If whitespace was seen pass the quoted string, otherwise pass the
	# original input string.

	if (quoteit) {
	    Memc[op] = '"'
	    op = op + 1
	    Memc[op] = EOS
	    call pargstr (Memc[buf])
	} else
	    call pargstr (str)

	call sfree (sp)
end
