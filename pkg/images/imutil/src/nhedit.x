# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<evexpr.h>
include	<imset.h>
include	<ctype.h>
include	<lexnum.h>

define	LEN_USERAREA	28800		# allow for the largest possible header
define	SZ_IMAGENAME	63		# max size of an image name
define	SZ_FIELDNAME	31		# max size of a field name
define  HRECLEN         80

define	OP_EDIT		1		# hedit opcodes
define	OP_INIT		2		
define	OP_ADD		3
define	OP_DELETE	4
define  OP_DEFPAR       5
define  OP_RENAME       6
define  BEFORE		1
define  AFTER           2


# NHEDIT -- Edit or view selected fields of an image header or headers.  This
# editor performs a single edit operation upon a relation, e.g., upon a set
# of fields of a set of images.  Templates and expressions may be used to 
# automatically select the images and fields to be edited, and to compute
# the new value of each field.

procedure t_nhedit()

pointer	fields			# template listing fields to be processed
pointer	valexpr			# the value expression (if op=edit|add)

bool	noupdate, quit
int	imlist, nfields, up, min_lenuserarea
pointer	sp, field, comment, sections, im, ip, image, buf
pointer cmd, pkey
int	operation, verify, show, update, fd, baf
int     dp_oper, dp_update, dp_verify, dp_show	

pointer	immap()
bool    streq()
int	imtopenp(), imtgetim(), getline(), nowhite()
int	envfind(), ctoi(), open()

begin
	call smark (sp)
	call salloc (buf,       SZ_FNAME, TY_CHAR)
	call salloc (image,     SZ_FNAME, TY_CHAR)
	call salloc (field,     SZ_FNAME, TY_CHAR)
	call salloc (fields,    SZ_FNAME, TY_CHAR)
	call salloc (pkey,      SZ_FNAME, TY_CHAR)
	call salloc (valexpr,   SZ_LINE,  TY_CHAR)
	call salloc (comment,   SZ_LINE,  TY_CHAR)
	call salloc (sections,  SZ_FNAME, TY_CHAR)
	call salloc (cmd,       SZ_LINE,  TY_CHAR)

	# Get the primary operands.
	imlist = imtopenp ("images")

	# Determine type of operation to be performed (default is edit).

	# Do we have a command file instead of a command line?  Allow either
	# a null string or the string "NULL" to indicate we don't.

        call clgstr ("comfile", Memc[fields], SZ_LINE)
	if (nowhite (Memc[fields], Memc[fields], SZ_LINE) == 0 ||
            streq (Memc[fields], "NULL")) {
	        call he_getpars (operation, fields, valexpr, Memc[comment], 
	            Memc[pkey], baf, update, verify, show)
	        fd = 0
        } else {
	    call he_getpars (dp_oper, NULL, valexpr, Memc[comment], 
	        Memc[pkey], baf, dp_update, dp_verify, dp_show)
	    fd = open(Memc[fields], READ_ONLY, TEXT_FILE)
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
		if (update == YES || fd != 0)
		    im = immap (Memc[image], READ_WRITE,  min_lenuserarea)
		else
		    im = immap (Memc[image], READ_ONLY,  min_lenuserarea)
	    } then {
		call erract (EA_WARN)
		next
	    }
            
	    if (fd != 0) {
	        # Open the command file and start processing each line. 
                # rewind file before  proceeding

	        call seek(fd, BOF)
                while (getline(fd, Memc[cmd]) != EOF) {
		    for (ip=cmd;  IS_WHITE(Memc[ip]);  ip=ip+1)
			    ;
		    if (Memc[cmd] == '#' || Memc[ip] == '\n')
		        next

                    call he_getcmdf (Memc[cmd], operation, Memc[fields],
		        Memc[valexpr], Memc[comment], Memc[pkey], baf, 
			update, verify, show)

		    # Set the default parameters for the command file.
                    if (operation < 0) {
			dp_oper = -operation
			if (update != -1)
			    dp_update = update
			if (verify != -1)
			    dp_verify = verify
			if (show != -1)
			    dp_show = show
                        next
		    }

                    # Set the parameters for the current command, the
                    # command parameters take precedence over the defaults.
                    call nh_setpar (operation, dp_oper, dp_update, 
			   dp_verify, dp_show, update, verify, show)

		    iferr (call nh_edit (im, Memc[image], operation,
			Memc[fields], Memc[valexpr], Memc[comment],
			Memc[pkey], baf, update, verify, show, nfields))
			call erract (EA_WARN)

                }

            } else
                iferr (call nh_edit (im, Memc[image], operation, Memc[fields],
	            Memc[valexpr], Memc[comment], Memc[pkey], baf, update,
		    verify, show, nfields))
		    call erract (EA_WARN)

	    # Update the image header and unmap the image.

	    noupdate = false
	    quit = false

	    if (update == YES) {
		if (nfields == 0 && fd == 0)
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
	    } else {
		call imunmap (im)
	    }

	    call flush (STDOUT)
	    if (quit)
		break
	} #end of while

	# Close command file
        if (fd != 0)
            call close(fd)
	call imtclose (imlist)
	call sfree (sp)
end


# NH_EDIT -- Edit the field in the image header. 

procedure nh_edit (im, image, operation, keyws, exprs, comment, pkey, baf,
	                   update, verify, show, nfields)

pointer im		#I image descriptor
char    image[ARB]      #  
int	operation	#I operation code
char    keyws[ARB]      # Memc[fields]
char    exprs[ARB]	# Memc[valexpr]
char    comment[ARB]	# Memc[comment]
char    pkey[ARB]	# 
int	baf
int	update
int	verify
int	show
int	nfields

pointer sp, field
int	imgnfn(), imofnlu()
int	flist

begin

	call smark(sp)
	call salloc (field, SZ_FNAME, TY_CHAR)
	   
	if (operation == OP_INIT || operation == OP_ADD) {
	    # Add a field to the image header.  This cannot be done within
	    # the IMGNFN loop because template expansion on the existing
	    # fields of the image header would discard the new field name
	    # since it does not yet exist.

	    nfields = 1
	    call he_getopsetimage (im, image, keyws)
	    switch (operation) {
	    case OP_INIT:
	        call nh_initfield (im, image, keyws, exprs, comment, 
		    pkey, baf, verify, show, update)
	    case OP_ADD:
	        call nh_addfield (im, image, keyws, exprs, comment,
		    pkey, baf, verify, show, update)
	    }
	} else {
	    # Open list of fields to be processed.
	    flist = imofnlu (im, keyws)
	    nfields = 0
	    while (imgnfn (flist, Memc[field], SZ_FNAME) != EOF) {
		call he_getopsetimage (im, image, Memc[field])

		switch (operation) {
		case OP_EDIT:
		    call nh_editfield (im, image, Memc[field],
			exprs, comment, verify, show, update)
		case OP_RENAME:
		    call nh_renamefield (im, image, Memc[field],
			exprs, verify, show, update)
		case OP_DELETE:
		    call nh_deletefield (im, image, Memc[field],
			exprs, verify, show, update)
		}
		nfields = nfields + 1
	    }

	    call imcfnl (flist)
	}
	call sfree(sp)
end


# NH_EDITFIELD -- Edit the value of the named field of the indicated image.
# The value expression is evaluated, interactively inspected if desired,
# and the resulting value put to the image.

procedure nh_editfield (im, image, field, valexpr, comment, verify, 
    show, update)

pointer	im			# image descriptor of image to be edited
char	image[ARB]		# name of image to be edited
char	field[ARB]		# name of field to be edited
char	valexpr[ARB]		# value expression
char	comment[ARB]		# keyword comment
int	verify			# verify new value interactively
int	show			# print record of edit
int	update			# enable updating of the image

int	goahead, nl
pointer	sp, ip, oldval, newval, defval, o, fcomm, ncomm

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
	call salloc (fcomm, HRECLEN, TY_CHAR)
	call salloc (ncomm, HRECLEN, TY_CHAR)

	call strcpy (comment, Memc[ncomm], HRECLEN)

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

	call imgcom (im, field, Memc[fcomm])
	if (streq (Memc[newval], ".") && streq (comment, ".")) {
	    # Merely print the value of the field.

	    if (Memc[fcomm] == EOS) {
	    	call printf ("%s,%s = %s\n")
		call pargstr (image)
		call pargstr (field)
		call he_pargstr (Memc[oldval])
            } else {
	        call strcpy (Memc[oldval], Memc[newval], SZ_LINE)
		call printf ("%s,%s = %s / %s\n")
		call pargstr (image)
		call pargstr (field)
		call he_pargstr (Memc[oldval])
		call pargstr(Memc[fcomm])
	    }

	} else if (verify == YES) {
	    # Query for new value and edit the field.  If the response is a
	    # blank line, use the default new value.  If the response is "$"
	    # or EOF, do not change the value of the parameter.

	    if (streq (Memc[newval], ".")) {
		call strcpy (Memc[oldval], Memc[newval], SZ_LINE)
	    }
	    if (streq (comment, ".")) 
	        call strcpy (Memc[fcomm], Memc[ncomm], SZ_LINE)
	    call strcpy (Memc[newval], Memc[defval], SZ_LINE)
	    call eprintf ("%s,%s (%s -> %s): ")
		call pargstr (image)
		call pargstr (field)
		call nh_pargstrc (Memc[oldval], Memc[fcomm])
		call nh_pargstrc (Memc[defval], Memc[ncomm]) 
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
		if (goahead == YES && update == YES)
		    call nh_updatefield (im, image, field, Memc[oldval],
			Memc[newval], Memc[fcomm], Memc[ncomm], show)

		call flush (STDOUT)
	    }

	} else {
	    if (streq (Memc[newval], ".")) {
		call strcpy (Memc[oldval], Memc[newval], SZ_LINE)
	    }
	    if (streq (comment, ".")) 
	        call strcpy (Memc[fcomm], Memc[ncomm], SZ_LINE)
	    if (update == YES) {
		call nh_updatefield (im, image, field, Memc[oldval],
                      Memc[newval], Memc[fcomm], Memc[ncomm], show)
            }
	}
	if (update == NO && show == YES) {
	    call printf ("%s,%s: %s -> %s\n")
		call pargstr (image)
		call pargstr (field)
		call nh_pargstrc (Memc[oldval], Memc[fcomm])
		call nh_pargstrc (Memc[newval], Memc[ncomm])
	}

	call sfree (sp)
end


# NH_RENAMEFIELD -- Rename the named field of the indicated image.
# The value expression is evaluated, interactively inspected if desired,
# and the resulting value put to the image.

procedure nh_renamefield (im, image, field, valexpr, verify, show, update)

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
	call strupr (Memc[newval])

	if (verify == YES) {
	    # Query for new value and edit the field.  If the response is a
	    # blank line, use the default new value.  If the response is "$"
	    # or EOF, do not change the value of the parameter.

	    call strcpy (field, Memc[oldval], SZ_LINE)
	    if (streq (Memc[newval], "."))
		call strcpy (Memc[oldval], Memc[newval], SZ_LINE)
	    call strcpy (Memc[newval], Memc[defval], SZ_LINE)
	    call eprintf ("%s,%s (%s -> %s): ")
		call pargstr (image)
		call pargstr (field)
		call pargstr (field)
		call pargstr (Memc[newval])
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
		if (goahead == YES && update == YES)
		    call nh_updatekey (im, image, field, Memc[newval], show)

		call flush (STDOUT)
	    }

	} else {
	    call strcpy (field, Memc[oldval], SZ_LINE)
	    if (update == YES)
		call nh_updatekey (im, image, field, Memc[newval], show)
	}
	if (update == NO && show == YES) {
	    call printf ("%s,%s: %s -> %s\n")
		call pargstr (image)
		call pargstr (field)
		call pargstr (field)
		call pargstr (Memc[newval])
	}

	call sfree (sp)
end


# NH_INITFIELD -- Add a new field to the indicated image.  If the field already
# existsdo not set its value.  The value expression is evaluated and the
# resulting value used as the initial value in adding the field to the image.

procedure nh_initfield (im, image, field, valexpr, comment, pkey, baf,
       verify, show, update)

pointer	im			# image descriptor of image to be edited
char	image[ARB]		# name of image to be edited
char	field[ARB]		# name of field to be edited
char	valexpr[ARB]		# value expression
char	comment[ARB]		# keyword comment
char	pkey[ARB]		# 
int	baf
int	verify			# verify new value interactively
int	show			# print record of edit
int	update			# enable updating of the image

bool	numeric
int	numlen, ip
pointer	sp, newval, o
pointer	evexpr()
int	imaccf(), locpr(), strlen(), lexnum()
extern	he_getop()
errchk	imaccf, evexpr, imakbc, imastrc, imakic, imakrc

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
	    call xev_initop (o, strlen(valexpr), TY_CHAR)
	    call strcpy (valexpr, O_VALC(o), ARB)
	}

	# Add the field to the image (or update the value).  The datatype of
	# the expression value operand determines the datatype of the new
	# parameter.

	if (update == YES) {
	    switch (O_TYPE(o)) {
	    case TY_BOOL:
	        if (pkey[1] != EOS && baf != 0)
	            call imakbci (im, field, O_VALB(o), comment, pkey, baf)
	        else   
	            call imakbc (im, field, O_VALB(o), comment)
	    case TY_CHAR:
	        if (pkey[1] != EOS && baf != 0)
	            call imastrci (im, field, O_VALC(o), comment, pkey, baf)
	        else
	            call imastrc (im, field, O_VALC(o), comment)
	    case TY_INT:
	        if (pkey[1] != EOS && baf != 0)
	            call imakici (im, field, O_VALI(o), comment, pkey, baf)
	        else
	            call imakic (im, field, O_VALI(o), comment)
	    case TY_REAL:
	        if (pkey[1] != EOS && baf != 0)
	            call imakrci (im, field, O_VALR(o), comment, pkey, baf)
	        else
	            call imakrc (im, field, O_VALR(o), comment)
	    default:
	        call error (1, "unknown expression datatype")
	    }
	}

	if (show == YES) {
	    call he_encodeop (o, Memc[newval], SZ_LINE)
	    call printf ("add %s,%s = %s / %s\n")
		call pargstr (image)
		call pargstr (field)
		call he_pargstr (Memc[newval])
		call pargstr(comment)
	}

	call xev_freeop (o)
	call mfree (o, TY_STRUCT)
	call sfree (sp)
end


# NH_ADDFIELD -- Add a new field to the indicated image.  If the field already
# exists, merely set its value.  The value expression is evaluated and the
# resulting value used as the initial value in adding the field to the image.

procedure nh_addfield (im, image, field, valexpr, comment, pkey, baf,
            verify, show, update)

pointer	im			# image descriptor of image to be edited
char	image[ARB]		# name of image to be edited
char	field[ARB]		# name of field to be edited
char	valexpr[ARB]		# value expression
char	comment[ARB]		# keyword comment
char	pkey[ARB]		# pivot keyword name
int	baf			# either BEFORE or AFTER value
int	verify			# verify new value interactively
int	show			# print record of edit
int	update			# enable updating of the image

bool	numeric
int	numlen, ip
pointer	sp, newval, o
pointer	evexpr()
bool    streq()
int	imaccf(), locpr(), strlen(), lexnum()
extern	he_getop()
errchk	imaccf, evexpr, imakbc, imastrc, imakic, imakrc

begin
	call smark (sp)
	call salloc (newval, SZ_LINE, TY_CHAR)

	# If the named field already exists, this is really an edit operation
	# rather than an add.  Call editfield so that the usual verification
	# can take place.
        if (!streq(field, "comment") && !streq(field, "history")) {
	    if (imaccf (im, field) == YES) {
	        call nh_editfield (im, image, field, valexpr, comment,
	             verify, show, update)
	        call sfree (sp)
	        return
	    }
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
	    call strcpy (valexpr, O_VALC(o), SZ_LINE)
	}

	# Add the field to the image (or update the value).  The datatype of
	# the expression value operand determines the datatype of the new
	# parameter.
	if (update == YES) {
	    switch (O_TYPE(o)) {
	    case TY_BOOL:
	        if (pkey[1] != EOS && baf != 0)
	            call imakbci (im, field, O_VALB(o), comment, pkey, baf)
	        else
	            call imakbc (im, field, O_VALB(o), comment)
	    case TY_CHAR:
	        if (streq(field, "comment") || 
	            streq(field, "history") ||
	            streq(field, "add_textf") ||
		    streq(field, "add_blank")) {
                        if (streq(field, "add_textf")) {
	                    call imputextf (im, O_VALC(o), pkey, baf) 
		        } else {
	                    call imphis (im, field, O_VALC(o), pkey, baf) 
	                }
	        } else if (pkey[1] != EOS && baf != 0) {
	            call imastrci (im, field, O_VALC(o), comment, pkey, baf)
	        } else {
	            call imastrc (im, field, O_VALC(o), comment)
	        }
	    case TY_INT:
	        if (pkey[1] != EOS && baf != 0)
	            call imakici (im, field, O_VALI(o), comment, pkey, baf)
	        else
	            call imakic (im, field, O_VALI(o), comment)
	    case TY_REAL:
	        if (pkey[1] != EOS && baf != 0)
	            call imakrci (im, field, O_VALR(o), comment, pkey, baf)
	        else
	            call imakrc (im, field, O_VALR(o), comment)
	    default:
	        call error (1, "unknown expression datatype")
	    }
	}

	if (show == YES) {
	    call he_encodeop (o, Memc[newval], SZ_LINE)
	    call printf ("add %s,%s = %s / %s\n")
		call pargstr (image)
		call pargstr (field)
		call he_pargstr (Memc[newval])
		call pargstr(comment)
	}

	call xev_freeop (o)
	call mfree (o, TY_STRUCT)
	call sfree (sp)
end


# NH_DELETEFIELD -- Delete a field from the indicated image.  If the field does
# not exist, print a warning message.

procedure nh_deletefield (im, image, field, valexpr, verify, show, update)

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

            if (update == YES) {
	        iferr (call imdelf (im, field))
		    call erract (EA_WARN)
	        else if (show == YES) {
		    call printf ("%s,%s deleted\n")
		        call pargstr (image)
		        call pargstr (field)
	    } else if (show == YES)
		call printf ("%s,%s deleted, no update\n")
		    call pargstr (image)
		    call pargstr (field)
	    }
	}

	call sfree (sp)
end


# NH_UPDATEFIELD -- Update the value of an image header field.

procedure nh_updatefield (im, image, field, oldval, newval, oldcomm,
           newcomm, show)

pointer	im			# image descriptor
char	image[ARB]		# image name
char	field[ARB]		# field name
char	oldval[ARB]		# old value, encoded as a string
char	newval[ARB]		# new value, encoded as a string
char	oldcomm[ARB]		# old keyword comment
char	newcomm[ARB]		# new keyword comment
int	show			# print record of update

begin
	iferr (call impstrc (im, field, newval, newcomm)) {
	    call eprintf ("cannot update %s,%s\n")
		call pargstr (image)
		call pargstr (field)
	    return
	}
	if (show == YES) {
	    call printf ("%s,%s: %s -> %s\n")
		call pargstr (image)
		call pargstr (field)
		call nh_pargstrc (oldval, oldcomm)
		call nh_pargstrc (newval, newcomm)

	}
end


# NH_UPDATEKEY -- Update the image header field.

procedure nh_updatekey (im, image, field, newkey, show)

pointer	im			# image descriptor
char	image[ARB]		# image name
char	field[ARB]		# field name
char	newkey[ARB]		# new key
int	show			# print record of update

begin
	iferr (call imrenf (im, field, newkey)) {
	    call eprintf ("cannot update %s,%s\n")
		call pargstr (image)
		call pargstr (field)
	    return
	}
	if (show == YES) {
	    call printf ("%s,%s: %s -> %s\n")
		call pargstr (image)
		call pargstr (field)
		call pargstr (field)
		call pargstr (newkey)

	}
end


# NH_CPSTR -- Copy a string to a header record with optional comment.

procedure nh_cpstr (str, outbuf)

char    str[ARB]                	# string to be printed
char    outbuf[ARB]            		# comment string to be printed
                                                                                
int     ip
bool    quoteit
pointer sp, op, buf
                                                                                
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
            if (IS_WHITE(str[ip])) {            # detect whitespace
                quoteit = true
                Memc[op] = str[ip]
            } else if (str[ip] == '\n') {       # prettyprint newlines
                Memc[op] = '\\'
                op = op + 1
                Memc[op] = 'n'
            } else                              # normal characters
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
            call strcpy (Memc[buf], outbuf, SZ_LINE)
        } else
            call strcpy (str, outbuf, SZ_LINE)
                                                                                
        call sfree (sp)
end


# NH_PARGSTRC -- Pass a string to a printf statement plus the comment string.
										procedure nh_pargstrc (str, comment)

char	str[ARB]		# string to be printed
char	comment[ARB]		# comment string to be printed

pointer	sp, buf

begin

	call smark (sp)
        call salloc (buf, SZ_LINE, TY_CHAR)
                                                                                
        call nh_cpstr (str, Memc[buf])

        if (comment[1] != EOS) {
           call strcat (" / ", Memc[buf], SZ_LINE)
           call strcat (comment, Memc[buf], SZ_LINE)
        }

        call pargstr (Memc[buf])

        call sfree (sp)
end


# HE_GETPARS -- get the cl parameters for this task

procedure he_getpars (operation, fields, valexpr, comment,
	        pivot, baf, update, verify, show)
        
int	operation
pointer	fields			# template listing fields to be processed
pointer	valexpr			# the value expression (if op=edit|add)
char	comment[ARB]
char	pivot[ARB]
int	baf
int	update
int	verify
int	show
bool	clgetb(), streq()

pointer ip
int	btoi()

begin
	# Set switches.
	operation = OP_EDIT
	if (clgetb ("add"))
	    operation = OP_ADD
	else if (clgetb ("addonly"))
	    operation = OP_INIT
	else if (clgetb ("delete"))
	    operation = OP_DELETE
	else if (clgetb ("rename"))
	    operation = OP_RENAME

	# If fields is NULL then this will be done in a command file.
	if (fields != NULL) {

	    # Get list of fields to be edited, added, or deleted.
	    call clgstr ("fields", Memc[fields], SZ_LINE)
	    for (ip=fields;  IS_WHITE (Memc[ip]);  ip=ip+1)
		;
	    call strcpy (Memc[ip], Memc[fields], SZ_LINE)

	    # Set value expression.
	    Memc[valexpr] = EOS
	    if (operation != OP_DELETE) {
		call clgstr ("value", Memc[valexpr], SZ_LINE)
		if (operation != OP_RENAME)
		    call clgstr ("comment", comment, SZ_LINE)
		   
		# Justify value
		for (ip=valexpr;  IS_WHITE (Memc[ip]);  ip=ip+1)
		    ;
		call strcpy (Memc[ip], Memc[valexpr], SZ_LINE)
		ip = valexpr
		while (Memc[ip] != EOS)
		    ip = ip + 1
		while (ip > valexpr && IS_WHITE (Memc[ip-1]))
		    ip = ip - 1
		Memc[ip] = EOS
	    }

	    # If only printing results ignore the RENAME flag.
	    if (operation == OP_RENAME && streq (Memc[valexpr], ".")) {
		operation = OP_EDIT
		call strcpy (".", comment, SZ_LINE)
	    }

	} else {
	    Memc[valexpr] = EOS
	    comment[1] = EOS
	}


	# Get switches.  If the expression value is ".", meaning print value
	# rather than edit, then we do not use the switches.
	
	if (operation == OP_EDIT && streq (Memc[valexpr], ".") &&
	    streq (comment, ".")) {
	    update = NO
	    verify = NO
	    show   = NO
	} else {
	    update = btoi (clgetb ("update"))
	    verify = btoi (clgetb ("verify"))
	    show   = btoi (clgetb ("show"))
            call clgstr ("after", pivot, SZ_LINE)
            if (pivot[1] != EOS)
                baf = AFTER
            if (pivot[1] == EOS) {
                call clgstr ("before", pivot, SZ_LINE)
                if (pivot[1] != EOS)
                   baf = BEFORE
	    }
	}
end


# NH_SETPAR -- Set a parameter.

procedure nh_setpar (operation, dp_oper, dp_update, dp_verify, dp_show, 
                        update, verify, show)
int     operation
int     dp_oper
int     dp_update
int     dp_verify
int     dp_show
int     update
int     verify
int     show

begin
        # If the value is positive then the parameter has been set
        # in the command line.

	if (operation == OP_DEFPAR)
            operation = dp_oper
        if (update == -1)
            update = dp_update
	if (verify == -1)
            verify = dp_verify
        if (show == -1)
            show = dp_show
end
