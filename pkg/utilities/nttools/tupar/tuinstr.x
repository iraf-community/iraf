include <ctype.h>		# defines IS_WHITE
include <tbset.h>
include "tupar.h"		# defines TUPAR_EXIT & TUPAR_QUIT

# tu_instr -- execute edit instruction
# Execute one instruction regarding header parameters for a table:
# get, put, delete, replace, type, or list.
# The flag DONE will be set to true if the instruction is 'q' or
# if the user's response to a prompt is EOF.  Prompting is turned
# off if the input is redirected.
#
# Phil Hodge, 28-Mar-1988  Subroutine created
# Phil Hodge,  9-Sep-1988  Prompt changed for delete & replace
# Phil Hodge,  9-Mar-1989  Change data type of header parm from char to int.
# Phil Hodge, 23-Aug-1991  Include eq_flag, allowing quit without saving changes
# Phil Hodge,  9-Jul-1993  Set modified=true if header was changed.
# Phil Hodge,  7-Mar-1995  In tu_putpar, also put a comment, if present.
# Phil Hodge, 17-May-1995  In tu_putpar, allow ' as well as " as delimiter.
# Phil Hodge, 22-May-1996  Add "k" instruction.
# Phil Hodge,  5-Jun-1997  In tu_getpar and tu_listpar, also print comments
# Phil Hodge,  2-Jul-1998  In tu_putpar, check for "true" or "t" for a boolean
#			parameter; get data type from existing parameter;
#			in tu_listpar, print boolean as "yes" or "no".
# Ellyne Kinney, 2-Feb-1999 Testing Automatic updates of IRAFRA under CVS
# 			13th try.

procedure tu_instr (tp, linebuf, readonly, prompt, from_stdin,
		iredir, save_instr, isbuf, bufsize, ibp,
		modified, eq_flag, done, istat)

pointer tp			# i: pointer to table descriptor
char	linebuf[ARB]		# o: scratch for input line
bool	readonly		# i: was table opened readonly?
bool	prompt			# i: prompt for input?
bool	from_stdin		# i: get input from STDIN (or from buffer)
bool	iredir			# i: input redirected?
bool	save_instr		# i: save instruction?
pointer isbuf			# io: pointer to instruction buffer
int	bufsize			# io: current size of instruction buffer
int	ibp			# io: current index in instruction buffer
bool	modified		# io: set to true if the header was modified
int	eq_flag			# o: exit or quit
bool	done			# o: set to true if done with current table
int	istat			# o: > 0 if put or delete but table is readonly
#--
char	instr[11]		# instruction from user:  q, g, p, etc.
int	ip			# index in linebuf
int	clen			# length of command (to check for "!")
bool	verify			# verify before delete or replace?
bool	incl_num		# include par numbers when listing keywords?
int	ctowrd(), tu_gline(), tu_rd_instr()
int	strlen()
errchk	tu_getpar, tu_putpar, tu_delpar, tu_replpar, tu_ch_name

begin
	# default value in case user finishes by giving an EOF
	eq_flag = TUPAR_EXIT

	istat = 0
	done = false

	if (from_stdin) {
	    # Read an instruction from STDIN into linebuf.
	    if (prompt) {
		call eprintf (":")
		call flush (STDOUT)
		call flush (STDERR)
	    }
	    if (tu_gline (STDIN, linebuf) == EOF)
		done = true
	} else {
	    # Read an instruction from buffer into linebuf.
	    if (tu_rd_instr (Memc[isbuf], ibp, linebuf) == EOF)
		done = true
	}
	if ( done )
	    return

	ip = 1
	if (ctowrd (linebuf, ip, instr, 11) <= 0)	# a blank line
	    return
	if (instr[1] == '#')				# a comment line
	    return

	if (instr[1] == 'e') {
	    eq_flag = TUPAR_EXIT
	    done = true

	} else if (instr[1] == 'q') {
	    clen = strlen (instr)
	    if (instr[clen] == '!')
		eq_flag = TUPAR_QUIT_NC
	    else
		eq_flag = TUPAR_QUIT
	    done = true

	} else if (instr[1] == 'g') {

	    call tu_getpar (tp, linebuf, ip, instr,
			save_instr, isbuf, bufsize, ibp)

	} else if (instr[1] == 'p') {

	    if (readonly) {
		istat = 1
		return
	    }
	    call tu_putpar (tp, linebuf, ip, instr,
			save_instr, isbuf, bufsize, ibp)
	    modified = true

	} else if (instr[1] == 'd') {

	    # Delete a header parameter specified by name or by number.
		
	    if (readonly) {
		istat = 1
		return
	    }
	    verify = ( ! iredir ) && (instr[2] != '!')
	    call tu_delpar (tp, linebuf, ip, verify,
			save_instr, isbuf, bufsize, ibp, modified)

	} else if (instr[1] == 'r') {

	    # Replace a header parameter specified by name or by number.
		
	    if (readonly) {
		istat = 1
		return
	    }
	    verify = ( ! iredir ) && (instr[2] != '!')
	    call tu_replpar (tp, linebuf, ip, prompt, from_stdin,
			verify, save_instr, isbuf, bufsize, ibp,
			modified, done)

	} else if (instr[1] == 'k') {

	    # Change keyword name.
		
	    if (readonly) {
		istat = 1
		return
	    }
	    call tu_ch_name (tp, linebuf, ip,
			save_instr, isbuf, bufsize, ibp,
			modified)

	} else if (instr[1] == 't' || instr[1] == 'l') {

	    # Type or list parameters; list means include keyword number.

	    incl_num = instr[1] == 'l'		# list rather than type
	    call tu_listpar (tp, linebuf, ip, incl_num,
			save_instr, isbuf, bufsize, ibp)

	} else {
	    call eprintf ("The options are:\n")
	    call eprintf (
		"   e, q, g, p, d, r, t, l\n")
	    call eprintf (
		"   (exit, quit, get, put, delete, replace, type, list)\n")
	    call eprintf (
		"  e    exit the task, saving changes\n")
	    call eprintf (
		"  q    quit the task WITHOUT saving any changes\n")
	    call eprintf (
		"  g keyword         get parameter with keyword `keyword'\n")
	    call eprintf (
		"  p keyword value   put parameter `keyword'\n")
	    call eprintf (
		"  d keyword         delete parameter `keyword'\n")
	    call eprintf (
		"  r keyword         replace parameter `keyword'\n")
	    call eprintf (
		"  k oldkey newkey   change keyword name\n")
	    call eprintf (
		"  t                 type the parameters\n")
	    call eprintf (
		"  l                 list parameters and show par numbers\n")
	    call eprintf (
		"      see help for further info about these instructions\n")
	}
end


# tu_getpar -- get a parameter
# The value of a parameter specified by name (not by number) will be gotten
# and displayed.  If the keyword is not found in the header, nothing will
# be displayed (i.e. no error message).  If the keyword is HISTORY, COMMENT,
# or a blank, then all keywords of that type will be displayed.

procedure tu_getpar (tp, linebuf, ip, instr,
		save_instr, isbuf, bufsize, ibp)

pointer tp			# i: pointer to table descriptor
char	linebuf[ARB]		# i: input line
int	ip			# io: index in linebuf
char	instr[ARB]		# i: the instruction (needed for data type)
bool	save_instr		# i: save instruction?
pointer isbuf			# io: pointer to instruction buffer
int	bufsize			# io: current size of instruction buffer
int	ibp			# io: current index in instruction buffer
#--
char	keyword[SZ_KEYWORD]	# keyword for parameter
char	kwrd[SZ_KEYWORD]	# keyword returned by tbhgnp
char	text[SZ_PARREC]		# buffer for value of parameter
char	comment[SZ_PARREC]	# buffer for comment, if any
int	dtype			# data type (TY_CHAR, etc)
double	dblval
real	realval
int	intval
bool	boolval
int	npar			# current number of parameters
int	k			# loop index
double	tbhgtd()
real	tbhgtr()
int	tbhgti(), tbpsta()
bool	tbhgtb()
int	ctowrd()
bool	streq()

begin
	npar = tbpsta (tp, TBL_NPAR)

	if (ctowrd (linebuf, ip, keyword, SZ_KEYWORD) <= 0) {
	    call eprintf ("syntax:  g keyword\n")
	    return				# get next instruction
	}
	call strupr (keyword)

	# Get comment, if it exists.
	iferr (call tbhgcm (tp, keyword, comment, SZ_PARREC))
	    comment[1] = EOS

	if (instr[2] == 'r') {
	    ifnoerr (realval = tbhgtr (tp, keyword)) {
		call printf ("%s = %g")
		    call pargstr (keyword)
		    call pargr (realval)
		if (comment[1] == EOS) {
		    call printf ("\n")
		} else {
		    call printf ("  %s\n")
			call pargstr (comment)
		}
	    }
	} else if (instr[2] == 'd') {
	    ifnoerr (dblval = tbhgtd (tp, keyword)) {
		call printf ("%s = %g")
		    call pargstr (keyword)
		    call pargd (dblval)
		if (comment[1] == EOS) {
		    call printf ("\n")
		} else {
		    call printf ("  %s\n")
			call pargstr (comment)
		}
	    }
	} else if (instr[2] == 'i') {
	    ifnoerr (intval = tbhgti (tp, keyword)) {
		call printf ("%s = %d")
		    call pargstr (keyword)
		    call pargi (intval)
		if (comment[1] == EOS) {
		    call printf ("\n")
		} else {
		    call printf ("  %s\n")
			call pargstr (comment)
		}
	    }
	} else if (instr[2] == 'b') {
	    ifnoerr (boolval = tbhgtb (tp, keyword)) {
		call printf ("%s = %b")
		    call pargstr (keyword)
		    call pargb (boolval)
		if (comment[1] == EOS) {
		    call printf ("\n")
		} else {
		    call printf ("  %s\n")
			call pargstr (comment)
		}
	    }
	} else {
	    if (streq (keyword, "HISTORY") || streq (keyword, "COMMENT") ||
			keyword[1] == EOS) {
		# Print all history or comment or blank-keyword records.
		do k = 1, npar {
		    call tbhgnp (tp, k, kwrd, dtype, text)
		    if (streq (keyword, kwrd)) {
			call printf ("%s = %s\n")
			    call pargstr (keyword)
			    call pargstr (text)
		    }
		}
	    } else {
		ifnoerr (call tbhgtt (tp, keyword, text, SZ_PARREC)) {
		    if (comment[1] == EOS) {
			call printf ("%s = %s\n")
			    call pargstr (keyword)
			    call pargstr (text)
		    } else {
			call printf ("%s = '%s'  %s\n")
			    call pargstr (keyword)
			    call pargstr (text)
			    call pargstr (comment)
		    }
		}
	    }
	}

	if (save_instr)
	    call tu_save_instr (linebuf, isbuf, bufsize, ibp)
end


# tu_putpar -- add or replace a parameter
# A parameter specified by name (not by number) is put into the table.
# If the parameter already exists it will be replaced (and the data type
# may be changed); otherwise, it will be added.

procedure tu_putpar (tp, linebuf, ip, instr,
		save_instr, isbuf, bufsize, ibp)

pointer tp			# i: pointer to table descriptor
char	linebuf[ARB]		# i: input line
int	ip			# io: index in linebuf
char	instr[ARB]		# i: the instruction (needed for data type)
bool	save_instr		# i: save instruction?
pointer isbuf			# io: pointer to instruction buffer
int	bufsize			# io: current size of instruction buffer
int	ibp			# io: current index in instruction buffer
#--
pointer sp
pointer value			# scratch for a string value
char	keyword[SZ_KEYWORD]	# keyword for parameter
int	dtype			# data type code for parameter
bool	found			# parameter already in header? (ignored)
double	dblval
real	realval
int	intval
bool	boolval
int	npar			# current number of parameters
int	tbpsta()
int	nchar, ctowrd(), ctod(), ctoi(), nscan(), strlen()
bool	streq()

begin
	npar = tbpsta (tp, TBL_NPAR)

	if (ctowrd (linebuf, ip, keyword, SZ_KEYWORD) <= 0) {
	    call eprintf ("syntax:  p keyword value\n")
	    return
	}

	call smark (sp)
	call salloc (value, SZ_FNAME, TY_CHAR)

	# Set the data type.  If the user specified a type, use that.
	# If none was specified, find out whether the parameter is
	# already in the header, and if so, use the existing type.
	if (instr[2] == 'r')
	    dtype = TY_REAL
	else if (instr[2] == 'd')
	    dtype = TY_DOUBLE
	else if (instr[2] == 'i')
	    dtype = TY_INT
	else if (instr[2] == 'b')
	    dtype = TY_BOOL
	else if (instr[2] == 't')
	    dtype = TY_CHAR
	else
	    call tu_partype (tp, keyword, dtype, found)

	# In each section, we use ctowrd or ctod (or something), not only
	# to extract the value but also to skip past it so we can check for
	# a comment.
	if (dtype == TY_REAL) {
	    if (ctod (linebuf, ip, dblval) > 0)
		realval = dblval
	    else
		realval = INDEFR
	    call tbhadr (tp, keyword, realval)
	} else if (dtype == TY_DOUBLE) {
	    if (ctod (linebuf, ip, dblval) < 1)
		dblval = INDEFD
	    call tbhadd (tp, keyword, dblval)
	} else if (dtype == TY_INT) {
	    if (ctoi (linebuf, ip, intval) < 1)
		intval = INDEFI
	    call tbhadi (tp, keyword, intval)
	} else if (dtype == TY_BOOL) {
	    nchar = ctowrd (linebuf, ip, Memc[value], SZ_FNAME)
	    call strlwr (Memc[value])
	    call sscan (Memc[value])
		call gargb (boolval)
	    if (nscan() < 1) {
		if (streq (Memc[value], "true") || streq (Memc[value], "t") ||
		    streq (Memc[value], "1"))
		    boolval = true
		else
		    boolval = false
	    }
	    call tbhadb (tp, keyword, boolval)

	} else {

	    while (IS_WHITE(linebuf[ip]))
		ip = ip + 1

	    if (linebuf[ip] == '"' || linebuf[ip] == '\'') {
		nchar = ctowrd (linebuf, ip, Memc[value], SZ_FNAME)
		call tbhadt (tp, keyword, Memc[value])
	    } else {
		call tbhadt (tp, keyword, linebuf[ip])
		# Set ip to point to end of line because there's no comment.
		ip = strlen (linebuf) + 1
	    }
	}

	# Check for a comment.
	while (IS_WHITE(linebuf[ip]))
	    ip = ip + 1
	if (linebuf[ip] != EOS)
	    call tbhpcm (tp, keyword, linebuf[ip])

	if (save_instr)
	    call tu_save_instr (linebuf, isbuf, bufsize, ibp)

	call sfree (sp)
end


# tu_delpar -- delete a parameter
# A parameter is to be deleted.  The user will be prompted for confirmation
# if the input is not redirected and the instruction was not 'q!'.

procedure tu_delpar (tp, linebuf, ip, verify,
		save_instr, isbuf, bufsize, ibp, modified)

pointer tp			# i: pointer to table descriptor
char	linebuf[ARB]		# i: input line
int	ip			# i: index in linebuf
bool	verify			# i: ask for verification before deleting?
bool	save_instr		# i: save instruction?
pointer isbuf			# io: pointer to instruction buffer
int	bufsize			# io: current size of instruction buffer
int	ibp			# io: current index in instruction buffer
bool	modified		# io: set to true if parameter was deleted
#--
char	keyword[SZ_KEYWORD]	# keyword for parameter
char	text[SZ_PARREC]		# buffer for value of parameter
char	char_type		# data type as a letter
int	dtype			# data type
int	par1, par2		# range of par numbers to delete
int	i			# loop index
int	parnum			# keyword number
int	ctowrd()
bool	clgetb()

begin
	i = ip

	if (ctowrd (linebuf, i, keyword, SZ_KEYWORD) <= 0) {
	    call eprintf ("syntax:  d keyword   (or  d parnum)\n")
	    return
	}

	# Get the parameter numbers; = 0,-1 if not found.
	call tu_parnum (tp, linebuf[ip], par1, par2)

	# Delete in increasing numerical order.  That means that if we
	# delete every one, it will be the same par number each time.
	# Whenever we don't delete one, we increment parnum.
	parnum = par1
	do i = par1, par2 {
	    # Get parameter by number.
	    call tbhgnp (tp, parnum, keyword, dtype, text)
	    if (dtype == 0)
		call error (1, "tu_delpar:  keyword miscount")

	    if ( verify ) {
		# Change data type to a char.
		switch (dtype) {
		case TY_REAL:
		    char_type = 'r'
		case TY_INT:
		    char_type = 'i'
		case TY_DOUBLE:
		    char_type = 'd'
		case TY_BOOL:
		    char_type = 'b'
		default:
		    char_type = 't'
		}
		# Ask for confirmation before deleting.
		call clputb ("go_ahead", clgetb ("delete_default"))
		call eprintf (
		    "The following parameter is to be deleted:\n")
		call eprintf ("%-8s %c %s\n")
		    call pargstr (keyword)
		    call pargc (char_type)
		    call pargstr (text)
		call eprintf ("   ...   OK to delete")
		call flush (STDERR)
		if (clgetb ("go_ahead")) {
		    call tbhdel (tp, parnum)	# delete it
		    modified = true
		} else {
		    parnum = parnum + 1		# point to next parameter
		}
	    } else {
		# Delete without asking for confirmation.
		call tbhdel (tp, parnum)
		modified = true
	    }
	}

	# Note that we may save this instruction even if the parameter
	# was not found in the current table.
	if (save_instr)
	    call tu_save_instr (linebuf, isbuf, bufsize, ibp)
end


# tu_replpar -- replace a parameter
# Replace an existing parameter, specified either by name or by number.
# The instruction and the replacement string will be saved in the instruction
# buffer if appropriate.  Neither will be saved, however, if the keyword is
# not found in the first table.  (This is in contrast to the behavior of the
# delete instruction.)  The user will be prompted for confirmation if the
# input is not redirected and the instruction was not 'r!'.

procedure tu_replpar (tp, linebuf, ip, prompt, from_stdin,
		verify, save_instr, isbuf, bufsize, ibp,
		modified, done)

pointer tp			# i: pointer to table descriptor
char	linebuf[ARB]		# i: input line
int	ip			# i: index in linebuf
bool	prompt			# i: prompt for input?
bool	from_stdin		# i: get instructions from STDIN?
bool	verify			# i: ask for verification before deleting?
bool	save_instr		# i: save instruction?
pointer isbuf			# io: pointer to instruction buffer
int	bufsize			# io: current size of instruction buffer
int	ibp			# io: current index in instruction buffer
bool	modified		# io: set to true if parameter was replaced
bool	done			# io: set to false if done with current table
#--
char	keyword[SZ_KEYWORD]	# keyword for parameter
char	text[SZ_PARREC]		# buffer for value of parameter
char	rtext[SZ_PARREC]	# replacement value for a parameter
char	char_type		# data type as a letter
int	dtype			# data type (TY_CHAR, etc)
int	par1, par2		# range of keywords to replace
int	i			# loop index for keyword number
int	ctowrd(), tu_gline(), tu_rd_instr()
bool	clgetb()

begin
	i = ip

	if (ctowrd (linebuf, i, keyword, SZ_KEYWORD) <= 0) {
	    call eprintf ("syntax:  r keyword   (or  r parnum)\n")
	    return
	}

	# Save the instruction; the replacement value(s) will be 
	# saved within the loop over keyword number.
	if (save_instr)
	    call tu_save_instr (linebuf, isbuf, bufsize, ibp)

	# Get the parameter numbers, 0,-1 if not found.
	call tu_parnum (tp, linebuf[ip], par1, par2)

	do i = par1, par2 {
	    # Get parameter by number.
	    call tbhgnp (tp, i, keyword, dtype, text)
	    if (dtype == 0)
		call error (1, "tu_replpar:  keyword miscount")

	    # Change data type to a char.
	    switch (dtype) {
	    case TY_REAL:
		char_type = 'r'
	    case TY_INT:
		char_type = 'i'
	    case TY_DOUBLE:
		char_type = 'd'
	    case TY_BOOL:
		char_type = 'b'
	    default:
		char_type = 't'
	    }

	    if (prompt) {
		# Display current value.
		call eprintf (
		    "keyword %s, type %c; give replacement value:\n")
		    call pargstr (keyword)
		    call pargc (char_type)
		call eprintf ("%s\n")
		    call pargstr (text)
	    }
	    # Read replacement text, either from STDIN or from instr buffer.
	    if (from_stdin) {
		if (tu_gline (STDIN, rtext) == EOF) {
		    done = true
		    return
		}
	    } else {
		if (tu_rd_instr (Memc[isbuf], ibp, rtext) == EOF) {
		    done = true
		    return
		}
	    }

	    # Tab is saved in the instruction buffer to mean that the
	    # value should not be changed.  This allows replacing a value
	    # with blanks.
	    if (rtext[1] == EOS) {
		call eprintf ("no action taken\n")
		call strcpy ("\t", rtext, SZ_PARREC)

	    } else if (rtext[1] == '\t') {
		;

	    } else if (verify) {
		# Prompt for confirmation.
		call clputb ("go_ahead", clgetb ("delete_default"))
		call eprintf ("Current parameter and its replacement are:\n")
		call eprintf ("%-8s %c %s\n")
		    call pargstr (keyword)
		    call pargc (char_type)
		    call pargstr (text)
		call eprintf ("%-8s %c %s\n")
		    call pargstr (keyword)
		    call pargc (char_type)
		    call pargstr (rtext)
		call eprintf ("   ...   OK to replace")
		call flush (STDERR)
		if (clgetb ("go_ahead")) {
		    call tbhpnp (tp, i, keyword, dtype, rtext)	# replace it
		    modified = true
		} else {
		    call eprintf ("not replaced\n")
		}

	    } else {
		# Replace the value without prompting.
		call tbhpnp (tp, i, keyword, dtype, rtext)
		modified = true
	    }

	    # Save the replacement value.
	    if (save_instr)
		call tu_save_instr (rtext, isbuf, bufsize, ibp)
	}
end

# tu_ch_name -- change keyword name
# Replace the name of an existing keyword without changing either the
# value or comment.
# The instruction and the replacement string will be saved in the instruction
# buffer if appropriate.  Neither will be saved, however, if the keyword is
# not found in the first table.  The user will be prompted for confirmation
# if the input is not redirected and the instruction was not 'k!'.

procedure tu_ch_name (tp, linebuf, ip,
		save_instr, isbuf, bufsize, ibp,
		modified)

pointer tp			# i: pointer to table descriptor
char	linebuf[ARB]		# i: input line
int	ip			# i: index in linebuf
bool	save_instr		# i: save instruction?
pointer isbuf			# io: pointer to instruction buffer
int	bufsize			# io: current size of instruction buffer
int	ibp			# io: current index in instruction buffer
bool	modified		# io: set to true if parameter was replaced
#--
char	oldkey[SZ_KEYWORD]	# current keyword
char	newkey[SZ_KEYWORD+1]	# new keyword; extra space for testing length
int	i
int	parnum			# parameter specified by number (zero)
bool	insufficient_input	# true if not enough input was given
int	ctowrd(), strlen()
errchk	tbhckn

begin
	i = ip

	insufficient_input = false		# initial value

	if (ctowrd (linebuf, i, oldkey, SZ_KEYWORD) <= 0)
	    insufficient_input = true
	if (ctowrd (linebuf, i, newkey, SZ_KEYWORD+1) <= 0)
	    insufficient_input = true

	if (insufficient_input) {
	    call eprintf ("syntax:  k oldkey newkey\n")
	    return
	}

	if (strlen (newkey) > SZ_KEYWORD) {
	    call eprintf ("new keyword name is too long; limit is %d\n")
		call pargi (SZ_KEYWORD)
	    return
	}

	# Save the instruction.
	if (save_instr)
	    call tu_save_instr (linebuf, isbuf, bufsize, ibp)

	# Replace the keyword name.
	parnum = 0
	call tbhckn (tp, oldkey, parnum, newkey)
	modified = true
end


# tu_listpar -- list parameters
# Either all parameters or a range of parameters specified by number
# may be displayed.  The parameter numbers may optionally be displayed.

procedure tu_listpar (tp, linebuf, ip, incl_num,
		save_instr, isbuf, bufsize, ibp)

pointer tp			# i: pointer to table descriptor
char	linebuf[ARB]		# i: input line
int	ip			# io: index in linebuf
bool	incl_num		# i: include number when listing parameters?
bool	save_instr		# i: save instruction?
pointer isbuf			# io: pointer to instruction buffer
int	bufsize			# io: current size of instruction buffer
int	ibp			# io: current index in instruction buffer
#--
char	keyword[SZ_KEYWORD]	# keyword for parameter
char	text[SZ_PARREC]		# buffer for value of parameter
char	comment[SZ_PARREC]	# buffer for comment, if any
char	char_type		# data type as a letter
int	dtype			# data type (a character constant)
int	j1, j2			# loop bounds:  first & last par numbers
int	npar			# current number of parameters
int	k			# loop index
int	tbpsta()
int	ctoi()

begin
	npar = tbpsta (tp, TBL_NPAR)

	# Get the range of keywords to list.
	if (ctoi (linebuf, ip, j1) <= 0) {
	    j1 = 1
	    j2 = npar
	} else if (ctoi (linebuf, ip, j2) <= 0) {
	    j2 = j1
	}
	if (j2 < j1) {
	    k = j1			# swap j1, j2
	    j1 = j2
	    j2 = k
	}
	if (j1 > npar || j2 < 1) {
	    call eprintf ("out of range; max is %d\n")
		call pargi (npar)
	    j1 = 1			# so loop will not be executed
	    j2 = 0
	}
	j1 = max (j1, 1)
	j2 = min (j2, npar)
	do k = j1, j2 {
	    call tbhgnp (tp, k, keyword, dtype, text)
	    call tbhgcm (tp, keyword, comment, SZ_PARREC)
	    # Change data type to a char.
	    switch (dtype) {
	    case TY_REAL:
		char_type = 'r'
	    case TY_INT:
		char_type = 'i'
	    case TY_DOUBLE:
		char_type = 'd'
	    case TY_BOOL:
		char_type = 'b'
	    default:
		char_type = 't'
	    }
	    if (incl_num) {			# include keyword number
		call printf ("%2d ")
		    call pargi (k)
	    }
	    call printf ("%-8s %c")
		call pargstr (keyword)
		call pargc (char_type)
	    if (comment[1] == EOS) {
		if (dtype == TY_BOOL && text[1] == '1') {
		    call printf (" yes\n")
		} else if (dtype == TY_BOOL && text[1] == '0') {
		    call printf (" no\n")
		} else {
		    call printf (" %s\n")
			call pargstr (text)
		}
	    } else {				# also print comment
		if (char_type == 't') {
		    call printf (" '%s'")	# enclose text in quotes
			call pargstr (text)
		} else if (dtype == TY_BOOL) {
		    if (text[1] == '1') {
			call printf (" yes")
		    } else if (text[1] == '0') {
			call printf (" no")
		    } else {
			call printf (" %s")
			    call pargstr (text)
		    }
		} else {
		    call printf (" %s")		# no quotes needed
			call pargstr (text)
		}
		call printf ("  %s\n")
		    call pargstr (comment)
	    }
	}

	if (save_instr)
	    call tu_save_instr (linebuf, isbuf, bufsize, ibp)
end


# tu_gline -- getline without newline
# Read a line using getline, and replace the newline character with EOS.
# Either EOF or the number of char read before the newline will be returned.

int procedure tu_gline (fd, linebuf)

int	fd		# i: identifies input file
char	linebuf[ARB]	# o: output buffer for text that was read
#--
int	istat
int	k
int	getline()

begin
	istat = getline (fd, linebuf)
	if (istat == EOF)
	    return (istat)

	k = 1
	while (linebuf[k] != EOS) {
	    if (linebuf[k] == '\n') {
		linebuf[k] = EOS
		break
	    }
	    k = k + 1
	}
	return (k-1)
end


# tu_parnum -- get parameter number
# Either one or a pair of keywords may be given as input in the string
# 'keyword', and each may be specified either by name or by number.
# This routine reads the numbers and/or names and converts keyword
# names to numbers.  If the parameter (or either of two) is not found, or
# if the number is larger than the number of header keywords, par1
# will be set to 0 and par2 to -1.  If only one keyword is given, par2
# will be set equal to par1.

procedure tu_parnum (tp, keyword, par1, par2)

pointer tp		# i: pointer to table descriptor
char	keyword[ARB]	# i: keyword name or number
int	par1		# o: number of first parameter to delete
int	par2		# o: number of last parameter to delete
#--
char	key1[SZ_KEYWORD], key2[SZ_KEYWORD]
int	ip		# counter within keyword
int	ipi		# counter in key1 or key2
int	temp		# for swapping par1 & par2
int	npar		# total number of header parameters
int	nchar
int	ctowrd(), ctoi(), tbpsta()

begin
	npar = tbpsta (tp, TBL_NPAR)
	# Default values so a loop from par1 to par2 will not execute.
	par1 = 0
	par2 = -1

	# Extract the first (and possibly only) word.
	ip = 1
	nchar = ctowrd (keyword, ip, key1, SZ_KEYWORD)
	if (nchar < 1)
	    return				# nothing given

	# Interpret the first word as a number or name.  First try to
	# read it as an integer.  If it's not an integer, or if there's
	# something after an integer part (e.g. key1 = "37test"), then
	# treat it as a keyword name.
	ipi = 1
	nchar = ctoi (key1, ipi, par1)
	if ( (nchar <= 0) || (key1[ipi] != EOS) )
	    call tbhfkw (tp, key1, par1)	# get the par number

	if (par1 < 1) {
	    call eprintf ("warning:  keyword `%s' not found\n")
		call pargstr (key1)
	    return
	}

	nchar = ctowrd (keyword, ip, key2, SZ_KEYWORD)	# read second word
	if (nchar < 1) {
	    par2 = par1				# there was only one word
	} else {
	    ipi = 1
	    nchar = ctoi (key2, ipi, par2)
	    if ( (nchar <= 0) || (key2[ipi] != EOS) )
		call tbhfkw (tp, key2, par2)
	    if (par2 < 1) {
		call eprintf ("warning:  keyword `%s' not found\n")
		    call pargstr (key2)
		return
	    }
	    if (par1 > par2) {
		temp = par2
		par2 = par1
		par1 = temp
	    }
	}

	if (par1 > npar || par2 > npar) {
	    call eprintf (
	"there are only %d header parameters; no action taken\n")
		call pargi (npar)
	    par1 = 0
	    par2 = -1
	}
end


# tu_partype -- get data type of parameter
# This routine looks for the given keyword in the header.  If it is found,
# the data type (integer code) is returned as dtype.  If not, dtype is set
# to the default TY_CHAR, and found is set to false.

procedure tu_partype (tp, keyword, dtype, found)

pointer tp		# i: pointer to table descriptor
char	keyword[ARB]	# i: keyword name
int	dtype		# o: data type (TY_INT, TY_CHAR, ...)
bool	found		# o: true if keyword was found in header.
#--
char	kwrd[SZ_KEYWORD]	# keyword returned by tbhgnp
char	value[SZ_PARREC]	# buffer for value of parameter
int	parnum

begin
	# Get the keyword number, or zero if it isn't in the header.
	call tbhfkw (tp, keyword, parnum)

	# Get the data type, ignoring the value.
	if (parnum > 0) {
	    call tbhgnp (tp, parnum, kwrd, dtype, value)
	    found = true
	} else {
	    dtype = TY_CHAR		# default
	    found = false
	}
end
