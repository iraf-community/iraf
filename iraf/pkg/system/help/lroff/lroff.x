# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<chars.h>
include	<ctype.h>
include	"lroff.h"

.help lroff
.nf ___________________________________________________________________________
Source for the LROFF line-oriented text formatter.  LROFF is a simple text
formatter patterned after the NROFF formatter of UNIX.  LROFF is unusual in
that it is a "numerical" library procedure which does not directly do any
i/o (except for buffer allocation and use of ERROR).

LROFF -- The main entry point.  Lroff reads lines with the input procedure,
performs the format conversion, and writes lines with the output procedure,
both of which are passed as arguments.  Conversion proceeds until the .endhelp
directive or EOF is reached.  The calling sequences for the input and output
procedures are as follows:

	stat =	 inproc (inarg, linebuf)
		outproc (outarg, linebuf)

where "inarg" and "outarg" are magic integer arguments which Lroff merely
passes on to the input and output procedures when they are called.

Other arguments to Lroff include PLM and PRM, the permanent left and right
margins for the output text, and SOFLAG, set to YES if "standout mode" control
chars are permitted in the output text.

The forms control directives BP, TP, KS, and KE are ignored unless forms mode
is enabled (foflag=YES).  If forms control is enabled these directives cause
a breakline followed by output of a special control character forms directive,
used to control the layout of text on a page.  When forms mode is in effect
the section header directives also cause output of a TP (test page) directive
before the section.  Processing of forms control characters is left to the
program that reads lroff output.
.endhelp ______________________________________________________________________

procedure lroff (in, in_arg, out, out_arg, plm, prm, soflag_val, foflag_val)

extern	in()			# called to get lines of input text
int	in_arg			# magic argument for in()
extern	out()			# called to output formatted lines of text
int	out_arg			# magic argument for out()
int	plm, prm		# permanent left and right margins
int	soflag_val		# output standout mode control chars?
int	foflag_val		# output form control chars?

char	ctrlstr[2]
pointer	sp, ibuf
int	ip, command, last_command
int	in(), nextcmd(), lgetarg(), input(), nofill()

errchk	input, textout, nofill, rawcopy, skiplines, breakline, right_justify
errchk	salloc, new_section, new_indented_section, center_text, init_ls
errchk	init_nh, indent_left_margin, do_LS, textout, set_wordbuf, set_outbuf
include	"lroff.com"

define	text_	98

begin
	call smark (sp)
	call salloc (ibuf, SZ_IBUF, TY_CHAR)

	if (plm > prm || plm < 1)
	    call error (1, "Lroff called with invalid margins")

	# General initialization.  Set up the Lroff common.  Call the various
	# initialization procedures to initialize the directives and to
	# set up the word buffer and output buffer, the size of which depends
	# on the margins.

	justify = YES
	perm_left_margin = plm
	perm_right_margin = prm
	left_margin = plm
	right_margin = prm
	last_command = NULL
	in_magic_arg = in_arg
	out_magic_arg = out_arg
	soflag = soflag_val
	foflag = foflag_val
	standout_mode_enabled = false

	ls_indent = DEF_LSINDENT
	ih_indent = DEF_IHINDENT
	sh_nskip  = DEF_SHNSKIP
	nh_nskip  = DEF_NHNSKIP
	ih_nskip  = DEF_IHNSKIP

	call init_ls()
	call init_nh()
	call set_wordbuf (prm - plm + 2)
	call set_outbuf (max (SZ_LINE, 2 * (prm - plm + 1)))

	# If the first line of text is not an Lroff directive, we copy the
	# input to the output without modification, except for moving the text
	# to the desired left margin, stopping only at EOF or .endhelp.
	# If any directive is given, the default mode is justify+fill.

	if (input (in, Memc[ibuf]) == EOF) {
	    call sfree (sp)
	    return
	} else if (nextcmd (Memc[ibuf], ip) < 0) {
	    call rawcopy (in, out, Memc[ibuf])
	    call sfree (sp)
	    return
	}


	# The main Lroff interpreter loop.  Get input line: if directive,
	# execute directive; else call textout() to process a line of text.
	# The basic idea is to break the input stream up into words, saving
	# these until we have one more than needed to fill the output line.
	# The words are then copied into the output line, starting at the left
	# margin, adding spaces as needed to right justify the line.  Many
	# commands cause the "current output line" to be broken, forcing
	# whatever has been accumulated out without right justification.

	repeat {
	    command = nextcmd (Memc[ibuf], ip)
	    switch (command) {
	    case FI:
		call breakline (out, NJ)
	    case NF:
		call breakline (out, NJ)
		if (nofill (in, out, Memc[ibuf]) == ENDHELP)
		    break
	    case JU:
		justify = YES
	    case NJ:
		justify = NO
	    case RJ:
		call right_justify (in, out, Memc[ibuf], ip)
	    case SH:
		call new_section (in, out, Memc[ibuf], ip)
	    case IH:
		call new_indented_section (in, out, Memc[ibuf], ip)
	    case NH:
		call new_numbered_section (in, out, Memc[ibuf], ip)
	    case BR:
		call breakline (out, NJ)
	    case CE:
		call center_text (in, out, Memc[ibuf], ip)
	    case SP:
		call skiplines (out, lgetarg(Memc[ibuf],ip,1))
	    case IN:
		call indent_left_margin (in, out, lgetarg(Memc[ibuf],ip,0))
	    case LS, LE:
		call do_LS (out, Memc[ibuf+ip-1], command, last_command)

	    case HR:
		# HTML href target of the form ".hr <href> <text>", we skip
		# ahead to the <text> and process as a normal line.
		while (IS_WHITE(Memc[ibuf+ip]))		# skip space
		    ip = ip + 1
		while (!IS_WHITE(Memc[ibuf+ip]))	# skip <href>
		    ip = ip + 1
		call amovc (Memc[ibuf+ip+1], Memc[ibuf], SZ_IBUF)
		ip = 0
		goto text_

	    case HN:
		# HTML name target of the form ".hn <name>", ignore.
		next

	    # The following cases are for forms control.
	    case BP:
		call breakline (out, NJ)
		if (foflag == YES)
		    call outcc (out, FC_BREAKPAGE)
	    case TP:
		call breakline (out, NJ)
		if (foflag == YES) {
		    ctrlstr[1] = FC_TESTPAGE
		    ctrlstr[2] = lgetarg (Memc[ibuf], ip, DEF_TPNLINES)
		    ctrlstr[3] = EOS
		    call out (out_magic_arg, ctrlstr)
		}
	    case KS:
		call breakline (out, NJ)
		if (foflag == YES)
		    call outcc (out, FC_STARTKEEP)
	    case KE:
		call breakline (out, NJ)
		if (foflag == YES)
		    call outcc (out, FC_ENDKEEP)

	    case ENDHELP:			# normal exit point
		break

	    default:				# ordinary line of text
text_		if (Memc[ibuf] == '.') {
		    # Ignore unrecognized directives.
		    next
		} else {
		    # Determine if line is blank; skip a line if so, otherwise
		    # process a normal line of text.
		    for (ip=0;  Memc[ibuf+ip] == BLANK;  ip=ip+1)
			;
		    if (Memc[ibuf+ip] == EOS)
			call skiplines (out, 1)
		    else if (Memc[ibuf] == '\\')
			call textout (out, Memc[ibuf+1])
		    else
			call textout (out, Memc[ibuf])
		}
	    }

	    last_command = command
	
	} until (input (in, Memc[ibuf]) == EOF)


99	call breakline (out, NJ)
	call set_wordbuf (0)
	call set_outbuf (0)

	call sfree (sp)
end
