# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<chars.h>
include	<ctype.h>
include	"lroff.h"

# NEW_SECTION -- Begin a section heading.  Argument is the number of lines
# to skip.  Output section heading string given on next input line.  Reset left
# margin and cancel out any LS indents.

procedure new_section (in, out, linebuf, ip)

extern	in(), out()
char	linebuf[ARB]
int	ip

int	inbold(), in(), lgetarg()
errchk	skiplines, inbold, outline
include	"lroff.com"

begin
	sh_nskip = lgetarg (linebuf, ip, sh_nskip)
	call skiplines (out, sh_nskip)
	left_margin = perm_left_margin
	call testpage (out, DEF_TPNLINES)

	if (inbold (in, linebuf) != EOF)
	    call outline (out, linebuf)

	call init_ls()
end


# NEW_NUMBERED_SECTION -- Begin a numbered section heading.  Arguments are
# the number of lines to skip and the section level to be incremented (default
# is 1).  If only one arg is given, we assume it is the section level.
# Output section number followed by section heading string given on next input
# line.  Reset left margin and cancel out any LS indents.

procedure new_numbered_section (in, out, linebuf, ip)

extern	in(), out()
char	linebuf[ARB]
int	ip

int	i, n
int	inbold(), in(), lgetarg(), strlen()
errchk	skiplines, sprintf, pargi, outstr, outc, inbold, outline
include	"lroff.com"

begin
	# Get level, nskip arguments.
	n = max (1, min (MAX_NHLEVEL, lgetarg (linebuf, ip, 1)))
	nh_nskip = lgetarg (linebuf, ip, nh_nskip)

	call skiplines (out, nh_nskip)
	left_margin = perm_left_margin
	call testpage (out, DEF_TPNLINES)

	# Increment the desired section number; zero all higher numbered
	# section counters.

	nh_level[n] = nh_level[n] + 1
	call amovki (0, nh_level[n+1], MAX_NHLEVEL - n)

	# Output the section number followed by a blank and then the section
	# label.

	linebuf[1] = EOS
	do i = 1, n {
	    call sprintf (linebuf[strlen(linebuf)+1], SZ_IBUF, "%d.")
		call pargi (nh_level[i])
	}

	# Cancel the final "." if subsection heading.  Add a blank.
	if (n > 1 && linebuf[strlen(linebuf)] == '.')
	    linebuf[strlen(linebuf)] = EOS
	call outstr (out, linebuf)
	call outc (out, BLANK)

	# Get section label from next input line, write that out on the same
	# line in standout mode, then terminate the line.

	if (inbold (in, linebuf) != EOF)
	    call outline (out, linebuf)

	call init_ls()
end


# INIT_NH -- Initialize section numbering.

procedure init_nh()

include	"lroff.com"

begin
	call amovki (0, nh_level, MAX_NHLEVEL)
end


# NEW_INDENTED_SECTION -- Begin an indented section heading.  Optional
# arguments are the number of spaces to indent subsequent text and the number
# of lines to skip.  Output section heading string given on next input line.
# Reset left margin and cancel out any LS indents.

procedure new_indented_section (in, out, linebuf, ip)

extern	in(), out()
char	linebuf[ARB]
int	ip

int	inbold(), in(), lgetarg()
errchk	skiplines, inbold, outline
include	"lroff.com"

begin
	ih_indent = lgetarg (linebuf, ip, ih_indent)
	ih_nskip = lgetarg (linebuf, ip, ih_nskip)

	call skiplines (out, ih_nskip)
	left_margin = perm_left_margin
	call testpage (out, DEF_TPNLINES)

	# Read in and output the section heading in boldface.
	if (inbold (in, linebuf) != EOF)
	    call outline (out, linebuf)

	# Reset the left margin and cancel out any LS indents.
	left_margin = max (perm_left_margin, min (right_margin,
	    perm_left_margin + ih_indent))

	call init_ls()
end


# INBOLD -- Input a line in standout mode.  If the line is already all in
# upper case, do not use standout mode.  The input procedure processes
# all font escape sequences.  We must get the raw input line by calling the
# user input procedure, then pass it on to input() enclosed in \fB...\fR
# font escape sequences, to enable standout mode.

int procedure inbold (in, user_linebuf)

extern	in()
int	in()
char	user_linebuf[ARB]

pointer	sp, ip, lbuf, first
int	save_in_magic_arg, status
int	stropen(), input()
extern	getline()
errchk	salloc, stropen, input
include	"lroff.com"

begin
	call smark (sp)
	call salloc (lbuf, SZ_LINE + 3 + 3, TY_CHAR)

	# Deposit escape sequence to turn bold on.
	call strcpy ("\\fB", Memc[lbuf], ARB)

	# Read in the input line after the three char escape sequence.
	if (in (in_magic_arg, Memc[lbuf+3]) == EOF) {
	    call sfree (sp)
	    return (EOF)
	}

	# Scan the line to see if there are any lower case characters.
	# If all upper case, omit the mode control sequences (this procedure
	# becomes equivalent to input()).

	first = lbuf + 3
	for (ip=lbuf+3;  Memc[ip] != EOS;  ip=ip+1)
	    if (IS_LOWER (Memc[ip])) {
		first = lbuf
		break
	    }

	# Step on the newline if there is one, then add the \fR
	# sequence to turn bold off.
	if (first == lbuf) {
	    for (ip=lbuf;  Memc[ip] != EOS;  ip=ip+1)
		;
	    if (Memc[ip-1] == '\n')
		Memc[ip-1] = EOS
	    call strcat ("\\fR\n", Memc[lbuf], ARB)
	}

	# Now open the string as a file and call input to process it
	# into our caller's buffer.  We must save and restore the input
	# magic argument, set to the fd of the string file when input is
	# called.  This is a good example of the disadvantages of commons...

	save_in_magic_arg = in_magic_arg
	in_magic_arg = stropen (Memc[first], ARB, READ_ONLY)
	status = input (getline, user_linebuf)
	call close (in_magic_arg)
	in_magic_arg = save_in_magic_arg

	call sfree (sp)
	return (status)
end


# TESTPAGE -- If forms mode is enabled, output the control code for a test
# page followed by the number of lines to test for.  Test page tests if the
# specified number of lines are left on a page, and breaks the page if not.

procedure testpage (out, nlines)

extern	out()
int	nlines
char	ctrlstr[2]
include	"lroff.com"

begin
	if (foflag == YES) {
	    ctrlstr[1] = FC_TESTPAGE
	    ctrlstr[2] = nlines
	    ctrlstr[3] = EOS
	    call out (out_magic_arg, ctrlstr)
	}
end
