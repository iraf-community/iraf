# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ttyset.h>
include	<error.h>
include	<ctype.h>
include	<chars.h>
include	<mach.h>
include	<finfo.h>
include	<fset.h>

# PAGEFILES.X -- Page through a file or set of files.  Both backwards and
# forwards traversals of files and file lists are supported, but not
# (currently) backwards paging of a pipe.
#
# This program is a hack as it was coded starting from the original PAGE
# program, which was much simpler.  TODO: Add upscrolling and the ability
# to buffer input and scoll backwards on a pipe.  The present program is
# monolithic and should be restructured if these features are added.

define	CC_PREFIX	'^'
define	MAKE_PRINTABLE	($1+'A'-1)
define	SZ_QUERYMSG	80
define	SZ_KEYSTR	80
define	LNO_MAXLINES	2048
define	SZ_LONGLINE	4096
define	MAX_PAGE	100
define	MAX_PBCMD	100
define	UKEYS		"ukey"		# CL parameter for keyboard input

# Command keystrokes.

define	HELPTXT "[q=quit,e=edit,d=dn,u=up,f|sp=fpg,b=bpg,j|cr=dnln,k=upln,.=bof,N=nfile,P=pfile]"

define	HELP		'?'		# print helptxt
define	QUIT		'q'		# return to CL
define	EDIT		'e'		# edit current file
define	FWD_SCREEN	'f'		# forward one full screen
define	BACK_SCREEN	'b'		# back one full screen
define	SCROLL_DOWN	'd'		# forward half a screen
define	SCROLL_UP	'u'		# back half a screen
define	PREV_LINE	'k'		# back one line
define	NEXT_LINE	'j'		# forward one line
define	TO_BOF		'.'		# to beginning of file
define	TO_EOF		'G'		# to end of file
define	TO_EOF_ALT	'g'		# to end of file
define	SEARCH		'n'		# search for next occurrence of pattern
define	REDRAW		'\014'		# redraw screen

define	NEXT_FILE	'N'		# goto next file in list
define	PREV_FILE	'P'		# goto previous file in list
define	NEXT_FILE_ALT	'\016'		# <ctrl/n>
define	PREV_FILE_ALT	'\020'		# <ctrl/p>

define	LCMD		':'		# colon commands
define	TO_FILE		'F'		# ":file filename"


# PAGEFILES -- Display a text file or files on the standard output (the user
# terminal) one screen at a time, pausing after each screen has been filled.
# The program is keystroke driven in raw mode, and currently recognizes the
# keystrokes defined above.
# 
# If map_cc is enabled, all unknown control characters will be converted into
# printable sequences.  The following control character sequences have a
# special significance in IRAF textfiles: FF=formfeed, SO=set standout mode,
# SI=clear standout mode.  These sequences are mapped into whatever the output
# device requires upon output by the TTY subroutines.

procedure pagefiles (files)

char	files[ARB]		# file template

string	device "terminal"
string	prompt ""
int	first_page
int	clear_screen
int	map_cc

begin
	first_page = 1
	clear_screen = YES
	map_cc = YES

	call xpagefiles (files, device,
	    prompt, first_page, clear_screen, map_cc)
end


# PAGEFILE -- Page a single file; an alternate entry point to the more general
# routine.  A prompt string different than the filename may be specified and
# the screen is not cleared when scrolling downward.

procedure pagefile (fname, prompt)

char	fname[ARB]		# name of file to be paged
char	prompt[ARB]		# prompt string, if different than fname

string	device "terminal"
int	first_page
int	clear_screen
int	map_cc

begin
	first_page = 1
	clear_screen = NO
	map_cc = YES

	call xpagefiles (fname, device,
	    prompt, first_page, clear_screen, map_cc)
end


# XPAGEFILES -- Generalized file pager.

procedure xpagefiles (files, device, prompt, first_page, clear_screen, map_cc)

char	files[ARB]		# file template
char	device[ARB]		# output device name
char	prompt[ARB]		# prompt string (filename if null)
int	first_page		# first page to be displayed
int	clear_screen		# clear screen between pages
int	map_cc			# map control chars on output

bool	redirin, useroot
pointer	sp, fname, newfname, tty, lbuf
int	spoolfd, list, nfiles, cmd, i, j, n, o

pointer	ttyodes()
bool	ttygetb()
int	strncmp(), strlen()
int	fntopnb(), fntrfnb(), fntlenb(), fnldir()
int	pg_getcmd(), pg_pagefile(), fstati()
errchk	fntopnb, ttyodes, ttygetb, fntrfnb, pg_pagefile, pg_getcmd
define	err_ 91

begin
	call smark (sp)
	call salloc (newfname, SZ_FNAME, TY_CHAR)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (lbuf, SZ_LINE, TY_CHAR)

	list = fntopnb (files, YES)
	nfiles = fntlenb (list)
	spoolfd = NULL

	tty = ttyodes (device)
	redirin = (fstati (STDIN, F_REDIR) == YES)

	# If terminal cannot scroll, set clear_screen to true regardless of the
	# value given above.

	if (ttygetb (tty, "ns"))
	    clear_screen = YES

	cmd = NEXT_FILE
	for (i=1;  i <= nfiles && cmd != QUIT;  i=i+1) {
	    # Get next filename.
	    if (fntrfnb (list, i, Memc[fname], SZ_FNAME) == EOF)
		break

	    # Page the file.
	    cmd = pg_pagefile (tty, Memc[fname], Memc[newfname], prompt,
		clear_screen, first_page, map_cc, i, nfiles, redirin, spoolfd)

	    # Decide what to do next.
	    while (cmd != QUIT) {
		switch (cmd) {
		case NEXT_FILE, BLANK, NEXT_LINE, CR, LF:
		    if (i >= nfiles)
			cmd = pg_getcmd (tty, "no more files", 0,0,0,i,nfiles)
		    else
			break
		case PREV_FILE:
		    if (i <= 1)
			cmd = pg_getcmd (tty, "at first file", 0,0,0,i,nfiles)
		    else {
			i = i - 2
			break
		    }

		case TO_FILE:
		    # Position within the file list.  If the user specified a
		    # logical directory in the filename, perform the compares
		    # on the raw filenames in the list, otherwise use only the
		    # root filename.

		    useroot = (fnldir(Memc[newfname],Memc[fname],SZ_FNAME) <= 0)
		    n = strlen (Memc[newfname])

		    for (j=1;  j <= nfiles;  j=j+1)
			if (fntrfnb (list, j, Memc[lbuf], SZ_FNAME) > 0) {
			    if (useroot)
				o = fnldir (Memc[lbuf], Memc[fname], SZ_FNAME)
			    else
				o = 0
			    if (strncmp (Memc[lbuf+o], Memc[newfname], n) >= 0)
				break
			}

		    if (j > nfiles)
			i = nfiles - 1
		    else
			i = j - 1
		    break

		case LCMD:
		    # Colon escape.  Only :file is recognized at this level.

		    call pg_getstr (Memc[newfname], SZ_FNAME)
		    cmd = TO_FILE

		case HELP:
		    cmd = pg_getcmd (tty, HELPTXT, 0,0,0,0,0)

		default:
err_		    if (!redirin) {
		        call eprintf ("\07")
		        call flush (STDERR)
			i = i - 1		# redisplay current file
		    }
		    break
		}
	    }
	}

	if (spoolfd != NULL)
	    call close (spoolfd)

	call fntclsb (list)
	call sfree (sp)
end


# PG_PAGEFILE -- Display the named file on the standard output, page by
# page, pausing for user response between pages.

int procedure pg_pagefile (tty, fname, newfname, u_prompt, clear_screen,
	first_page, map_cc, fileno, nfiles, redirin, spoolfd)

pointer	tty
char	fname[ARB]		# file to be paged
char	newfname[ARB]		# next file to be page (ret. by :file)
char	u_prompt[ARB]		# prompt string, if not same as filename
int	clear_screen		# clear screen between pages?
int	first_page		# first page of file to be displayed
int	map_cc			# map control characters?
int	fileno			# current file number
int	nfiles			# number of files to be paged
bool	redirin			# reading from the standard input
int	spoolfd			# fd if spooling output in a file

char	patbuf[SZ_LINE]
int	nlines, ncols, maxlines, maxcols
long	fi[LEN_FINFO], nchars, totchars, loffset
pointer	sp, lbuf, prompt, token, cmdbuf, ip, op, lp
long	pgoff[MAX_PAGE], pgnch[MAX_PAGE], pglno[MAX_PAGE]
int	fd, lineno, linelen, nleft, destline, toklen, lnout, i
bool	ateof, first_call, redirout, pushback, upline, upline_ok
int	o_loffset, o_nchars, o_lineno, o_pageno, junk, pageno, cmd, ch, n

long	note()
pointer	lno_open()
bool	streq(), ttygetb()
int	pg_getcmd(), ctoi(), strncmp(), patmake(), patmatch(), pg_peekcmd()
int	open(), finfo(), strlen(), pg_getline(), getci()
int	lno_fetch(), fstati(), ttyctrl()
data	first_call /true/

define	err_ 91
define	quit_ 92
define	search_ 93
define	destline_ 94

begin
	call smark (sp)
	call salloc (lbuf, SZ_LONGLINE, TY_CHAR)
	call salloc (cmdbuf, SZ_LINE, TY_CHAR)
	call salloc (prompt, SZ_FNAME, TY_CHAR)
	call salloc (token, SZ_FNAME, TY_CHAR)

	if (first_call) {
	    # The pattern buffer is retained indefinitely.
	    patbuf[1] = EOS
	    spoolfd = NULL
	    first_call = false
	}

	call pg_setprompt (Memc[prompt], u_prompt, fname)
	call xttysize (ncols, nlines)
	maxlines = nlines - 1
	maxcols  = ncols

	redirout = (fstati (STDOUT, F_REDIR) == YES)
	upline_ok = (!redirout && ttygetb(tty,"cm") && ttygetb(tty,"al"))
	call pg_pushcmd (NULL)

	# Get file size for (xx%) info in nomore.  If reading from the
	# standard input, file size is not known.

	nchars = 0
	if (streq (fname, "STDIN")) {
	    totchars = -1
	} else if (finfo (fname, fi) == ERR) {
	    call sprintf (Memc[lbuf], SZ_LINE, "Cannot access file `%s'")
		call pargstr (fname)
	    cmd = pg_getcmd (tty, Memc[lbuf], 0, 0, 0, fileno, nfiles)
	    call sfree (sp)
	    return (cmd)
	} else
	    totchars = FI_SIZE(fi)

	# If file is empty, return immediately without clearing screen.
	if (totchars == 0) {
	    call sprintf (Memc[lbuf], SZ_LINE, "Null length file `%s'")
		call pargstr (fname)
	    cmd = pg_getcmd (tty, Memc[lbuf], 0, 0, 0, fileno, nfiles)
	    call sfree (sp)
	    return (cmd)
	}

	# Open the file.
	iferr (fd = open (fname, READ_ONLY, TEXT_FILE)) {
	    call sprintf (Memc[lbuf], SZ_LINE, "Cannot open file `%s'")
		call pargstr (fname)
	    cmd = pg_getcmd (tty, Memc[lbuf], 0, 0, 0, fileno, nfiles)
	    call sfree (sp)
	    return (cmd)
	}

	# Open the line offset save/fetch database.
	lp = lno_open (LNO_MAXLINES)

	# Advance to the first page of the file to be displayed.  Pages are
	# marked by FF chararacters in the text.  If the first page is number
	# one, do nothing.  If the first character in the file is FF, do not
	# count it.  This is necessary to count pages correctly whether or not
	# the first page is preceeded by a FF.

	pageno = 1
	lineno = 1
	pgoff[1] = BOF
	pgnch[1] = nchars
	pglno[1] = lineno

	if (first_page > 1) {
	    junk = getci (fd, ch)
	    nchars = nchars + 1

	    while (pageno < first_page) {
		while (getci (fd, ch) != '\f') {
		    nchars = nchars + 1
		    if (ch == '\n')
			lineno = lineno + 1
		    if (ch == EOF) {
			call close (fd)
			call lno_close (lp)
			call sfree (sp)
			return
		    }
		}
		pageno = pageno + 1
		nchars = nchars + 1
		pgoff[pageno] = note (fd)
		pgnch[pageno] = nchars
		pglno[pageno] = lineno
	    }
	}

	# Always clear the screen between files; the "clear_screen" param
	# applies only to the pages of a single file.

	if (!redirout) {
	    call ttyclear (STDERR, tty)
	    call flush (STDERR)
	}

	# Output lines, mapping control characters if enabled.  Pause at the
	# end of every screen, or when FF is encountered in the text.

	pushback= false
	ateof	= false
	nleft	= maxlines	# nlines left to display before prompt
	lnout   = 0

	repeat {
	    # Fetch and display the next line of the file.

	    if (pushback)
		pushback = false
	    else
		loffset = note (fd)

	    if (pg_getline (fd, Memc[lbuf]) == EOF) {
		if ((nfiles==1 && lineno <= maxlines) || redirin || redirout) {
		    # Simply quit if a single small file or the standard input
		    # is being paged.

		    call close (fd)
		    call lno_close (lp)
		    call sfree (sp)
		    return (QUIT)

		} else {
		    nchars = totchars + SZ_LINE
		    ateof = true
		    nleft = 0
		}

	    } else if (Memc[lbuf] == '\f') {
		# Formfeed encountered; pause for the prompt and print the
		# remainder of the line on the next screen.  If we have not
		# yet written anything on the screen (nleft=maxlines) don't
		# bother to prompt again.

		pageno = pageno + 1
		pgoff[pageno] = loffset
		pglno[pageno] = lineno
		pgnch[pageno] = nchars
		call ungetline (fd, Memc[lbuf+1])
		pushback = true

		if (nleft == maxlines)
		    next
		else
		    nleft = 0

	    } else {
		# Output line, processing all escapes as req'd by the device.
		# Keep track of position in file for %done message in prompt,
		# and of position on screen so that we know when to prompt.

		call lno_save (lp, lineno, loffset, nchars)
		linelen = strlen (Memc[lbuf])
		nchars  = nchars + linelen
		lineno  = lineno + 1

		# Count the number of printed columns in the output text.
		n = 1
		do i = 1, linelen
		    if (ch >= ' ')
			n = n + 1
		    else if (ch == '\t') {
			n = n + 1
			while (mod (n-1, 8) != 0)
			    n = n + 1
		    }

		# Decrement lines left on screen.
		nleft = nleft - max (1, ((n + maxcols-1) / maxcols))

		if (spoolfd != NULL)
		    call putline (spoolfd, Memc[lbuf])

		# Cancel upline if line is too long.
		if (upline && lnout <= 0 && linelen >= maxcols) {
		    call ttyclear (STDERR, tty)
		    call flush (STDERR)
		    upline = false
		    lnout = 0
		}

		if (!(upline && lnout > 0))
		    call ttyputline (STDOUT, tty, Memc[lbuf], map_cc)
		lnout = min (maxlines, lnout + 1)
	    }

	    if (nleft <= 0) {
		# Move cursor to query line at end of line insert sequence.
		if (upline) {
		    # Don't bother if the next command is another insert.
		    if (pg_peekcmd() != PREV_LINE) {
			call ttygoto (STDOUT, tty, 1, lnout + 1)
			call ttyclearln (STDOUT, tty)
		    }
		    upline = false
		}

		# Pause and get next keystroke from the user.
		cmd = pg_getcmd (tty, Memc[prompt], nchars, totchars, lineno,
		    fileno, nfiles)

		# Allow use of the space bar to advance to the next file,
		# when at the end of the current file.

		if (ateof && nfiles > 1 && (cmd == BLANK || cmd == FWD_SCREEN))
		    cmd = NEXT_FILE

		repeat {
		    switch (cmd) {
		    case NEXT_FILE:
			# This really means the next file if multiple files.
			if (nfiles > 1)
			    goto quit_
			else if (pushback) {
			    cmd = FWD_SCREEN
			    next
			}

			# Otherwise we want the next page (formfeed).
			o_loffset = note (fd)
			o_nchars  = nchars
			o_lineno  = lineno

			repeat {
			    loffset = note (fd)
			    n = pg_getline (fd, Memc[lbuf])
			    if (n == EOF) {
				if (!redirin) {
				    call seek (fd, o_loffset)
				    pushback = false
				    nchars = o_nchars
				    lineno = o_lineno
				    ateof  = false
				}
				cmd = pg_getcmd (tty, "No more pages",
				    nchars,totchars, lineno, fileno,nfiles)
				Memc[lbuf] = EOS
				break
			    }

			    call lno_save (lp, lineno, loffset, nchars)

			    if (Memc[lbuf] == '\f') {
				pageno = min (MAX_PAGE, pageno + 1)
				pgoff[pageno] = loffset
				pgnch[pageno] = nchars
				pglno[pageno] = lineno
				if (!redirout) {
				    call ttyclear (STDERR, tty)
				    call flush (STDERR)
				    lnout = 0
				}
				call ungetline (fd, Memc[lbuf+1])
				pushback = true
				nleft = maxlines
				break
			    }

			    nchars = nchars + n
			    lineno = lineno + 1
			}

			if (n == EOF)
			    next
			else
			    break

		    case PREV_FILE:
			# If there are multiple files go to previous file,
			# otherwise, go to previous page (formfeed).

			if (nfiles > 1)
			    goto quit_
			if (redirin)
			    goto err_

			# Special case - just reached beginning of next
			# page, but still displaying previous page.

			if (pglno[pageno] == lineno)
			    pageno = max (1, pageno - 1)

			# If the beginning of the current page is not on
			# the screen, go back to the beginning of the page.

			if (lineno <= pglno[pageno]+maxlines)
			    pageno = max (1, pageno - 1)

			# Go there.
			call seek (fd, pgoff[pageno])
			nchars = pgnch[pageno]
			lineno = pglno[pageno]
			pushback = false

			if (!redirout) {
			    call ttyclear (STDERR, tty)
			    call flush (STDERR)
			    lnout = 0
			}
			if (getci (fd, ch) != '\f')
			    call ungetci (fd, ch)
			nleft = maxlines
			break

		    case QUIT:
quit_			call close (fd)
			call lno_close (lp)
			call sfree (sp)
			return (cmd)

		    case TO_BOF:
			if (redirin)
			    goto err_

			call pg_setprompt (Memc[prompt], u_prompt, fname)
			call seek (fd, BOFL)
			pushback = false
			Memc[lbuf] = EOS
			ateof  = false
			lineno = 1
			nchars = 0
			nleft  = maxlines
			pageno = 1

			if (!redirout) {
			    call ttyclear (STDERR, tty)
			    call flush (STDERR)
			    lnout = 0
			}
			break

		    case FWD_SCREEN, BLANK:
			if (!ateof && clear_screen == YES && !redirout) {
			    call ttyclear (STDERR, tty)
			    call flush (STDERR)
			    lnout = 0
			}
			nleft = maxlines
			break

		    case TO_EOF, TO_EOF_ALT:
			destline = MAX_INT
			goto destline_
		    case SCROLL_DOWN:
			nleft = (maxlines + 1) / 2
			break
		    case SCROLL_UP:
			if (redirin)
			    goto err_
			if (upline_ok) {
			    destline = lineno - 2
			    do i = 1, ((maxlines + 1) / 2 - 1)
				if (lineno - lnout - i > 1)
				    call pg_pushcmd (PREV_LINE)
			} else
			    destline = lineno - ((maxlines + 1) / 2) - 1
			goto destline_
		    case BACK_SCREEN:
			if (redirin)
			    goto err_
			destline = lineno - maxlines - 1
			goto destline_
		    case PREV_LINE:
			if (redirin)
			    goto err_
			destline = lineno - 2
			goto destline_
		    case REDRAW:
			if (redirin)
			    goto err_
			destline = lineno
			goto destline_
		    case NEXT_LINE, CR, LF:
			nleft = 1
			break
		    case SEARCH:
			# Stop at next line containing current pattern.
			goto search_

		    case HELP:
			cmd = pg_getcmd (tty, HELPTXT, 0, 0, 0, 0, 0)
			# get another command

		    case EDIT:
			# Edit the file being paged.
			if (redirin)
			    goto err_

			# Close file and LNO database.
			call close (fd)
			call lno_close (lp)
			call flush (STDOUT)
			call flush (STDERR)

			# Command the CL to edit the file.
			call sprintf (Memc[lbuf], SZ_LINE, "edit (\"%s\")")
			    call pargstr (fname)
			iferr (call clcmdw (Memc[lbuf]))
			    call erract (EA_WARN)

			# Reopen the file and LNO database.
			iferr (fd = open (fname, READ_ONLY, TEXT_FILE)) {
			    call sfree (sp)
			    return (NEXT_FILE)
			} else
			    lp = lno_open (LNO_MAXLINES)

			# Redisplay the file at the BOF.
			if (!redirout) {
			    call ttyclear (STDERR, tty)
			    call flush (STDERR)
			    lnout = 0
			}
			Memc[lbuf] = EOS
			nchars = 0
			lineno = 1
			nleft  = maxlines
			break

		    case LCMD:
			# Colon escape.
			call pg_getstr (Memc[cmdbuf], SZ_LINE)
			for (ip=cmdbuf;  IS_WHITE (Memc[ip]);  ip=ip+1)
			    ;

			if (Memc[ip] == '!') {
			    # Send a command to the CL.

			    iferr (call clcmdw (Memc[cmdbuf+1]))
				call erract (EA_WARN)
			    cmd = pg_getcmd (tty, Memc[prompt],
				nchars, totchars, lineno, fileno, nfiles)
			    Memc[lbuf] = EOS
			    next

			} else if (Memc[ip] == '/') {
			    # Search for a line containing the given pattern.

			    if (patmake (Memc[ip+1], patbuf, SZ_LINE) == ERR)
				goto err_
search_
			    if (patbuf[1] == EOS) {
				cmd = pg_getcmd (tty, "No current pattern",
				    0, 0, 0, fileno, nfiles)
				Memc[lbuf] = EOS
				next
			    }

			    o_loffset = note (fd)
			    o_nchars  = nchars
			    o_lineno  = lineno
			    o_pageno  = pageno

			    repeat {
				loffset = note (fd)
				n = pg_getline (fd, Memc[lbuf])
				if (n == EOF) {
				    if (!redirin) {
					call seek (fd, o_loffset)
					pushback = false
					nchars = o_nchars
					lineno = o_lineno
					pageno = o_pageno
					ateof  = false
				    }
				    cmd = pg_getcmd (tty, "Pattern not found",
					nchars,totchars,lineno,fileno,nfiles)
				    Memc[lbuf] = EOS
				    break
				}

				call lno_save (lp, lineno, loffset, nchars)
				if (Memc[lbuf] == '\f') {
				    pageno = pageno + 1
				    pgoff[pageno] = loffset
				    pgnch[pageno] = nchars
				    pglno[pageno] = lineno
				}

				if (patmatch (Memc[lbuf], patbuf) > 0) {
				    if (redirin) {
					call ungetline (fd, Memc[lbuf])
					pushback = true
					nleft = maxlines
					break
				    } else {
					destline = lineno
					nchars = nchars + n
					lineno = lineno + 1
					goto destline_
				    }
				}

				nchars = nchars + n
				lineno = lineno + 1
			    }

			    if (n == EOF)
				next
			    else
				break
			}

			# Case ":cmd arg".
			for (op=token;  IS_ALPHA (Memc[ip]);  ip=ip+1) {
			    Memc[op] = Memc[ip]
			    op = op + 1
			}
			for (;  IS_WHITE (Memc[ip]);  ip=ip+1)
			    ;
			Memc[op] = EOS
			toklen = op - token

			# Print help if no : string given.
			if (toklen <= 0) {
			    call strcpy ("help", Memc[token], SZ_FNAME)
			    toklen = 4
			}

			if (strncmp (Memc[token], "line", toklen) == 0) {
			    # Move to the destination line, expressed as
			    # ":line N" for an absolute line, or ":line +/-N"
			    # for a relative move.

			    destline = lineno
			    if (Memc[ip] == '+') {
				ip = ip + 1
				if (ctoi (Memc, ip, n) > 0)
				    destline = lineno + n
			    } else if (Memc[ip] == '-') {
				ip = ip + 1
				if (ctoi (Memc, ip, n) > 0)
				    destline = lineno - n
			    } else if (ctoi (Memc, ip, n) > 0)
				destline = n
destline_
			    # Upscroll one line?
			    if (upline_ok && destline == lineno-2)
				upline = true

			    # Determine line at top of new screen.
			    nleft = maxlines
			    if (destline < lineno && destline >= lineno-lnout-1)
				destline = destline - lnout + 1
			    else
				destline = destline - nleft + 1

			    # Don't upscroll off the top of the screen.
			    if (destline < 1) {
				destline = 1
				upline = false
			    }

			    # Look up the desired line offset in the database
			    # and go directly there if found, otherwise either
			    # advance forward or rewind the file and advance
			    # forward to the indicated line.

			    if (lno_fetch(lp,destline,loffset,nchars)==ERR) {
				if (!redirin && destline < lineno) {
				    call seek (fd, BOFL)
				    pushback = false
				    lineno = 1
				    pageno = 1
				    nchars = 0
				    ateof  = false
				    call pg_setprompt (Memc[prompt],
					u_prompt, fname)
				}

				while (lineno < destline) {
				    loffset = note (fd)
				    n = pg_getline (fd, Memc[lbuf])
				    if (n == EOF) {
					destline = lineno - 1	# goto EOF
					goto destline_
				    }
				    call lno_save (lp, lineno, loffset, nchars)
				    if (Memc[lbuf] == '\f') {
					pageno = pageno + 1
					pgoff[pageno] = loffset
					pgnch[pageno] = nchars
					pglno[pageno] = lineno
				    }
				    nchars = nchars + n
				    lineno = lineno + 1
				}
			    } else if (!redirin) {
				call seek (fd, loffset)
				pushback = false
				lineno = destline
				ateof  = false

				# Determine which page we are in.
				do i = 2, MAX_PAGE
				    if (pglno[i] <= 0) {
					pageno = i - 1
					break
				    } else if (pglno[i] >= lineno) {
					pageno = i
					break
				    }

				call pg_setprompt (Memc[prompt],u_prompt,fname)
			    }

			    # Prepare to draw the screen.  Upline mode means
			    # we want to insert a line at the top of the screen
			    # and then skip to the page prompt; otherwise we
			    # clear the screen and output a full page of text.

			    if (!redirout) {
				if (upline) {
				    # Clear screen if backing up over a page.
				    if (destline+1 == pglno[pageno])
					call ttyclear (STDERR, tty)
				    call ttygoto (STDOUT, tty, 1, 1)
				    junk = ttyctrl (STDOUT, tty, "al", maxlines)
				    lnout = 0
				} else {
				    call ttyclear (STDERR, tty)
				    upline = false
				    lnout = 0
				}
				call flush (STDERR)
			    }
			    Memc[lbuf] = EOS
			    break

			} else if (strncmp (Memc[token], "file", toklen) == 0) {
			    # Position to the named file (must be in file list).

			    call strcpy (Memc[ip], newfname, SZ_FNAME)
			    call close (fd)
			    call lno_close (lp)
			    call sfree (sp)
			    return (TO_FILE)

			} else if (strncmp (Memc[token],"spool",toklen) == 0) {
			    # Begin spooling output in a file.

			    if (spoolfd != NULL) {
				call close (spoolfd)
				spoolfd = NULL
			    }

			    if (Memc[ip] == EOS) {
				;
			    } else iferr {
				spoolfd = open (Memc[ip], APPEND, TEXT_FILE)
			    } then {
				spoolfd = NULL
				call erract (EA_WARN)
			    }

			    # Get next keystroke from the user.
			    cmd = pg_getcmd (tty, Memc[prompt],
				nchars, totchars, lineno, fileno, nfiles)

			} else {
			    cmd = pg_getcmd (tty,
			    "colon cmds: :!cmd :/pat :line L :file F :spool F",
				0, 0, 0, 0, 0)
			}

		    default:
err_			call eprintf ("\07")
			call flush (STDERR)
			cmd = pg_getcmd (tty, Memc[prompt], nchars, totchars,
			    lineno, fileno, nfiles)
		    }
		}
	    }
	}
end


# PG_SETPROMPT -- Set the prompt string for the ukey end-of-page query.
# The name of the file currently being paged is used unless a prompt string
# is given.

procedure pg_setprompt (prompt, u_prompt, fname)

char	prompt[SZ_FNAME]		# receives prompt string
char	u_prompt[ARB]			# user prompt string
char	fname[ARB]			# file being paged

int	gstrcpy()

begin
	if (gstrcpy (u_prompt, prompt, SZ_FNAME) <= 0)
	    call strcpy (fname, prompt, SZ_FNAME)
end


# PG_GETLINE -- Get a line from the input file.  Accumulates very long lines
# (requiring several getline calls to read) into a single string.

int procedure pg_getline (fd, lbuf)

int	fd			# input file
char	lbuf[SZ_LONGLINE]	# output buffer

int	nchars, op
int	getline()
errchk	getline

begin
	for (op=1;  op + SZ_LINE < SZ_LONGLINE;  op=op+nchars) {
	    nchars = getline (fd, lbuf[op])
	    if (nchars == EOF) {
		if (op == 1)
		    return (EOF)
		else
		    return (op - 1)
	    } else if (lbuf[op+nchars-1] == '\n')
		break
	}

	return (op + nchars - 1)
end


# PG_GETCMD -- Query the user for a single character command keystroke.
# A prompt naming the current file and our position in it is printed,
# we read the single character command keystroke in raw mode, and then
# the prompt line is cleared and we return.

int procedure pg_getcmd (tty, fname, nchars, totchars, lineno, fileno, nfiles)

pointer	tty			# tty descriptor
char	fname[ARB]		# prefix string
long	nchars			# position in file
long	totchars		# size of file
int	lineno			# current line number
int	fileno			# current file number
int	nfiles			# nfiles being paged through

char	keystr[SZ_KEYSTR]
int	key, pb, pbcmd[MAX_PBCMD]
common	/pgucom/ key, pb, pbcmd, keystr
int	clgkey(), fstati()

begin
	# If any commands have been pushed, return the next pushed command
	# without generating a query.

	if (pb > 0) {
	    key = pbcmd[pb]
	    pb = pb - 1
	    return (key)
	}

	# If the standard output is redirected, skip the query and just go on
	# to the next page.

	if (fstati (STDOUT, F_REDIR) == YES)
	    return (FWD_SCREEN)

	# Ensure synchronization with the standard output.
	call flush (STDOUT)

	# Print query in standout mode, preceded by %done info.
	call ttyso (STDERR, tty, YES)
	call eprintf ("%s")
	    call pargstr (fname)
	if (totchars > 0) {
	    if (nchars >= totchars + SZ_LINE)
		call eprintf ("-(EOF)")
	    else {
		call eprintf ("-(%02d%%)")
		    call pargi (max(0, min(99, nchars * 100 / totchars)))
	    }
	}
	if (lineno > 0) {
	    call eprintf ("-line %d")
		call pargi (lineno - 1)
	}
	if (fileno > 0 && nfiles > 0) {
	    call eprintf ("-file %d of %d")
		call pargi (fileno)
		call pargi (nfiles)
	}
	call ttyso (STDERR, tty, NO)
	call flush (STDERR)

	call fseti (STDIN, F_SETREDRAW, REDRAW)

	# Read the user's response, normally a single keystroke.
	if (clgkey (UKEYS, key, keystr, SZ_KEYSTR) == EOF)
	    key = INTCHAR

	call fseti (STDIN, F_SETREDRAW, 0)

	if (key == INTCHAR)
	    key = QUIT
	else if (key == NEXT_FILE_ALT)
	    key = NEXT_FILE
	else if (key == PREV_FILE_ALT)
	    key = PREV_FILE

	# Erase the prompt and return.
	call eprintf ("\r")
	call ttyclearln (STDERR, tty)
	call flush (STDERR)

	return (key)
end


# PG_GETSTR -- Called after receipt of a : key to get the string value.

procedure pg_getstr (strval, maxch)

char	strval[maxch]		# receives string
int	maxch

char	keystr[SZ_KEYSTR]
int	key, pb, pbcmd[MAX_PBCMD]
common	/pgucom/ key, pb, pbcmd, keystr

begin
	call strcpy (keystr, strval, maxch)
end


# PG_PUSHCMD -- Push back a command keystroke.

procedure pg_pushcmd (cmd)

int	cmd			#I command to be pushed

char	keystr[SZ_KEYSTR]
int	key, pb, pbcmd[MAX_PBCMD]
common	/pgucom/ key, pb, pbcmd, keystr

begin
	if (cmd <= 0)
	    pb = 0
	else {
	    pb = min (MAX_PBCMD, pb + 1)
	    pbcmd[pb] = cmd
	}
end


# PG_PEEKCMD -- Peek at any pushed back command keystroke.

int procedure pg_peekcmd()

char	keystr[SZ_KEYSTR]
int	key, pb, pbcmd[MAX_PBCMD]
common	/pgucom/ key, pb, pbcmd, keystr

begin
	if (pb <= 0)
	    return (ERR)
	else
	    return (pbcmd[pb])
end
