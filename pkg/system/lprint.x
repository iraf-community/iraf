# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<error.h>
include	<finfo.h>
include	<time.h>
include	<ttyset.h>

define	WIDE_PAGE	100
define	SZ_DDSTR	256


# LPRINT -- Print a file or files on the lineprinter.  A one line header is
# printed at the top of each page, unless we are reading from the standard
# input.  If printing multiple files, each file begins on a new page.
# TTYPUTLINE is used to output lines to the printer file, so that formfeeds,
# standout mode, etc., may be properly translated for the indicated device.

procedure t_lprint()

int	out, list, map_cc
bool	input_redirected, one_file
bool	print_heading, auto_header
pointer	sp, fname, device, label, lp, ddstr

pointer	ttyodes()
bool	streq(), clgetb()
int	clpopni(), clplen(), envgets(), lpopen(), clgfil(), ttygets(), btoi()
string	printer "printer"
errchk	clgfil, lp_print_file

begin
	call smark (sp)
	call salloc (fname,  SZ_FNAME, TY_CHAR)
	call salloc (label,  SZ_FNAME, TY_CHAR)
	call salloc (device, SZ_FNAME, TY_CHAR)
	call salloc (ddstr,  SZ_DDSTR, TY_CHAR)

	# Open list of files to be printed.
	list = clpopni ("files")
	one_file = (clplen (list) == 1)

	# Get device name.  Default is "printer", which means that the actual
	# device name is given by the environment variable "printer".

	call clgstr ("device", Memc[device], SZ_FNAME)
	if (streq (Memc[device], printer))
	    if (envgets (printer, Memc[device], SZ_FNAME) <= 0) {
		call clpcls (list)
		call syserrs (SYS_ENVNF, printer)
	    }

	# Map unknown control chars into printable sequences?
	map_cc = btoi (clgetb ("map_cc"))

	# If automatic pagination is selected, lprint will break pages and
	# print headers only if the standard input is not redirected.
	# Otherwise, pagination is disabled unless paginate=yes.  The file
	# name buffer is used as a temporary to hold the value string.

	call clgstr ("paginate", Memc[fname], SZ_FNAME)
	auto_header = streq (Memc[fname], "auto")
	if (!auto_header)
	    print_heading = streq (Memc[fname], "yes")

	# Open TTY descriptor and printer file.  We deal only with character
	# data, so open printer as a text file.  Output the files to the line
	# printer device.

	lp = ttyodes (Memc[device])
	if (ttygets (lp, "DD", Memc[ddstr], SZ_DDSTR) <= 0)
	    call error (1, "missing 'DD' parameter in termcap entry")

	out = lpopen (Memc[ddstr], APPEND, TEXT_FILE)

	while (clgfil (list, Memc[fname], SZ_FNAME) != EOF) {
	    # If only one file and it is the standard input (i.e., reading
	    # from a pipe), and if header printing is not explicitly enabled
	    # or disabled, do not break pages and print headers.

	    input_redirected = (one_file && streq (Memc[fname], "STDIN"))
	    if (auto_header)
		print_heading = !input_redirected

	    # Get page header label string.  File name is used unless reading
	    # from the standard input.

	    if (input_redirected)
		call clgstr ("label", Memc[label], SZ_FNAME)
	    else
		call strcpy (Memc[fname], Memc[label], SZ_FNAME)

	    # Output file, followed by a form feed.
	    iferr {
		call lp_print_file (out, lp, Memc[fname], Memc[label], map_cc,
		    print_heading)
		call flush (out)
	    } then
		call erract (EA_WARN)
	}

	call close (out)
	call clpcls (list)
	call ttycdes (lp)
	call sfree (sp)
end


# LP_PRINT_FILE -- Open and print the named text file on the output file,
# under control of the tty device descriptor LP.  Print a header on each
# page if enabled, and formfeed at the end of the file.  The number of lines
# per page is given by the tty descriptor.

procedure lp_print_file (out, lp, fname, label, map_cc, print_heading)

int	out
pointer	lp
char	fname[ARB]
char	label[ARB]
int	map_cc
bool	print_heading

bool	one_tab_in, streq()
int	pageno, lineno, maxlines, totlines, status, in
long	fi[LEN_FINFO], time, clktime()
pointer	sp, ip, lbuf, timebuf
int	open(), getline(), finfo(), ttystati()
errchk	salloc, finfo, cnvtime, open, ttystati, getline, ttyputline

begin
	call smark (sp)
	call salloc (lbuf, SZ_LINE+1, TY_CHAR)
	call salloc (timebuf, SZ_TIME, TY_CHAR)

	# Get time and date file was last modified.
	if (print_heading) {
	    if (streq (fname, "STDIN"))
		time = clktime (long(0))
	    else {
		if (finfo (fname, fi) == ERR) {
		    call sprintf (Memc[lbuf], SZ_LINE, "%s '%s'\n")
			call pargstr ("Cannot get info on file")
			call pargstr (fname)
		    call error (1, Memc[lbuf])
		} else
		    time = FI_MTIME(fi)
	    }
	    call cnvtime (time, Memc[timebuf], SZ_TIME)
	}

	in = open (fname, READ_ONLY, TEXT_FILE)
	maxlines = ttystati (lp, TTY_NLINES)

	# If printer page is very wide, set page in one tabstop from left
	# margin.

	one_tab_in = (ttystati (lp, TTY_NCOLS) > WIDE_PAGE)
	if (one_tab_in) {
	    Memc[lbuf] = '\t'
	    ip = lbuf + 1
	} else
	    ip = lbuf

	# If printing header and breaking pages, allow for the 3 line header
	# and a three line border at the bottom of the page.

	if (print_heading)
	    maxlines = maxlines - 6

	totlines = 0
	status = OK

	# Format and write each page of output.  If headers are enabled,
	# output formfeed and header between pages.

	for (pageno=1;  status != EOF;  pageno=pageno+1) {
	    # Print header, if enabled.  We assume that we are already
	    # positioned to the top of a page.

	    if (print_heading) {
		call sprintf (Memc[ip], SZ_LINE, "%s  %s  Page %d\n\n\n")
		    call pargstr (Memc[timebuf])
		    call pargstr (label)
		    call pargi (pageno)
		call ttyputline (out, lp, Memc[lbuf], map_cc)
		totlines = totlines + 3
	    }

	    # Output one page of text.  Each output line is processed by
	    # ttyputline to expand tabs, underline, etc.

	    for (lineno=1;  lineno <= maxlines;  lineno=lineno+1) {
		status = getline (in, Memc[ip])
		if (status == EOF)
		    break
		if (Memc[ip] == '\f') {
		    call ttyputline (out, lp, "\f", map_cc)
		    call strcpy (Memc[ip+1], Memc[ip], SZ_LINE)
		}
		call ttyputline (out, lp, Memc[lbuf], map_cc)
		totlines = totlines + 1
	    }

	    # Output formfeed, leaving printer positioned to top of page.
	    # Do not break pages if headers are disabled.

	    if (print_heading && totlines > 0)
		call ttyputline (out, lp, "\f", map_cc)
	}

	call close (in)
	call sfree (sp)
end
