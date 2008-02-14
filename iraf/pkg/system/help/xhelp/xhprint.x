# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<error.h>
include <ctype.h>
include	<ttyset.h>
include "../help.h"
include	"xhelp.h"


# XH_PRINT_HELP -- Print the requested help topic.

procedure xh_print_help (xh, topic, curpack, name)

pointer xh                                      #i task struct pointer
char    topic[ARB]                           	#i help topic
char    curpack[ARB]                           	#i current package
char    name[ARB]                           	#i printer name

int	fd
char	fname[SZ_FNAME]

int	open()
errchk	open

begin
	# Open a temp file with the help information.
	call mktemp ("tmp$xhelpi", fname, SZ_FNAME)
	fd = open (fname, NEW_FILE, TEXT_FILE)
	call xh_get_help (fd, topic, curpack, "", HF_PS, HELPDB(xh),
	    "all", OPTION(xh))
	call close (fd)

	call strcpy (name, PRINTER(xh), SZ_FNAME)
	call xh_lprint (fname, PRINTER(xh))

	# Clean up.
	call delete (fname)
end


# XH_LPRINT -- Print a file or files on the lineprinter.  TTYPUTLINE is used
# to output lines to the printer file, so that formfeeds, standout mode, etc.,
# may be properly translated for the indicated device.

procedure xh_lprint (fname, device)

char	fname[ARB]
char	device[ARB]

int	out
pointer	sp, lp, ddstr

pointer	ttyodes()
bool	streq()
int	envgets(), lpopen(), ttygets()
string	printer "printer"
errchk	clgfil, xh_print_file

begin
	call smark (sp)
	call salloc (ddstr,  SZ_DDSTR, TY_CHAR)

	# Get device name.  Default is "printer", which means that the actual
	# device name is given by the environment variable "printer".

	if (streq (device, printer))
	    if (envgets (printer, device, SZ_FNAME) <= 0)
		call syserrs (SYS_ENVNF, printer)

	# Open TTY descriptor and printer file.  We deal only with character
	# data, so open printer as a text file.  Output the files to the line
	# printer device.

	lp = ttyodes (device)
	if (ttygets (lp, "DD", Memc[ddstr], SZ_DDSTR) <= 0)
	    call error (1, "missing 'DD' parameter in termcap entry")

	out = lpopen (Memc[ddstr], APPEND, TEXT_FILE)

	# Output file, followed by a form feed.
	iferr {
	    call xh_print_file (out, lp, fname)
	    	call flush (out)
	} then
	    call erract (EA_WARN)

	call close (out)
	call ttycdes (lp)
	call sfree (sp)
end


# XH_PRINT_FILE -- Open and print the named text file on the output file,
# under control of the tty device descriptor LP.  Print a header on each
# page if enabled, and formfeed at the end of the file.  The number of lines
# per page is given by the tty descriptor.

procedure xh_print_file (out, lp, fname)

int	out
pointer	lp
char	fname[ARB]

bool	one_tab_in
int	lineno, maxlines, status, in
pointer	sp, ip, lbuf
int	open(), getline(), ttystati()
errchk	salloc, open, ttystati, getline, ttyputline

begin
	call smark (sp)
	call salloc (lbuf, SZ_LINE+1, TY_CHAR)

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


	# Format and write each page of output.  If headers are enabled,
	# output formfeed and header between pages.

	status = OK
	while (status != EOF) {
	    # Output one page of text.  Each output line is processed by
	    # ttyputline to expand tabs, underline, etc.

	    for (lineno=1;  lineno <= maxlines;  lineno=lineno+1) {
		status = getline (in, Memc[ip])
		if (status == EOF)
		    break
		if (Memc[ip] == '\f') {
		    call ttyputline (out, lp, "\f", YES)
		    call strcpy (Memc[ip+1], Memc[ip], SZ_LINE)
		}
		call ttyputline (out, lp, Memc[lbuf], YES)
	    }
	}

	call close (in)
	call sfree (sp)
end
