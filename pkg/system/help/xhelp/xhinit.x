# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"xhelp.h"


# XH_INIT -- Initialize the task and the GUI.

procedure xh_init (xh, file_template, search)

pointer	xh				#i task struct pointer
int	file_template			#i is topic a file template?
int	search				#i doing a search?

char	curdir[SZ_FNAME]
int	fd

pointer	strestore()
int	envgets(), open()
bool	streq()
errchk	open

begin
	# Update the quickref file.
	call xh_updt_quickref (xh)

	# If starting up with a search, get that information first.
 	if (search == YES && TEMPLATE(xh) != EOS)  {
	    call xh_search (xh, NO, TEMPLATE(xh))
	    call strcpy ("", TEMPLATE(xh), SZ_FNAME)
	    call strcpy ("", LIST(xh), SZ_LINE)
	} 

	# Create the root package and send the results to the GUI.
	call xh_root_pkg (xh)
	call gmsg (XH_GP(xh), "pkglist", LIST(xh))

	# Initialize the package list symtab.
	iferr (fd = open (PKGFILE, READ_ONLY, BINARY_FILE)) 
	    call error (0, "Cannot open package list symtab")
	XH_STP(xh) = strestore (fd)
	call close (fd)
	call gmsgi (XH_GP(xh), "showtype", XH_SHOWTYPE(xh))


	if (TEMPLATE(xh) != EOS && file_template == NO) {
	    # If we're given an initial help topic, get the page and load it.
	    #call xh_help (xh, TEMPLATE(xh), TEMPLATE(xh), OPTION(xh))
	    call xh_cmd_help (xh, TEMPLATE(xh), "{}", OPTION(xh))
	    call strcpy ("", TEMPLATE(xh), SZ_FNAME)

	} else if (TEMPLATE(xh) != EOS && file_template == YES) {
	    # Load a user defined page.
	    call xh_open_file (xh, "helpres", TEMPLATE(xh), YES, YES)
	    call strcpy ("", TEMPLATE(xh), SZ_FNAME)

	} else {
	    # Load either a user defined homepage or the task help.
	    call clgstr ("home", HOMEPAGE(xh), SZ_FNAME)
	    if (streq ("", HOMEPAGE(xh)))
	        call strcpy (HELP, HOMEPAGE(xh), SZ_FNAME)
	    call xh_open_file (xh, "helpres", HOMEPAGE(xh), NO, YES)
	}

	# Set the printer to be used.
	call gmsg (XH_GP(xh), "printer", PRINTER(xh))

	# Initialize the online help doc.
	call xh_open_file (xh, "help", HELP, NO, YES)

	# Initialize the file browsing parameters.  Since we can't
	# get the current directory for the session use home$.
	if (envgets ("home", curdir, SZ_FNAME) != EOF) {
	    call xh_set_pattern (xh, "*")
	    call xh_set_curdir (xh, curdir)
	    call xh_dirlist (xh, CURDIR(xh), PATTERN(xh))
	}
end
