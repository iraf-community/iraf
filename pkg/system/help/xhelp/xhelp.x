# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	"../help.h"
include	"xhelp.h"


# XHELP -- The main task procedure.  XHELP is a GUI client program for
# browsing the IRAF help system.  As much as possible it uses the existing
# help database code but provides a friendlier interface, allowing users to
# browse packages for help pages in the same way they would browse packages
# in the CL.  It provides an HTML converter for LROFF sources for better
# presentation in the GUI, as well as Postscript generation for better
# looking hardcopy.  XHelp acts as a server for the help system, merely
# returning any output that the GUI has requested.  Navigation is done in
# the GUI code, this program maintains just the state of the last page
# returned and knows nothing about how it got there.  See the xhelp.hlp for
# detailed documentation.

procedure xhelp (topic)

char	topic[ARB]					#I help template

pointer	xh
char	uifname[SZ_FNAME]
int     search, template

pointer	xh_open(), gopenui()
bool    clgetb()
int     btoi()

begin
	# Open structure and allocate pointers.
	xh = xh_open ()
        call strcpy (topic, TEMPLATE(xh), SZ_FNAME)

        # Load the task parameters.
        call clgstr ("option",  OPTION(xh),  SZ_FNAME)
        call clgstr ("printer", PRINTER(xh), SZ_FNAME)
        call clgstr ("quickref", QUICKREF(xh), SZ_FNAME)
	XH_SHOWTYPE(xh) = btoi (clgetb("showtype"))
	search = btoi (clgetb("search"))
	template = btoi (clgetb("file_template"))

        # Fetch the name of the help database.
	call xh_ghelpdb (xh)

        # Open the GUI.
        call clgstr ("uifname", uifname, SZ_FNAME)
        XH_GP(xh) = gopenui ("stdgraph", NEW_FILE, uifname, STDGRAPH)
	call gflush (XH_GP(xh))

	# Initialize the task and send topic list to the GUI.
	call xh_init (xh, template, search)

	# Initialize the task and send topic list to the GUI.
	call xh_command_loop (xh)

	# Clean up.
	call gclose (XH_GP(xh))
	call xh_close (xh)
end


# XH_OPEN -- Open and allocate the XHELP task structure.

pointer procedure xh_open ()

pointer	xh					# task descriptor
errchk	calloc

begin
        iferr (call calloc (xh, SZ_XHELPSTRUCT, TY_STRUCT))
            call error (0, "Error opening task structure.")

	iferr {
            call calloc (XH_LPTR(xh), SZ_HELPLIST, TY_CHAR)
            call calloc (XH_TEMPLATE(xh), SZ_FNAME, TY_CHAR)
            call calloc (XH_OPTION(xh), SZ_FNAME, TY_CHAR)
            call calloc (XH_PRINTER(xh), SZ_FNAME, TY_CHAR)
            call calloc (XH_CURTASK(xh), SZ_FNAME, TY_CHAR)
            call calloc (XH_CURPACK(xh), SZ_FNAME, TY_CHAR)
            call calloc (XH_QUICKREF(xh), SZ_FNAME, TY_CHAR)
            call calloc (XH_HOMEPAGE(xh), SZ_FNAME, TY_CHAR)
            call calloc (XH_CURDIR(xh), SZ_PATHNAME, TY_CHAR)
            call calloc (XH_PATTERN(xh), SZ_FNAME, TY_CHAR)
            call calloc (XH_HELPDB(xh), SZ_HELPDB, TY_CHAR)
	} then
	    call error (0, "Error allocating structure pointers.")

	return (xh)
end


# XH_CLOSE -- Close the XHELP task structure.

procedure xh_close (xh)

pointer	xh					# task descriptor

begin
        call mfree (XH_TEMPLATE(xh), TY_CHAR)
        call mfree (XH_OPTION(xh), TY_CHAR)
        call mfree (XH_PRINTER(xh), TY_CHAR)
        call mfree (XH_CURTASK(xh), TY_CHAR)
        call mfree (XH_CURPACK(xh), TY_CHAR)
        call mfree (XH_QUICKREF(xh), TY_CHAR)
        call mfree (XH_HOMEPAGE(xh), TY_CHAR)
        call mfree (XH_CURDIR(xh), TY_CHAR)
        call mfree (XH_PATTERN(xh), TY_CHAR)
        call mfree (XH_HELPDB(xh), TY_CHAR)
        call mfree (XH_LPTR(xh),  TY_CHAR)

        call mfree (xh, TY_STRUCT)
end


# XH_GHELPDB -- Fetch the name of the help database, i.e., "helpdb",
# "helpdir",  or the name of a file.   If the helpdb string is a list check
# for the existance of each file in the list to ensure the final list
# contains only valid help databases.

procedure xh_ghelpdb (xh)

pointer	xh					# task descriptor

pointer	sp, hdb, hdbstr, name
int	list
int	fntopnb(), fntgfnb()
int	access(), envgets()
bool	streq()

begin
	call smark (sp)
	call salloc (name, SZ_FNAME, TY_CHAR)
	call salloc (hdb, SZ_HELPDB, TY_CHAR)
	call salloc (hdbstr, SZ_HELPDB, TY_CHAR)

	# Clear the working memory.
	call aclrc (Memc[name], SZ_FNAME)
	call aclrc (Memc[hdb], SZ_HELPDB)
	call aclrc (Memc[hdbstr], SZ_HELPDB)

	# Get the parameter value.
        call clgstr ("helpdb", Memc[hdbstr], SZ_HELPDB)
        if (streq (Memc[hdbstr], "helpdb"))
            if (envgets ("helpdb", Memc[hdbstr], SZ_HELPDB) <= 0)
                call syserrs (SYS_ENVNF, "helpdb")

	# Open the list.
	list = fntopnb (Memc[hdbstr], YES)

	# Copy each of the existing files in the list to the output database
	# string to be used by the task.
	while (fntgfnb(list, Memc[name], SZ_FNAME) != EOF) {
	    if (access (Memc[name], 0, 0) == YES) {
		if (Memc[hdb] != EOS)
		    call strcat (",", Memc[hdb], SZ_HELPDB)
		call strcat (Memc[name], Memc[hdb], SZ_HELPDB)
	    }
	}
	call strcpy (Memc[hdb], HELPDB(xh), SZ_HELPDB)

	# Clean up.
	call fntclsb (list)
	call sfree (sp)
end
