# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	"../help.h"
include	"xhelp.h"


# XHELP callback commands.
define  XC_COMMANDS "|help|load|print|quit|search|files|directory|type|package|"
define  CMD_HELP        1
define  CMD_LOAD        2
define  CMD_PRINT       3
define  CMD_QUIT        4
define  CMD_SEARCH      5
define  CMD_FILES       6
define  CMD_DIRECTORY   7
define  CMD_TYPE   	8
define  CMD_PACKAGE     9


# XH_COMMAND_LOOP -- Process the GUI command loop.

procedure xh_command_loop (xh)

pointer	xh					#I task descriptor

pointer	sp, cmd, name, pkg, pat, opt, dev
real    x, y
int     wcs, key, exact_match
char	str[SZ_FNAME]

bool	streq()
int	strdic(), clgcur()
int     xh_pkglist()

begin
	call smark (sp)
	call salloc (name, SZ_FNAME, TY_CHAR)
	call salloc (pkg, SZ_FNAME, TY_CHAR)
	call salloc (opt, SZ_FNAME, TY_CHAR)
	call salloc (dev, SZ_FNAME, TY_CHAR)
	call salloc (cmd, SZ_FNAME, TY_CHAR)
	call salloc (pat, SZ_FNAME, TY_CHAR)


	# Enter the command loop.
        while (clgcur ("coords", x, y, wcs, key, str, SZ_FNAME) != EOF) {

	    # Skip any non-colon commands.
	    if (key != ':') 			
		next

	    # Get the colon command string.
	    call sscan (str)
	        call gargwrd (Memc[cmd], SZ_FNAME)

	    switch (strdic (Memc[cmd], Memc[cmd], SZ_FNAME, XC_COMMANDS)) {
	    case CMD_HELP:
	        call gargwrd (Memc[name], SZ_FNAME)

	        # Get help on the requested topic, updates package list 
	        # if necesary.
	        if (streq(Memc[name],"Home")) {
	    	    call xh_init (xh, NO, YES) 			
	        } else {
	            call gargwrd (Memc[pkg], SZ_FNAME)		# curpack
		    call gargwrd (Memc[opt], SZ_FNAME)		# option
		    call xh_cmd_help (xh, Memc[name], Memc[pkg], Memc[opt])
		}

	    case CMD_FILES:
	        call gargwrd (Memc[name], SZ_FNAME)		# task name
		call gargwrd (Memc[pkg], SZ_FNAME)		# parent package
		call xh_files (xh, Memc[name], Memc[pkg])

	    case CMD_LOAD:
		# Load a requested page from the history.
	        call gargwrd (Memc[name], SZ_FNAME)		# task name
        	call gargwrd (Memc[pkg], SZ_FNAME)		# curpack
        	call gargwrd (Memc[opt], SZ_FNAME)		# help option
        	if (xh_pkglist (xh, Memc[name], HELPDB(xh), LIST(xh)) != 0)
        	    call gmsg (XH_GP(xh), "pkglist", LIST(xh))
        	call xh_help (xh, Memc[name], Memc[pkg], Memc[opt])

	    case CMD_PRINT:
		# Print the current results.
	        call gargwrd (Memc[name], SZ_FNAME)		# task name
        	call gargwrd (Memc[pkg], SZ_FNAME)		# curpack
		call gargwrd (Memc[dev], SZ_FNAME)		# printer name
		call xh_print_help (xh, Memc[name], Memc[pkg], Memc[dev])

	    case CMD_QUIT:
		# Quit the task.
		break

	    case CMD_SEARCH:
		# Get the results of the keyword search.
		call gargi (exact_match)
	        call gargstr (Memc[pat], SZ_FNAME)
		call xh_search (xh, exact_match, Memc[pat])

	    case CMD_DIRECTORY:
		# Process the directory browsing command.
	        call gargwrd (Memc[opt], SZ_FNAME)
		call xh_directory (xh, Memc[opt])

	    case CMD_TYPE:
		# Get the showtype value from the GUI
		call gargi (XH_SHOWTYPE(xh))

	    case CMD_PACKAGE:
		# For the given item return the package in which it
		# was found.  [DEBUG ROUTINE.]
	        call gargwrd (Memc[name], SZ_FNAME)
		call xh_pkgpath (xh, Memc[name], CURPACK(xh), Memc[pkg])
		call printf ("%s => %s\n")
		    call pargstr (Memc[name])
		    call pargstr (Memc[pkg])
		call flush(STDOUT)
	    }
	}

	call sfree (sp)
end


# XH_CMD_HELP -- Process a help command.

procedure xh_cmd_help (xh, topic, curpack, option)

pointer	xh					# task descriptor
char	topic[ARB]				# requested topic
char	curpack[ARB]				# current package
char	option[ARB]				# option (help|source|sysdoc)

int	len

bool    streq()
int	strncmp()
int	xh_pkgname(), xh_pkglist()

begin
	if (streq (option, "help")) {
	    # No package name given, find one and load it.
	    if (streq (curpack, "{}")) {
		curpack[1] = EOS
		len = 0
	    	if (xh_pkgname (xh, topic, curpack) == OK)
		    len = xh_pkglist (xh, curpack, HELPDB(xh), LIST(xh))

		if (len != 0 && 
		    strncmp(curpack, "root", 4) != 0 &&
		    strncmp(curpack, "clpack", 6) != 0) {
		        call gmsg (XH_GP(xh), "pkglist", LIST(xh))
		        call strcpy (curpack, CURPACK(xh), SZ_FNAME)
    		        call gmsg (XH_GP(xh), "curpack", curpack)
			call gmsg (XH_GP(xh), "history", "package")
		}
	    }

	    if (xh_pkglist (xh, topic, HELPDB(xh), LIST(xh)) != 0) {
		# Got a package listing....
		call gmsg (XH_GP(xh), "pkglist", LIST(xh))
		call strcpy (topic, CURPACK(xh), SZ_FNAME)
    		call gmsg (XH_GP(xh), "curpack", topic)
	    }
	}

	if (streq (topic, CURPACK(xh))) {
	    call gmsg (XH_GP(xh), "type", "package")
	    call gmsg (XH_GP(xh), "curtask", topic)
	    if (streq (option, "help"))
		call xh_help (xh, "", CURPACK(xh), option)
	    else
		call xh_help (xh, topic, curpack, option)
	    call strcpy (CURPACK(xh), CURTASK(xh), SZ_FNAME)

	} else {
	    call gmsg (XH_GP(xh), "type", "task")
	    call gmsg (XH_GP(xh), "curtask", topic)
	    call xh_help (xh, topic, CURPACK(xh), option)
	    call strcpy (topic, CURTASK(xh), SZ_FNAME)
	}
	call gmsg (XH_GP(xh), "history", "append")
end
