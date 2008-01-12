# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<fset.h>
include <ctype.h>
include "../help.h"
include	"xhelp.h"


# XH_FILES -- Get the files associated with the requested help topic,
# i.e. do a "help <topic> opt=files" request.

procedure xh_files (xh, topic, curpack)

pointer xh                                      #i task struct pointer
char    topic[ARB]                           	#i help topic
char    curpack[ARB]                           	#i current package

pointer sp, bp, buf
pointer	opt, val, line
int	fd
long	fsize
char	fname[SZ_FNAME]

long	fstatl()
int	getline(), access(), open(), stridxs(), strlen()

begin
	if (topic[1] == EOS && curpack[1] == EOS)
	    return

	call smark (sp)
	call salloc (line, SZ_LINE, TY_CHAR)
	call salloc (buf, SZ_FNAME, TY_CHAR)
	call salloc (opt, SZ_FNAME, TY_CHAR)
	call salloc (val, SZ_FNAME, TY_CHAR)

	# Get a temp file name.
	call mktemp ("tmp$xhelpi", fname, SZ_FNAME)

	# Open a temp file with the help information found.
	fd = open (fname, NEW_FILE, TEXT_FILE)
	call xh_get_help (fd, topic, curpack, "", HF_HTML, HELPDB(xh),
	    "all", "files")
	call close (fd)

	# Open the results file for reading.
	fd = open (fname, READ_ONLY, TEXT_FILE)
	fsize = fstatl (fd, F_FILESIZE)

	# If we got a result parse the lines for "opt=file" information.
	if (fsize != 0) {

            while (getline (fd, Memc[line]) != EOF) {

		# Stomp the newline.
	  	Memc[line+strlen(Memc[line])-1] = EOS

		# Extract the option type and filename.
		if (stridxs ("=", Memc[line]) > 0) {
		    for (bp=line; IS_WHITE(Memc[bp]); bp=bp+1)
			;
		    call strcpy (Memc[bp], Memc[opt], 3)
		    for (; Memc[bp] != '='; bp=bp+1)
			;
		    call strcpy (Memc[bp+1], Memc[val], SZ_FNAME)

		    # See if the file exists.
		    call sprintf (Memc[buf], SZ_FNAME, "%s %s \0")
		        call pargstr (Memc[opt])
		        call pargstr (Memc[val])
		    if (access (Memc[val],0,0) == YES)
		        call strcat (" 0", Memc[buf], SZ_FNAME)
		    else
		        call strcat (" 1", Memc[buf], SZ_FNAME)
		    call gmsg (XH_GP(xh), "helpfiles", Memc[buf])

		} else if (stridxs (":", Memc[line]) > 0) {
		    call xh_pkgpath (xh, topic, curpack, Memc[line])
		    call sprintf (Memc[buf], SZ_FNAME, "file %s")
		        call pargstr (Memc[line])
		    call gmsg (XH_GP(xh), "helpfiles", Memc[buf])
		}
	    }
	}

	call close (fd)		# clean up 
	call delete (fname)
	call sfree (sp)
end
