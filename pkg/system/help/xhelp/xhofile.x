# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<fset.h>
include	"xhelp.h"


# XH_OPEN_FILE -- Open the named file and send it to the GUI.  If this is
# a help document (i.e. it contains a ".help" block) we first convert it
# to HTML, otherwise send it as is.

procedure xh_open_file (xh, parameter, filename, check_for_help, warn)

pointer	xh					# task descriptor
char	parameter[ARB]				# GUI parameter to notify
char	filename[ARB]				# file to open
int	check_for_help				# check file for help block?
int	warn					# warn if not present?

pointer	sp, ip, buf, out, text
int	fdi, fdo
long	fsize
bool	has_help

int	access(), open(), getline()
int	strmatch(), gstrcpy()
long	fstatl()
errchk	open

define	err_	99

begin
	call smark (sp)
	call salloc (buf, SZ_LINE, TY_CHAR)
	call salloc (out, SZ_FNAME, TY_CHAR)

	# Make sure the file exists.
        if (access (filename, 0, 0) == NO) {
	    if (warn == YES) {
                call sprintf (Memc[buf], SZ_LINE, "File does not exist:\n`%s'.")
                    call pargstr (filename)
		call gmsg (XH_GP(xh), "alert", Memc[buf])
	    }
	    goto err_
	} else if (access (filename, 0, BINARY_FILE) == YES) {
	    if (warn == YES) {
                call sprintf (Memc[buf], SZ_LINE, 
		    "Attempt to load binary file:\n`%s'.")
                        call pargstr (filename)
		call gmsg (XH_GP(xh), "alert", "pop")
		call gmsg (XH_GP(xh), "alert", Memc[buf])
	    }
	    goto err_
	}

	# If we're told not to look for help simply open the file and send
	# it to the GUI (e.g. used for homepage and online help).
	if (check_for_help == NO) {
	    call xh_load_file (xh, parameter, filename)
	    call sfree (sp)
	    return
	}

	# Open the file.
	iferr (fdi = open (filename, READ_ONLY, TEXT_FILE)) {
	    if (warn == YES) {
                call sprintf (Memc[buf], SZ_LINE, "Cannot open file\n`%s'.")
                    call pargstr (filename)
		call gmsg (XH_GP(xh), "alert", Memc[buf])
	    }
	    goto err_
	}

	# Allocate an array the length of the file, if this isn't a help file
	# we use this as the message buffer and send it to the GUI.
	fsize = fstatl (fdi, F_FILESIZE)
	call salloc (text, fsize+1, TY_CHAR)
	call aclrc (Memc[text], fsize+1)

	# See whether this is a help file
	has_help = FALSE
	ip = text
	while (getline (fdi, Memc[buf]) != EOF) {
            if (strmatch (Memc[buf], "^.help") > 0) {
		has_help = TRUE
		break
	    }
	    ip = ip + gstrcpy (Memc[buf], Memc[ip], SZ_LINE)
	}
	Memc[ip] = EOS


	# If the file was found to have a .help block we're positioned at
	# the beginning of the block.  Convert the remainder to an HTML
	# temp file and send that to the GUI, otherwise we already have the
	# contents of the file in the text buffer so send that.
	if (has_help) {
	    # Create an output filename and open it for writing.
	    call mktemp ("tmp$xhelpi", Memc[out], SZ_FNAME)
	    fdo = open (Memc[out], NEW_FILE, TEXT_FILE)

	    # Convert the remainder to HTML and send it to the GUI.
	    if (fdo != ERR) {
        	call lroff2html (fdi, fdo, filename, "", "", "", "")
		call close (fdo)

		call xh_load_file (xh, "helpres", Memc[out])
		call delete (Memc[out])
	    }

	} else {
	    # No help was found, send the contents straight to the display.
            call xh_text_msg (XH_GP(xh), "helpres", Memc[text])
	}


err_	if (fdi != ERR)
	    call close (fdi)
	call sfree (sp)
end


# XH_LOAD_FILE -- Load the named file in the GUI.

procedure xh_load_file (xh, parameter, filename)

pointer	xh				# task descriptor
char	parameter[ARB]			# GUI parameter to notify
char	filename[ARB]			# file to display

pointer	sp, ip, line, text
int	fd, open(), getline(), gstrcpy()
long	fsize, fstatl()
errchk	open

begin
	call smark (sp)
	call salloc (line, SZ_LINE, TY_CHAR)

	# Open the file and send it to the display.
	fd = open (filename, READ_ONLY, TEXT_FILE)
	if (fd != ERR) {
	    fsize = fstatl (fd, F_FILESIZE)
	    call salloc (text, fsize+1, TY_CHAR)
	    call aclrc (Memc[text], fsize+1)

	    for (ip=text; getline (fd, Memc[line]) != EOF; )
	        ip = ip + gstrcpy (Memc[line], Memc[ip], SZ_LINE)

	    Memc[ip] = EOS
	    call close (fd)

            call xh_text_msg (XH_GP(xh), parameter, Memc[text])
	}

	call sfree (sp)
end


# XH_TEXT_MSG -- Send a text message to a named UI parameter but first
# escape all curly braces so it passes through the Tcl correctly.

procedure xh_text_msg (gp, param, msg)

pointer gp
char	param[ARB], msg[ARB]

pointer	buf, ip
int	i, nchars
int	strlen()

begin
	nchars = strlen (msg)
	call calloc (buf, nchars + SZ_LINE, TY_CHAR)

	ip = buf
	for (i=1; i < nchars; i=i+1) {
	    if (msg[i] == '{' || msg[i] == '}') {
		Memc[ip] = '\\'
		ip = ip + 1
	    }
	    Memc[ip] = msg[i]
	    ip = ip + 1
	}

	call gmsg (gp, "type", "file")
	call gmsg (gp, param, Memc[buf])
	call mfree (buf, TY_CHAR)
end
