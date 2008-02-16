# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <ctype.h>
include "../help.h"
include "xhelp.h"

define	SZ_EXTN		10


# XH_SAVE_FILE --  Save the currently displayed page to the named file in
# the specified format.

procedure xh_save_file (xh, iname, oname, format, overwrite)

pointer	xh					#i package descriptor
char	iname[ARB]				#i input file
char	oname[ARB]				#i output filename
char	format[ARB]				#i format
int	overwrite				#i overwrite flag

pointer	sp, buf
char	extn[SZ_EXTN]

int	access(), fnextn(), strlen(), strcmp()

begin
	if (access (oname, 0, 0) == YES && overwrite == NO) {
	    call smark (sp)
	    call salloc (buf, SZ_LINE, TY_CHAR)
	    call sprintf (Memc[buf], SZ_LINE,
		"Operation would overwrite\nexisting file '%s'")
		    call pargstr (oname)
	    call gmsg (XH_GP(xh), "alert", Memc[buf])
	    call sfree (sp)
	    return

	} else if (access (oname, 0, 0) == YES)
	    call delete (oname)

	call aclrc (extn, SZ_EXTN)

	# Strip off any braces around the arguments that were put
	# in by the Tcl script.
	if (iname[1] == '{') {
	    call amovc (iname[2], iname, strlen(iname)-2)
	    iname[strlen(iname)-1] = EOS
	}
	if (oname[1] == '{') {
	    call amovc (oname[2], oname, strlen(oname)-2)
	    oname[strlen(oname)-1] = EOS
	}

	switch (format[1]) {
	case 's':				# source
	    call fcopy (iname, oname)
	case 't':				# text
	    if (fnextn(iname, extn, 3) > 0 && strcmp("hlp", extn) == 0)
		call xh_save_text (xh, iname, oname)
	    else
	        call fcopy (iname, oname)
	case 'h':				# html
	    if (fnextn(iname, extn, 3) > 0 && strcmp("hlp", extn) == 0)
	        call xh_save_html (xh, iname, oname, YES)
	    else
	        call xh_save_html (xh, iname, oname, NO)
	case 'p':				# postscript
	    if (fnextn(iname, extn, 3) > 0 && strcmp("hlp", extn) == 0)
	        call xh_save_ps (xh, iname, oname, YES)
	    else
	        call xh_save_ps (xh, iname, oname, NO)
	default:
	    call gmsg (XH_GP(xh), "alert", "Invalid format specifier")
	}
end


# XH_SAVE_TEXT -- Save the page as a formatted text file.  

procedure xh_save_text (xh, in, out)

pointer	xh					#i package descriptor
char	in[ARB], out[ARB]			#i file names

char	err[SZ_LINE]
int	fdout, open()
errchk	open

begin
	# Open the output file.
	iferr (fdout = open (out, NEW_FILE, TEXT_FILE)) {
            call sprintf (err, SZ_LINE, "Cannot open output file `%s'.")
               call pargstr (out)
	    call gmsg (XH_GP(xh), "alert", err)
	    return
	}

	# Format the help as text.
	call xh_get_help (fdout, "", "", in, HF_TEXT, HELPDB(xh), "all", "help")

	# Close the file.
	call close (fdout)
end


# XH_SAVE_HTML -- Save the page as an HTML  file.  

procedure xh_save_html (xh, in, out, ishelp)

pointer	xh					#i package descriptor
char	in[ARB], out[ARB]			#i file names
int	ishelp					#i is this a help file?

char	err[SZ_LINE]
int	fdout, fdin, open()
errchk	open

begin
	# Open the output file.
	iferr (fdout = open (out, NEW_FILE, TEXT_FILE)) {
            call sprintf (err, SZ_LINE, "Cannot open output file `%s'.")
               call pargstr (out)
	    call gmsg (XH_GP(xh), "alert", err)
	    return
	}

	# Format the help as text.
	if (ishelp == YES) {
	    call xh_get_help (fdout, "", "", in, HF_HTML, HELPDB(xh),
		"all", "help")
	} else {
	    # Open the input file.
	    iferr (fdout = open (out, NEW_FILE, TEXT_FILE)) {
                call sprintf (err, SZ_LINE, "Cannot open output file `%s'.")
                   call pargstr (out)
	        call gmsg (XH_GP(xh), "alert", err)
	        return
	    }

	    call fprintf (out, "<HTML><BODY>\n")
	    call fprintf (out, "<TITLE>%s</TITLE>\n")
		call pargstr (in)
	    call fprintf (out, "<PRE>\n")
	    call fcopyo (in, out)
	    call fprintf (out, "</PRE>\n")
	    call fprintf (out, "</BODY></HTML>\n")

	    call close (fdin)
	}

	# Close the file.
	call close (fdout)
end


# XH_SAVE_PS -- Save the page as a postscript file.  

procedure xh_save_ps (xh, in, out, ishelp)

pointer	xh					#i package descriptor
char	in[ARB], out[ARB]			#i file names
int	ishelp					#i is this a help file?

char	err[SZ_LINE]
int	fdout, open()
errchk	open

begin
	if (ishelp == NO)
	    return

	# Open the output file.
	iferr (fdout = open (out, NEW_FILE, TEXT_FILE)) {
            call sprintf (err, SZ_LINE, "Cannot open output file `%s'.")
               call pargstr (out)
	    call gmsg (XH_GP(xh), "alert", err)
	    return
	}

	# Format the help as text.
	call xh_get_help (fdout, "", "", in, HF_PS, HELPDB(xh), "all", "help")

	# Close the file.
	call close (fdout)
end
