# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<fset.h>
include	<gset.h>

define	XS	0.216
define	XE	0.719
define	YS	0.214
define	YE	0.929

task test = t_test

# T_TEST -- Test program for graphics plotting.  A labelled grid is output.

procedure t_test ()

bool	redir
pointer	sp, gp
char	command[SZ_LINE], image[SZ_FNAME], word[SZ_LINE]
char	output[SZ_FNAME], output_file[SZ_FNAME], device[SZ_FNAME]
int	cmd, input_fd, stat, fd

pointer	gopen()
bool	streq()
int	fstati(), open(), getline()

begin
	# If the input has been redirected, input is read from the named
	# command file.  If not, each image name in the input template is
	# plotted.
	
	if (fstati (STDIN, F_REDIR) == YES) {
call eprintf ("Input has been redirected\n")
	    redir = true
	    cmd = open (STDIN, READ_ONLY, TEXT_FILE)
	} 

	# Loop over commands until EOF
	repeat {
	    if (redir) {
		if (getline (STDIN, command, SZ_LINE) == EOF)
		    break
		call sscan (command)
		    call gargwrd (word, SZ_LINE)
		if (!streq (word, "plot")) {
		    # Pixel window has been stored as WCS 2
		    call gseti (gp, G_WCS, 2)
		    call gscan (command)
		    next
		} else 
		    call gargwrd (image)
	    } 

	    call clgstr ("output", output, SZ_FNAME)
	    if (!streq (output, "")) {
	        call strcpy (output, output_file, SZ_FNAME)
	        fd = open (output_file, NEW_FILE, BINARY_FILE)
	    } else
		fd = open ("dev$crt", NEW_FILE, BINARY_FILE)

	    call clgstr ("device", device, SZ_FNAME)
	    gp = gopen (device, NEW_FILE, fd)
		
	    call gseti (gp, G_XDRAWGRID, 1)
	    call gseti (gp, G_YDRAWGRID, 1)
	    call gseti (gp, G_NMAJOR, 21)
	    call glabax (gp, "TEST", "NDC_X", "NDC_Y")
	    call gline (gp, XS, YS, XE, YS)
	    call gline (gp, XE, YS, XE, YE)
	    call gline (gp, XE, YE, XS, YE)
	    call gline (gp, XS, YE, XS, YS)
	    call gmark (gp, 0.5, 0.5, GM_CROSS, 3.0, 3.0)
	    call gtext (gp, XS, YS-0.1, "DICOMED crtpict film area")
	    call gclose  (gp)
	    call close   (fd)
	}

	call clpcls (input_fd)
	call sfree   (sp)
end
