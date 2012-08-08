include	<ctype.h>
include	"../curses.h"

define	SYNTAX		1
define	K_CMDLEN	7

# K_COMPILE -- Compile a file of editing key definitions (edcap)
#
# B.Simon	23-Jan-89	Original

procedure k_compile (cmdlist)

char	cmdlist[ARB]	# i: List of commands
#--
include	"screen.com"

char	sep
int	ic, maxcmd, nkey, maxkey
int	hlength, hwidth, hstart, hleft, fd, tag
pointer	sp, label, escape, name

bool	streq()
int	k_capfile(), ps_width(), fscan(), nscan(), strdic(), gstrcpy()

begin
	# Allocate dynamic memory for strings

	call smark (sp)
	call salloc (label, SZ_FNAME, TY_CHAR)
	call salloc (escape, SZ_FNAME, TY_CHAR)
	call salloc (name, SZ_FNAME, TY_CHAR)

	# Count the number of editing commands

	maxcmd = 0
	sep = cmdlist[1]
	for (ic = 1; cmdlist[ic] != EOS; ic = ic + 1) {
	    if (cmdlist[ic] == sep && cmdlist[ic+1] != EOS)
		maxcmd = maxcmd + 1
	}

	# Allocate dynamic memory for the function key table

	nkey = 0
	maxkey = K_CMDLEN * maxcmd
	call calloc (keytab, 4*maxkey, TY_INT)

	# Set up help text

	hwidth = ps_width() / 2
	hlength = maxcmd * hwidth
	call calloc (htext, hlength+1, TY_CHAR)
	hstart = htext
	hleft = hlength

	# Add each line from the edcap file to the table of function
	# key sequences and the help screen

	fd = k_capfile ()
	while (fscan (fd) != EOF) {
	    call gargwrd (Memc[label], SZ_FNAME)
	    call gargwrd (Memc[escape], SZ_FNAME)
	    call gargwrd (Memc[name], SZ_FNAME)

	    # Proceess line only if all three elements of command are present
	    # and command is found in command list

	    if (nscan () == 3) {

		if (streq (Memc[label], "EDIT_INIT"))
		    call k_convert (Memc[escape], ks, K_CMDLEN)
		else if (streq (Memc[label], "EDIT_TERM"))
		    call k_convert (Memc[escape], ke, K_CMDLEN)

		tag = strdic (Memc[label], Memc[label], SZ_FNAME, cmdlist)
		if (tag > 0) {
		    tag = tag + K_BASE - 1

		    # Add escape sequence to function key table

		    call k_doline (Memc[escape], tag, maxkey, 
				   nkey, Memi[keytab])

		    # Add label and name to help text

		    if (hleft > hwidth) {
			hstart = gstrcpy (Memc[label], Memc[hstart], hleft) +
				 hstart

			Memc[hstart] = '='
			hstart = hstart + 1			

			hstart = gstrcpy (Memc[name], Memc[hstart], hleft) +
				 hstart

			Memc[hstart] = '\n'
			hstart = hstart + 1
			hleft = hlength - (hstart - htext)
		    }
		}
	    }
	}

	Memc[hstart] = EOS
	call close (fd)
	call sfree (sp)
end

# K_CAPFILE -- Open the editing capabilities file (edcap)

int procedure k_capfile ()

#--
int	fd
pointer	sp, editor, edcap

int    	envgets(), access(), open()

begin
	# Allocate dynamic memory for strings

	call smark (sp)
	call salloc (editor, SZ_FNAME, TY_CHAR)
	call salloc (edcap, SZ_FNAME, TY_CHAR)

	# Get the name of the edcap file and open it

	if (envgets ("editor", Memc[editor], SZ_FNAME) <= 0)
	    call error (SYNTAX, "Editor not found")

	call sprintf (Memc[edcap], SZ_FNAME, "home$%s.ed")
	    call pargstr (Memc[editor])

	if (access (Memc[edcap], READ_ONLY, 0) == NO) {

	    call sprintf (Memc[edcap], SZ_FNAME, "dev$%s.ed")
		call pargstr (Memc[editor])

	    if (access (Memc[edcap], READ_ONLY, 0) == NO)
		call error (SYNTAX, "Edcap file not found")

	}

	fd = open (Memc[edcap], READ_ONLY, TEXT_FILE)
	call sfree (sp)

	return (fd)
end
