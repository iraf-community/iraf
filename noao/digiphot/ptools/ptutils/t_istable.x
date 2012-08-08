include "../../lib/ptkeysdef.h"

# T_ISTABLE -- Decide whether an input file is an ST Table, an APPHOT style
# text file or neither.

procedure t_istable ()

pointer	infile			# name of the input file

bool	table, text, other
int	fd, type
pointer	sp, line
int	access(), tbtopn(), open(), getline(), strmatch()
errchk	tbtopn(), open()

begin
	# Get some working space.
	call smark (sp)
	call salloc (infile, SZ_FNAME, TY_CHAR)
	call salloc (line, SZ_LINE, TY_CHAR)

	# Fetch the name of the input file
	call clgstr ("infile", Memc[infile], SZ_FNAME)
	if (access (Memc[infile], READ_ONLY, TEXT_FILE) == YES)
	    type = TEXT_FILE
	else
	    type = BINARY_FILE

	if (type == BINARY_FILE) {
	    iferr {
	        fd = tbtopn (Memc[infile], READ_ONLY, 0)
	    } then {
		table = false
		text = false
		other = true
	    } else {
		table = true
		text = false
		other = false
		call tbtclo (fd)
	    }
	} else {
	    table = false
	    iferr {
	        fd = open (Memc[infile], READ_ONLY, TEXT_FILE) 
	    } then {
	        text = false
	        other = true
	    } else {
	        Memc[line] = EOS
	        if (getline (fd, Memc[line]) != EOF) {
		    if (strmatch (Memc[line], KY_CHAR_IRAF) != 0) {
		        text = true
		        other = false
		    } else {
		        text = false
		        other = true
		    }
		}
		call close (fd)
	    }
	}

	# Store the results in the istable parameter file.
	call clputb ("table", table)
	call clputb ("text", text)
	call clputb ("other", other)

	# Free memory.
	call sfree (sp)
end
