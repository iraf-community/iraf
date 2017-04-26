# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gki.h>

define	I_OPCODE	Mems[$1+GKI_HDR_OPCODE-1]
define	WS_MODE		Mems[$1+GKI_OPENWS_M - 1]
define	SZ_TEXT		(5 * SZ_LINE)

# T_GKIDIR -- print a directory of frames in the named GKI metacode files.

procedure t_gkidir () 

pointer	sp, mc_fname
int	list, mfd
int	clpopni(), open(), clgfil()

begin
	call smark (sp)
	call salloc (mc_fname, SZ_FNAME, TY_CHAR)

	list = clpopni ("input")
	while (clgfil (list, Memc[mc_fname], SZ_FNAME) != EOF) {
	    mfd = open (Memc[mc_fname], READ_ONLY, BINARY_FILE)
	    call printf ("\nMETAFILE '%s':\n")
		call pargstr (Memc[mc_fname])
	    call gkd_read_metacode (mfd)
	    call close (mfd)
	}

	call clpcls (list)
	call sfree (sp)
end


# GKD_READ_METACODE -- reads through a metacode file, printing a directory of
# the size and title of each frame in the file.

procedure gkd_read_metacode (mf)

int	mf	# Metacode file descriptor

bool	new_frame
char	tx_string[SZ_TEXT+1]
pointer	gki
int	nframe, length, mc_length, seek_text
int	nchars, nchars_max, op_code
int	gki_fetch_next_instruction()

errchk	gki_fetch_next_instruction

begin
	nframe = 0
	mc_length = 0
	new_frame = false

	repeat {
	    length = gki_fetch_next_instruction (mf, gki)
	    if (length == EOF)
	        break

	    op_code = I_OPCODE (gki)

	    if ((op_code == GKI_OPENWS && WS_MODE(gki) == NEW_FILE) ||
	    	(op_code == GKI_CLEAR)) {
		# New frame encountered, marks end of previous frame.

		if (new_frame) {
		    # Last instruction was also a new frame.  Just bump length.
		    mc_length = mc_length + length
		    next
		} else
		    new_frame = true

		if (nframe > 0)
		    call gkd_print_directory (nframe, mc_length, tx_string)

		nframe = nframe + 1
		mc_length = length
		nchars_max = 0
		call strcpy ("(no title)", tx_string, SZ_LINE)
		seek_text = YES

	    } else {
		new_frame = false
	        mc_length = mc_length + length
	    }

	    if (op_code == GKI_MFTITLE) {
		# No need to look at gtext commands any more - found a title.
		seek_text = NO
		nchars_max = min (int(Mems[gki+GKI_MFTITLE_N-1]), SZ_TEXT)
		call achtsc (Mems[gki+GKI_MFTITLE_T-1], tx_string, nchars_max)
		tx_string[nchars_max+1] = EOS
	    }

	    if (op_code == GKI_TEXT && seek_text == YES) {
		# If this is longest string so far, save it as title.
		nchars = Mems[gki + GKI_TEXT_N - 1]
		if (nchars > nchars_max) {
		    nchars_max = min (nchars, SZ_TEXT)
		    call achtsc (Mems[gki+GKI_TEXT_T-1], tx_string, nchars_max)
		    tx_string[nchars_max+1] = EOS
		}
	    }
	}

	# Print information from last frame in index, unless last frame is
	# only a clear instruction.

	if (mc_length > GKI_CLEAR_LEN)
	    call gkd_print_directory (nframe, mc_length, tx_string)
end


# GKD_PRINT_DIRECTORY -- Print directory information of metacode frame.

procedure gkd_print_directory (nframe, size, title)

int	nframe 		# Frame number
int	size 		# Length of metacode file
char    title[ARB]	# Metacode title

begin
	call printf ("    [%d]  (%d words)  %26t%s\n")
	    call pargi (nframe)
	    call pargi (size)
	    call pargstr (title)
end
