include <gki.h>

define	LEN_DEFIBUF	2048
define	ONEWORD		SZ_SHORT
define	TWOWORDS	(2*SZ_SHORT)
define	MAX_RANGES	100
define	MAX_FRAMES	8192
define	SZ_TEXT		(5 * SZ_LINE)

# Header fields of a GKI instruction.
define	I_BOI		Mems[$1+GKI_HDR_BOI-1]
define	I_OPCODE	Mems[$1+GKI_HDR_OPCODE-1]
define	I_LENGTH	Mems[$1+GKI_HDR_LENGTH-1]
define	I_DATA		Mems[$1+GKI_DATAFIELDS-1]
define	WS_MODE		Mems[$1+GKI_OPENWS_M - 1]

# T_GKIEXTRACT -- extract individual frames from a GKI metacode file.

procedure t_gkiextract ()

char	mc_fname[SZ_FNAME], frames_list[SZ_LINE]
int	list, mfd, verify, nframes, junk
int	this_frame, frames[3,MAX_RANGES]
pointer	index, sp

bool	clgetb()
int	clpopni(), clgfil(), open(), get_next_number()
int	decode_ranges(), btoi(), gke_user_go_ahead()

begin
	# Allocate space for the index array.
	call smark (sp)
	call salloc (index, 4 * MAX_FRAMES, TY_INT)

	list = clpopni ("input")
	call clgstr ("frames", frames_list, SZ_LINE)
	if (decode_ranges (frames_list, frames, MAX_RANGES, junk) == ERR)
	    call error (1, "Ranges of frames incorrectly specified")
	verify = btoi (clgetb ("verify"))

	# Loop through the list of metacode frames.
	while (clgfil (list, mc_fname, SZ_FNAME) != EOF) {
	    mfd = open (mc_fname, READ_ONLY, BINARY_FILE)
	    call gke_make_index (mfd, Memi[index], nframes)

	    # Position to beginning of metacode file.
	    call seek (mfd, BOF)

	    this_frame = 0
	    while (get_next_number (frames, this_frame) != EOF) {
		if (this_frame > nframes) {
		    call eprintf ("Metacode file '%s' contains %d frames\n")
			call pargstr (mc_fname)
			call pargi (nframes)
		    break
		}

	        if (verify == YES) {
		    if (gke_user_go_ahead (this_frame, Memi[index]) == YES)
		        call gke_extract_plot (mfd, Memi[index], this_frame)
	        } else
	            call gke_extract_plot (mfd, Memi[index], this_frame)
	    }
	    call close (mfd)
	}

	call clpcls (list)
	call sfree (sp)
end


# GKE_USER_GO_AHEAD -- Print metacode frame directory and query the user.

int procedure gke_user_go_ahead (this_frame, index)

int	this_frame		# Current frame number
int	index[4, ARB]		# Metacode index

pointer	tty
pointer	ttyodes()
bool	clgetb()

begin
	# Print directory information for this_frame from index.
	call eprintf ("   [%d] (%d words) %26t%s")
	    call pargi (this_frame)
	    call pargi (index[3, this_frame])
	    call pargstr (Memc[index[4, this_frame]])

	# Now get user response from terminal.
	tty = ttyodes ("terminal")
	call flush (STDOUT)
	call clputb ("go_ahead", clgetb ("default_action"))
	call eprintf ("    Extract")
	call flush (STDERR)
	call ttycdes (tty)

	if (!clgetb ("go_ahead"))
	    return (NO)
	else 
	    return (YES)
end


# GKE_EXTRACT_PLOT -- extract the specified frame from a metacode file.
# Information about the frame's location and length is stored in "index".

procedure gke_extract_plot (mf, index, this_frame)

int	mf			# Metacode file descriptor
int	index[4,ARB]		# Index of metacode frames
int	this_frame		# Current frame number

int	mc_begin, nchars
pointer	metacode, sp
int	read()
errchk	seek, salloc, read, write, mfree

begin
	# Allocate space for the metacode instructions
	call smark (sp)
	nchars = index[3,this_frame] * SZ_SHORT
	call salloc (metacode, nchars, TY_CHAR)

	# Position to proper place in metacode file
	mc_begin = index[2,this_frame]
	iferr (call seek (mf, mc_begin))
	    call error (2, "Unable to position to metacode frame")

	# Now to read the metacode 
	if (read (mf, Memc[metacode], nchars) == EOF)
	    call error (3, "Unexpected EOF in metacode file encountered")

	# Write buffer to STDOUT
	call write (STDOUT, Memc[metacode], nchars)
	call flush (STDOUT)

	call sfree (sp) 
end


# GKE_MAKE_INDEX -- reads through a metacode file, returning an index of
# all plots in the file.  For each frame in the metacode file, the index 
# contains 4 entries:  plot ordinal, starting location, length and a pointer 
# to the title_string.

procedure gke_make_index (mf, index, nframes)

int	mf			# Metacode file descriptor
int	index[4,ARB]		# Index of metacode frames
int	nframes			# Number of frames in index (output)

bool	new_frame
char	tx_string[SZ_TEXT+1]
pointer	gki, ptr
int	nframe, length, mc_length, seek_text, nchars_read
int	nchars, nchars_max, op_code, file_pos
int	gke_read_next_instruction()
errchk	gke_read_next_instruction

begin
	nframe = 0
	mc_length = 0
	file_pos = 1
	new_frame = false

	repeat {
	    length = gke_read_next_instruction (mf, gki, nchars_read)
	    if (length == EOF)
	        break

	    file_pos = file_pos + nchars_read
	    op_code = I_OPCODE (gki)

	    if ((op_code == GKI_OPENWS && WS_MODE(gki) == NEW_FILE) ||
	    	(op_code == GKI_CLEAR)) {
		# New frame encountered, terminating previous plot.

		if (new_frame) {
		    # Last instruction was also new_frame.  Just bump length.
		    mc_length = mc_length + length
		    next
		} else
		    new_frame = true

		if (nframe > 0) {
		    # Fill index entries 
		    index[1,nframe] = nframe
		    index[2,nframe + 1] = file_pos - length
		    index[3,nframe] = mc_length
		    call malloc (ptr, nchars_max + 1, TY_CHAR)
		    call strcpy (tx_string, Memc[ptr], nchars_max)
		    index[4,nframe] = ptr
		} else 
		    # All that can be set is the file position
		    index[2,nframe + 1] = file_pos - length

		# Increment or reinitialize internal variables
		nframe = nframe + 1
		if (nframe > MAX_FRAMES)
		    call error (4, "Too many frames in metacode file.")
		mc_length = length
		nchars_max = 0
		call strcpy ("(no title)", tx_string, SZ_LINE)
		seek_text = YES

	    } else {
		new_frame = false
	        mc_length = mc_length + length
	    }

	    if (op_code == GKI_MFTITLE) {
		# No need to look at gtext commands any more -- found a title.
		seek_text = NO
		nchars_max = Mems[gki + GKI_MFTITLE_N - 1]
		nchars_max = min (SZ_TEXT, nchars_max)
		call achtsc (Mems[gki+GKI_MFTITLE_T-1], tx_string, nchars_max)
		tx_string[nchars_max+1] = EOS
	    }

	    if (op_code == GKI_TEXT && seek_text == YES) {
		# If this is the longest string so far, save it as title.
		nchars = Mems[gki + GKI_TEXT_N - 1]
		if (nchars > nchars_max) {
		    nchars_max = min (SZ_TEXT, nchars)
		    call achtsc (Mems[gki+GKI_TEXT_T-1], tx_string, nchars_max)
		    tx_string[nchars_max+1] = EOS
		}
	    }
	}

	# Store information for last plot in index, as long as last plot
	# isn't only a clear instruction.

	if (mc_length > GKI_CLEAR_LEN) {
            index[1,nframe] = nframe
            index[3,nframe] = mc_length
            call malloc (ptr, nchars_max + 1, TY_CHAR)
            call strcpy (tx_string, Memc[ptr], nchars_max)
            index[4,nframe] = ptr
	    nframes = nframe
	} else
	    nframes = nframe - 1
end


# GKE_READ_NEXT_INSTRUCTION -- read the next instruction from the input
# stream, returning a buffer pointer to the instruction and the number of
# chars read to get to this position.  This is a modified version of 
# gki_fetch_next_instruction, in that the total number of chars read
# (including partial and botched instructions) is returned as a procedure
# argument.

int procedure gke_read_next_instruction (fd, instruction, nchars_total)

int	fd			# input file containing metacode
pointer	instruction		# pointer to instruction (output)
int	nchars_total		# number of chars read from input stream

int	len_ibuf, nchars, nchars_read
pointer	ibuf
int	read()
errchk	read
data	ibuf/NULL/

begin
	# Allocate a default sized instruction buffer.  We can reallocate
	# a larger buffer later if necessary.

	if (ibuf == NULL) {
	    call malloc (ibuf, LEN_DEFIBUF, TY_SHORT)
	    len_ibuf = LEN_DEFIBUF
	}

	# Advance to the next instruction.  Nulls and botched portions of
	# instructions are counted.  Read the instruction header to determine
	# the length of the instruction, and then read the rest of instruction
	# into buffer.  If the entire instruction cannot be read we have a
	# botched instruction and must try again.  The total number of chars
	# read from the input stream is accumulated and returned as an
	# argument.

	nchars_total = 0
	repeat {
	    repeat {
		nchars_read = read (fd, I_BOI(ibuf), ONEWORD)
		if (nchars_read == EOF)
		    return (EOF)
		else 
		    nchars_total = nchars_total + nchars_read
	    } until (I_BOI(ibuf) == BOI)

	    nchars_read = read (fd, I_OPCODE(ibuf), TWOWORDS)
	    if (nchars_read == EOF)
		return (EOF)
	    else
		nchars_total = nchars_total + nchars_read
	    
	    # Make instruction buffer large enough to hold instruction.
	    # Compute length of remainder of instruction in chars.

	    if (I_LENGTH(ibuf) > len_ibuf) {
		len_ibuf = I_LENGTH(ibuf)
		call realloc (ibuf, len_ibuf, TY_SHORT)
	    }

	    nchars = (I_LENGTH(ibuf) - LEN_GKIHDR) * SZ_SHORT
	    if (nchars == 0)
		break

	    nchars_read = read (fd, I_DATA(ibuf), nchars)
	    if (nchars_read != EOF)
	        nchars_total = nchars_total + nchars_read
	} until (nchars_read == nchars)

	instruction = ibuf

	# Check for a soft end of file, otherwise return the length of the
	# instruction as the function value.

	if (I_OPCODE(ibuf) == GKI_EOF)
	    return (EOF)
	else
	    return (I_LENGTH(ibuf))
end
