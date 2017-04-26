# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<fset.h>
include	<gio.h>
include	<gki.h>
include	<gset.h>
include	<math.h>
include	<mach.h>

define	END_OF_MC	-10
define	QUIT		-11
define	SZ_COMMAND	10
define	NEW_FRAME	-1
define	SZ_MATCH	3
define	NPAIRS	2

define	cursor_loop_	91

define	LEN_DEFIBUF	2048
define	ONEWORD		SZ_SHORT
define	TWOWORDS	(2*SZ_SHORT)
define	MAX_RANGES	100
define	MAX_FRAMES	500
define	I_BOI		Mems[$1+GKI_HDR_BOI-1]
define	I_OPCODE	Mems[$1+GKI_HDR_OPCODE-1]
define	I_LENGTH	Mems[$1+GKI_HDR_LENGTH-1]
define	I_DATA		Mems[$1+GKI_DATAFIELDS-1]
define	WS_MODE		Mems[$1+GKI_OPENWS_M - 1]
define	KEY		"lib$scr/gkimosaic.key"
define	PROMPT		"Gkimosaic Options"

# T_GKIMOSAIC --  Plot multiple metacode frames on a single output page.
# Input is read from either STDIN or a metacode file; output can be
# sent directly to a named device or a metacode file.  The number of
# plots in both x and y is set by the user.  

procedure t_gkimosaic ()

pointer	sp, device, output, input, vp, ip, wcs
bool	fill, rotate, clear_screen
int	in, nx, ny, inlist, out, interactive, nwcs, buflen
int	nplots_page, index, lastp, nplot, nfiles, nf, pcounter
long	fpos, length_mc
bool	clgetb(), streq()
int	open(), clgeti(), clpopni(), clgfil(), btoi(), fstati(), gm_interact()
int	clplen()
long	gm_rwframe()


begin
	call smark  (sp)
	call salloc (device, SZ_FNAME, TY_CHAR)
	call salloc (input,  SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (wcs, LEN_WCSARRAY, TY_STRUCT)

	call gm_initwcs (wcs)

	# Determine characteristics of input and output; open graphics
	inlist = clpopni ("input")
	call clgstr ("output", Memc[output], SZ_FNAME)
	if (Memc[output] == EOS) 
	    call strcpy ("STDGRAPH", Memc[output], SZ_FNAME)

	out = open (Memc[output], APPEND, BINARY_FILE)

	call clgstr ("device", Memc[device], SZ_FNAME)
	if (streq (Memc[device], "stdgraph")) {
	    if (out != STDGRAPH || fstati (STDGRAPH, F_REDIR) == YES)
		interactive = NO
	    else
	        interactive = btoi (clgetb ("interactive"))
	} else
	    interactive = NO

	call gki_init (out)
	call gki_openws (out, Memc[device], NEW_FILE)

	# Get remaining cl parameters
	nx = max (1, clgeti ("nx"))
	ny = max (1, clgeti ("ny"))
	nplots_page = nx * ny
	fill = clgetb ("fill")
	rotate = clgetb ("rotate")

	# Calculate initial viewport corner points and store in array vp.
	call malloc (vp, nplots_page * 4, TY_REAL)
	call gm_getvp (vp, nx, ny, fill)

	# Initialize flag for clearing screen and plot and file counters.
	nplot = 1
	clear_screen = false
	nwcs = 0
	nfiles = clplen (inlist)
	nf = 0
	pcounter = 0

	# Main processing loop begins here
	while (clgfil (inlist, Memc[input], SZ_FNAME) != EOF) {
	    iferr {
		fpos = 1
		nf = nf + 1
		in = open (Memc[input], READ_ONLY, BINARY_FILE)
	    } then {
		call erract (EA_WARN)
		next
	    }

	    # Initialize memory and plot counters for maintaining index
	    buflen = MAX_FRAMES
	    call calloc (ip, buflen, TY_LONG)
	    Meml[ip] = long (fpos)
	    lastp = 0

	    repeat {
	        if (clear_screen && pcounter > 0) {
		    # Next plot will be first on page.  Attend to any cursor
		    # commands before clearing screen.  Put out accumulated
		    # SETWCS instruction before reading cursor.

cursor_loop_	    call gki_setwcs (out, Memi[wcs], LEN_WCSARRAY)

		    if (interactive == YES) {
		        call gki_flush (out)
			if (gm_interact (in, out, ip, vp, fpos, lastp, nx, ny, 
			    rotate) == QUIT)
			    break
			nplots_page = nx * ny
		    }

		    nplot = 1
		    nwcs = 0
		    pcounter = 0
		    call gm_initwcs (wcs)

		    # Don't want to clear screen if there is no more
		    # data to be plotted.
		    if (nf == nfiles && fpos == EOF)
			break

		    call gki_clear (out)
		}

		index = (nplot - 1) * 4
	        length_mc = gm_rwframe (in, out, Memr[vp+index], rotate, 
		    wcs, nwcs)

		if (length_mc == EOF) {
		    fpos = EOF
		    if (nf == nfiles && pcounter > 0)
			# Last file in list; bring up cursor
			goto cursor_loop_
		    else 
			# Go on to next file in list
			break
		} 

		lastp = lastp + 1
		nplot = nplot + 1
		pcounter = pcounter + 1

		if (nplots_page == 1 || mod (nplot, nplots_page) == 1)
		    clear_screen = true
		else
		    clear_screen = false

		if (length_mc == END_OF_MC)
		    fpos = EOF

		else {
	            # Positioned at beginning of another plot.  See if
	            # index buffer needs to be extended.

	            if (lastp > buflen) {
		        buflen = buflen + MAX_FRAMES
		        call realloc (ip, buflen, TY_LONG)
	            }

		    fpos = fpos + length_mc
	            Meml[ip+lastp] = fpos
	        }
	    }

	    call close (in)
	    call mfree (ip, TY_LONG)
	}

	call mfree (vp, TY_REAL)
	call gki_flush (out)
	call gki_closews (out, Memc[device])
	call close (out)
	call clpcls (inlist)

	call sfree  (sp)
end


# GM_INTERACT -- respond to user's interactive cursor commands.  The values
# of nx, ny, rotate and fill can change, requiring the vp array to be
# modified.  The metacode file can also be repositioned here, and the
# index of frame positions is modified accordingly.  A value of QUIT or
# OK is returned.

int procedure gm_interact (in, out, ip, vp, fpos, lastp, nx, ny, rotate)

int	in		# File descriptor for input metacode file
int	out		# File descriptor for output graphics stream
pointer	ip		# Pointer to index
pointer	vp		# Pointre to viewport array
int	fpos
int	lastp
int	nx, ny		# The number of plots in x and y
bool	rotate		# Rotate plots (y/n)?

pointer	sp, bp
bool	fill
int	nskip, new_vport, junk, key, cval, nxold, nyold
real	wx, wy
int	clgcur()

begin
	call smark (sp)
	call salloc (bp, SZ_COMMAND, TY_CHAR)
	nskip = 0
	new_vport = NO

	nxold = nx
	nyold = ny

	repeat {
	    cval = clgcur ("cursor", wx, wy, junk, key, Memc[bp], SZ_LINE)
	    if (cval == EOF) {
		call sfree (sp)
		return (QUIT)
	    }

            switch (key) {
            case 'q':
		call sfree (sp)
                return (QUIT)
            case ':':
                call gm_colon (Memc[bp], nx, ny, fill, new_vport, rotate, nskip)
	    case ' ':
		break
	    case '?':
		call gm_help (out, KEY)
	    case 'r':
		nskip = -1 * (nxold * nyold)
		break
            default:
	        call printf ("\07")
    	    }
	}

	# Reset viewport if necessary
        if (new_vport == YES) {
	    call realloc (vp, nx * ny * 4, TY_LONG)
	    call gm_getvp (vp, nx, ny, fill)
        }

	# Position metacode if necessary
	if (nskip != 0)
	    call gm_posmc (in, fpos, lastp, Meml[ip], nskip)

	call sfree (sp)
	return (OK)
end


# GM_RWFRAME --  Read and write a metacode frame to the graphics stream, 
# transforming coordinates as necessary.  This procedure returns the
# position in the input file which is entered into the metacode index
# for positioning.

long procedure gm_rwframe (in, out, vport, rotate, frame_wcs, nwcs)

int	in		# Metacode file descriptor
int	out		# File descriptor for graphics stream
real	vport[ARB]	# Array of viewport corner points
bool	rotate		# Rotate frame (y/n?)
pointer	frame_wcs	# Pointer to accumulated SETWCS instruction
int	nwcs		# Counter for number of SETWCS instructions encountered

pointer	gki
int	n_instructions, nchars_read, stat
long	length_mc
int	gm_read_next_instruction(), gm_writemc()
errchk	gm_read_next_instruction, gm_writemc

begin
	call gm_trinit (vport, rotate)
	n_instructions = 0
	length_mc = 0

	repeat {
	    if (gm_read_next_instruction (in, gki, nchars_read) == EOF) {
		if (length_mc == 0)
		    return (EOF)
		else
		    return (END_OF_MC)
	    }
	    
	    length_mc = length_mc + nchars_read
	    stat = gm_writemc (out, Mems[gki], frame_wcs, nwcs)

	    if (stat == NEW_FRAME && n_instructions > 1) 
	        return (length_mc)

	    else if (stat != NEW_FRAME)
		n_instructions = n_instructions + 1
	}
end


# GM_COLON -- Get options from colon commands.

procedure gm_colon (cmdstr, nx, ny, fill, new_vport, rotate, nskip)

char	cmdstr[ARB]
int	nx, ny
bool	fill
int	new_vport
bool	rotate
int	nskip

pointer	sp, bp, mp
bool	tempb, plus_sign
int	ncmd, tempi
int	strdic(), nscan(), stridxs()
errchk	strdic, nscan, stridxs

string	cmds "|nx|ny|fill|rotate|skip|"

begin
	call smark (sp)
	call salloc (bp, SZ_COMMAND, TY_CHAR)
	call salloc (mp, SZ_MATCH, TY_CHAR)

	# Parse the command string with fmtio.  First look for a minus sign,
	# then find the string in the string index, matching only the
	# first SZ_MATCH characters.

	call sscan (cmdstr)
	call gargwrd (Memc[bp], SZ_COMMAND)

	plus_sign = true
	if (stridxs ("-", Memc[bp]) > 0)
	    plus_sign = false
	call strcpy (Memc[bp], Memc[mp], SZ_MATCH)

	ncmd = strdic (Memc[mp], Memc[bp], SZ_MATCH, cmds)

	# Switch on the command and parse the arguments.

	switch (ncmd) {
	case 1:
	    # nx
	    call gargi (tempi)
	    if (nscan() >= 2) {
	        new_vport = YES
		nx = tempi
	    }

	case 2:
	    # ny
	    call gargi (tempi)
	    if (nscan() >= 2) {
		new_vport = YES
		ny = tempi
	    }

	case 3:
	    # fill
	    call gargb (tempb)
	    new_vport = YES

	    if (nscan() >= 2)
		fill = tempb
	    else 
		# Could be just "fill" or have either a +/-
		fill = plus_sign

	case 4:
	    # rotate
	    call gargb (tempb)

	    if (nscan() >= 2)
		rotate = tempb
	    else 
		# Could be just "rotate" or have either a +/-
		rotate = plus_sign

	case 5:
	    # skip
	    call gargi (tempi)
	    if (nscan() >= 2)
		nskip = tempi
	    else
		nskip = 0

	default:
	    # beep
	    call eprintf ("\07")
	    call flush (STDERR)
	}

	call sfree (sp)
end


# GM_POSMC -- position metacode file by skipping forward or backward 
# as requested.

procedure gm_posmc (in, file_pos, pcounter, mc_index, nskip)

int	in			# File descriptor of input file
long	file_pos		# Current position in file
int	pcounter		# Plot number just plotted upon entering
long	mc_index[ARB]		# Accumulated index of mc plots
int	nskip			# Requested nplots to skip 

int	desired_plot, i, nchars_read, pcounter_in, fpos_in
long	desired_position
int	gm_findnextplot()
errchk	seek, gm_findnextplot

begin
	# Save original plot number counter
	pcounter_in = pcounter
	fpos_in = file_pos

	# Skipping backwards
	if (nskip < 0) {
	    if (in == STDIN) {
	        call eprintf ("Cannot skip backwards on STDIN\n")
		return
	    }

	    if (abs (nskip) > pcounter) {
	        call eprintf ("At beginning of file\n")
	        call seek (in, BOFL)
		file_pos = 1
		pcounter = 0
		return
	    }

	    # Rewind mc to desired position and change the pcounter.  The
	    # calling program will redetermine the starting position as 
	    # before.

	    desired_plot = pcounter - abs (nskip) + 1
	    desired_position = mc_index[desired_plot]
	    call seek (in, desired_position)
	    pcounter = desired_plot - 1
	    file_pos = desired_position

	} else {
	    # Skipping forward - updating the index along the way.

	    desired_plot = pcounter_in + nskip + 1

	    do i = 1, nskip {
		nchars_read = gm_findnextplot (in)
		if (nchars_read == EOF) {
		    call eprintf ("Only %d plots left - position unchanged\n")
			call pargi (i - 1)
		    pcounter = pcounter_in
		    file_pos = fpos_in
		    call seek (in, fpos_in)
		    return
		}

		pcounter = pcounter + 1
		file_pos = file_pos + nchars_read
		mc_index[pcounter+1] = file_pos
	    }

	    # Reset pcounter; no need to seek to desired position as
	    # you are already there.
	    pcounter = desired_plot - 1
	}
end


# GM_FINDNEXTPLOT  -- read until the start of the next plot in the metacode
# file, returning the number of chars read to get there.

int procedure gm_findnextplot (in)

int	in
pointer	gki
int	nchars_read, opcode, plot_length
int	gm_read_next_instruction()

begin
	plot_length = 0
	repeat {
	    if (gm_read_next_instruction (in, gki, nchars_read) == EOF)
 	        return (EOF)
	    
	    plot_length = plot_length + nchars_read
	    opcode = I_OPCODE (gki)

	    if ((opcode == GKI_OPENWS && WS_MODE(gki) == NEW_FILE) || 
		(opcode == GKI_CLEAR))

	        # New frame encountered, terminating previous plot.
	        return (plot_length)
	}
end


# GM_READ_NEXT_INSTRUCTION -- read the next instruction from the input
# stream, returning a buffer pointer to the instruction and the number of
# chars read to get to this position.  This is a modified version of 
# gki_fetch_next_instruction, in that the total number of chars read
# (including partial and botched instructions) is returned as a procedure
# argument.

int procedure gm_read_next_instruction (fd, instruction, nchars_total)

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


# Test for finding the unitary transformation WCS
define	(USERSET_W, (WCS_WX1($1) > EPSILON)||(abs(1. - WCS_WX2($1)) >EPSILON) ||
    (WCS_WY1($1) > EPSILON) || (abs(1. - WCS_WY2($1)) > EPSILON))

define (USERSET_V, (WCS_SX1($1) > EPSILON)| (abs(1. - WCS_SX2($1)) > EPSILON) ||
    (WCS_SY1($1) > EPSILON) || (abs(1. - WCS_SY2($1)) > EPSILON))

# GM_SETWCS -- Find WCS window and viewport information from SETWCS 
# instruction.  This procedure gets all active wcs from the structure.
# The WCS is transformed in place.

procedure gm_setwcs (gki, frame_wcs, nwcs_cnt)

short	gki[ARB]	# GKI_SETWCS instruction
pointer	frame_wcs	# Pointer to accumulating SETWCS instruction
int	nwcs_cnt	# Number of SETWCS instructions encountered

int	nwords,	i, nwcs, temp, nwcs_in
real	xy_pairs[NPAIRS * 2]
pointer	sp, wcs_temp, w, ow

int	rotate
real	x1, y1, xcen, ycen, xscale, yscale, cos_angle, sin_angle
common	/gm_tform/ x1, y1, xcen, ycen, xscale, yscale, cos_angle, sin_angle, 
         rotate

errchk	amovi, gm_vtransr

begin
	call smark (sp)
	call salloc (wcs_temp, LEN_WCSARRAY, TY_STRUCT)

	nwcs_in = nwcs_cnt
	nwords = gki[GKI_SETWCS_N]
	nwcs = nwords * SZ_SHORT / SZ_STRUCT / LEN_WCS

	if (nwcs > 1) {
	    call amovi (gki[GKI_SETWCS_WCS], Memi[wcs_temp], nwcs * LEN_WCS)

	    do i = 1, nwcs {
		w = ((i - 1) * LEN_WCS) + wcs_temp

		if (USERSET_W(w) || USERSET_V(w)) {
		    # Got a valid WCS - increment counter and calculate 
		    # pointer into output frame_wcs array.
		    nwcs_cnt = nwcs_cnt + 1
		    ow = ((nwcs_cnt - 1) * LEN_WCS) + frame_wcs

		    # Now to do the transformation:
		    xy_pairs[1] = WCS_SX1(w)
		    xy_pairs[2] = WCS_SY1(w)
		    xy_pairs[3] = WCS_SX2(w)
		    xy_pairs[4] = WCS_SY2(w)

		    call gm_vtransr (xy_pairs, NPAIRS)

		    # Set those fields that have changed, viewport coordinates.

		    WCS_SX1(ow) = xy_pairs[1]
		    WCS_SY1(ow) = xy_pairs[2]
		    WCS_SX2(ow) = xy_pairs[3]
		    WCS_SY2(ow) = xy_pairs[4]

		    # X and Y transformations have changed if plot is rotated.
		    if (rotate == YES) {
			temp = WCS_XTRAN(w)
			WCS_XTRAN(ow) = WCS_YTRAN(w)
			WCS_YTRAN(ow) = temp
			xy_pairs[1] = WCS_WX1(w)
			xy_pairs[2] = WCS_WX2(w)
			xy_pairs[3] = WCS_WY1(w)
			xy_pairs[4] = WCS_WY2(w)
		        WCS_WX1 (ow) = xy_pairs[3]
		        WCS_WX2 (ow) = xy_pairs[4]
		        WCS_WY1 (ow) = xy_pairs[1]
		        WCS_WY2 (ow) = xy_pairs[2]
		    } else {
			WCS_XTRAN(ow) = WCS_XTRAN(w)
			WCS_YTRAN(ow) = WCS_YTRAN(w)
		        WCS_WX1 (ow) = WCS_WX1(w)
		        WCS_WX2 (ow) = WCS_WX2(w)
		        WCS_WY1 (ow) = WCS_WY1(w)
		        WCS_WY2 (ow) = WCS_WY2(w)
		    }

		    WCS_CLIP(ow) = WCS_CLIP(w)
		}
	    }
	}

	if (nwcs_in == nwcs_cnt) {
	    # No user WCS were used - output the default WCS 0, scaled and
	    # possibly rotated.

	    nwcs_cnt = nwcs_cnt + 1
	    ow = ((nwcs_cnt - 1) * LEN_WCS) + frame_wcs

	    xy_pairs[1] = 0.0
	    xy_pairs[2] = 0.0
	    xy_pairs[3] = 1.0
	    xy_pairs[4] = 1.0

	    call gm_vtransr (xy_pairs, NPAIRS)

	    # X and Y transformations have changed if plot is rotated.
	    if (rotate == YES) {
	        WCS_SX1 (ow) = xy_pairs[3]
	        WCS_SX2 (ow) = xy_pairs[4]
	        WCS_SY1 (ow) = xy_pairs[1]
	        WCS_SY2 (ow) = xy_pairs[2]
		WCS_WX1 (ow) = 0.0
		WCS_WX2 (ow) = 1.0
		WCS_WY1 (ow) = 1.0
		WCS_WY2 (ow) = 0.0
	    } else {
	        WCS_SX1 (ow) = xy_pairs[1]
	        WCS_SX2 (ow) = xy_pairs[3]
	        WCS_SY1 (ow) = xy_pairs[2]
	        WCS_SY2 (ow) = xy_pairs[4]
		WCS_WX1 (ow) = 0.0
		WCS_WX2 (ow) = 1.0
		WCS_WY1 (ow) = 0.0
		WCS_WY2 (ow) = 1.0
	    }

	    WCS_XTRAN(ow) = LINEAR
	    WCS_YTRAN(ow) = LINEAR
	    WCS_CLIP(ow) = YES
	}

	call sfree (sp)
end


# GM_INITWCS -- initialize the WCS structure to default values.
procedure gm_initwcs (wcs)

pointer	wcs	# Pointer to wcs structure
pointer	w
int	i

begin
	# Initialize the WCS to NDC coordinates.
	do i = 1, MAX_WCS {
	    w = ((i - 1) * LEN_WCS) + wcs
	    WCS_WX1(w) = 0.0
	    WCS_WX2(w) = 1.0
	    WCS_WY1(w) = 0.0
	    WCS_WY2(w) = 1.0
	    WCS_SX1(w) = 0.0
	    WCS_SX2(w) = 1.0
	    WCS_SY1(w) = 0.0
	    WCS_SY2(w) = 1.0
	    WCS_XTRAN(w) = LINEAR
	    WCS_YTRAN(w) = LINEAR
	    WCS_CLIP(w) = YES
	}
end


# GM_WRITEMC -- Output transformed metacode.  Action taken depends on
# individual metacode instruction.  Any instruction with (x,y) coordinates
# gets transformed; txset instruction gets rewritten; other instructions
# are simply written to graphics stream.  Metacode is rewritten in place.

int procedure gm_writemc (fd, gki, frame_wcs, nwcs)

int	fd			# File descriptor for graphics stream
short	gki[ARB]		# Metacode instruction
pointer	frame_wcs		# Pointer to accumulating SETWCS instruction
int	nwcs			# Counter for number of WCS instructions found

int	npairs, opcode
errchk gm_txset, gm_vtrans, gki_write, gm_setwcs

begin
	opcode = gki[GKI_HDR_OPCODE]
	switch (opcode) {

	case GKI_SETWCS:
	if (nwcs < MAX_WCS)
	    iferr (call gm_setwcs (gki, frame_wcs, nwcs))
		call erract (EA_WARN)

	case GKI_CLEAR:
	#This marks start of next metacode frame
	return (NEW_FRAME)

	case GKI_OPENWS:
	if (gki[GKI_OPENWS_M] == NEW_FILE) 
	    # This also marks the start of a new metacode frame
	    return (NEW_FRAME)
	    
	case GKI_CLOSEWS:
	# Just absorb these instructions - don't copy them
	    ;

	case GKI_POLYLINE:
	npairs = gki[GKI_POLYLINE_N]
	call gm_vtrans (gki[GKI_POLYLINE_P], npairs)
	call gki_write (fd, gki)

	case GKI_TXSET:
	# Several instruction fields have to be changed
	call gm_txset (gki)
	call gki_write (fd, gki)

	case GKI_POLYMARKER:
	npairs = gki[GKI_POLYMARKER_N]
	call gm_vtrans (gki[GKI_POLYMARKER_P], npairs)
	call gki_write (fd, gki)

	case GKI_TEXT:
	npairs = 1
	call gm_vtrans (gki[GKI_TEXT_P], npairs)
	call gki_write (fd, gki)

	case GKI_FILLAREA:
	npairs = gki[GKI_FILLAREA_N]
	call gm_vtrans (gki[GKI_FILLAREA_P], npairs)
	call gki_write (fd, gki)

	case GKI_PUTCELLARRAY:
	# Do both lower left and upper right corners
	npairs = 1
	call gm_vtrans (gki[GKI_PUTCELLARRAY_LL], npairs)
	call gm_vtrans (gki[GKI_PUTCELLARRAY_UR], npairs)

	call gki_write (fd, gki)

	default:
	call gki_write (fd, gki)
	}

	return (OK)
end


# GM_GETVP -- Calculate cornerpoints for the individual viewports on the page.  

procedure gm_getvp (vp, nx, ny, fill)

pointer	vp		# Pointer to array of viewport coordinates
int	nx		# Number of plots in x direction
int	ny		# Number of plots in y direction
bool	fill		# Fill viewport or preserve aspect ratio

int	i, j, plotnumber
real	x_sep, y_sep, x_ext, y_ext, x_center, y_center

begin
	if (fill) {
	    # x and y dimensions of plot viewports calculated independently.
	    x_sep = 1.0 / real (nx)
	    y_sep = 1.0 / real (ny)
	    x_ext = x_sep
	    y_ext = y_sep

	} else {
	    # Plot viewports are equal in NDC space for both x and y
	    x_sep = 1.0 / real (nx)
	    y_sep = 1.0 / real (ny)
	    x_ext = min (1.0 / real (nx), 1.0 / real (ny))
	    y_ext = min (1.0 / real (nx), 1.0 / real (ny))
	}

	# Find NDC coordinates of the page full of viewports

	plotnumber = 1
	do i = 1, nx {
	    x_center = 0.5 * x_sep + (i - 1) * x_sep

	    do j = 1, ny {
		y_center = 1.0 - (0.5 * y_sep + (j - 1) * y_sep)

		# Calculate x1, x2, y1, y2 for each viewport
		Memr[vp+plotnumber-1]   = x_center - (0.5 * x_ext)
		Memr[vp+plotnumber]     = x_center + (0.5 * x_ext)
		Memr[vp+plotnumber+1]   = y_center - (0.5 * y_ext)
		Memr[vp+plotnumber+2]   = y_center + (0.5 * y_ext)

		plotnumber = plotnumber + 4
	    }
	}
end


# GM_TRINIT -- Initialize transformation variables.  Called once per output
# plot - once per transformation.

procedure gm_trinit (viewport, rot_plot)

real	viewport[4]		# Corner points of plotting viewport
bool	rot_plot		# Rotate plots (y/n?)

int	rotate
real	x1, y1, xcen, ycen, xscale, yscale, cos_angle, sin_angle
common	/gm_tform/ x1, y1, xcen, ycen, xscale, yscale, cos_angle, sin_angle,
         rotate

begin
	# Calculate and store sine, cosine of rotation angle
	if (! rot_plot) {
	    cos_angle = 1.0
	    sin_angle = 0.0
	    rotate = NO
	} else {
	    cos_angle = 0.0
	    sin_angle = 1.0
	    rotate = YES
	}

	# Calculate origin, center and scale.
        x1 = viewport[1] * GKI_MAXNDC
        y1 = viewport[3] * GKI_MAXNDC
	xcen = (viewport[2] + viewport[1]) * 0.5 * GKI_MAXNDC
	ycen = (viewport[4] + viewport[3]) * 0.5 * GKI_MAXNDC
        xscale = viewport[2] - viewport[1]
        yscale = viewport[4] - viewport[3]
end


# GM_TXSET -- Rewrite the text set instruction.  The fields that
# need to be changed are the tx_size, chup vector and both the
# vertical and horizontal justification.  The instruction is rewritten
# in place.

procedure gm_txset (instruction)

short	instruction [ARB]		# Metacode instruction

short	temp, sz, hj, vj

int	rotate
real	x1, y1, xcen, ycen, xscale, yscale, cos_angle, sin_angle
common	/gm_tform/ x1, y1, xcen, ycen, xscale, yscale, cos_angle, sin_angle, 
         rotate

begin
	# First convert size, which is stored as NDC * 100
	sz = instruction[GKI_TXSET_SZ]
	temp = short ((real (sz) / 100.  * min (xscale, yscale)) * 100.)
	instruction [GKI_TXSET_SZ] = temp

	if (rotate == YES) {
	    # Axes have been rotated by 90 degrees.  Change character up vector.
	    instruction[GKI_TXSET_UP] = instruction[GKI_TXSET_UP] - 90

	    # Change vertical and horizontal text justification
	    hj = instruction[GKI_TXSET_HJ]
	    vj = instruction[GKI_TXSET_VJ]

	    switch (hj) {
	    case GT_LEFT:
		instruction[GKI_TXSET_VJ] = GT_TOP
	    case GT_RIGHT:
	        instruction[GKI_TXSET_VJ] = GT_BOTTOM
	    default:
	        instruction[GKI_TXSET_VJ] = hj
	    }

	    switch (vj) {
	    case GT_TOP:
		instruction[GKI_TXSET_HJ] = GT_RIGHT
	    case GT_BOTTOM:
	        instruction[GKI_TXSET_HJ] = GT_LEFT
	    default:
	        instruction[GKI_TXSET_HJ] = vj
	    }
	}
end


# GM_VTRANS -- transform a vector of coordinate pairs.  The transformation
# is done in place.

procedure gm_vtrans (xy_pairs, npairs)

short	xy_pairs[ARB]		# Metacode instruction coordinate pairs
int	npairs			# Number of coordinate pairs

int	i
long	xt, yt
real	xtemp, ytemp

int	rotate
real	x1, y1, xcen, ycen, xscale, yscale, cos_angle, sin_angle
common	/gm_tform/ x1, y1, xcen, ycen, xscale, yscale, cos_angle, sin_angle,
         rotate

begin
	do i = 1, 2 * npairs, 2 {
	    xtemp = real (xy_pairs[i]) * xscale + x1
	    ytemp = real (xy_pairs[i+1]) * yscale + y1

	    if (rotate == NO) {
	        xt = xtemp
	        yt = ytemp

	    } else {
	        # Rotate about center, making sure transformed coordinates
	        # are in NDC bounds.

	        xt = max (0, min (int(((ytemp - ycen) * xscale/yscale) + xcen), 
		    GKI_MAXNDC))
	        yt = max (0, min (int(((xcen - xtemp) * yscale/xscale) + ycen), 
		    GKI_MAXNDC))
	    }

	    xy_pairs[i] = short (xt)
	    xy_pairs[i+1] = short (yt)
	}
end


# GM_VTRANSR -- transform a vector of coordinate pairs.  The transformation
# is done in place.  To be used with real format xy.

procedure gm_vtransr (xy_pairs, npairs)

real	xy_pairs[ARB]		# Metacode binary coordinate pairs (e.g., WCS)
int	npairs			# Number of coordinate pairs

int	i
real	xt, yt, xtemp, ytemp

int	rotate
real	x1, y1, xcen, ycen, xscale, yscale, cos_angle, sin_angle
common	/gm_tform/ x1, y1, xcen, ycen, xscale, yscale, cos_angle, sin_angle,
         rotate

begin
	do i = 1, 2 * npairs, 2 {
	    xtemp = xy_pairs[i] * real (GKI_MAXNDC) * xscale + x1
	    ytemp = xy_pairs[i+1] * real (GKI_MAXNDC) * yscale + y1

	    if (rotate == NO) {
	        xt = xtemp
	        yt = ytemp

	    } else {
	        # Rotate about center, making sure transformed coordinates
	        # are in bounds.

	        xt = max (0., min ((((ytemp-ycen) * xscale/yscale) + xcen), 
		    real (GKI_MAXNDC)))
	        yt = max (0., min ((((xcen-xtemp) * yscale/xscale) + ycen), 
		    real (GKI_MAXNDC)))
		    
	    }

	    # Convert from GKI coordinates to NDC before returning.
	    xy_pairs[i] = xt / GKI_MAXNDC
	    xy_pairs[i+1] = yt / GKI_MAXNDC
	}
end


# GM_HELP -- Print interactive help for gkimosaic.  The workstation must
# be deactivated, then the file paged and the workstation reactivated.

procedure gm_help (out, file)

int	out				# File descriptor of graphics stream
char	file[ARB]			# File to be printed

begin
	call gki_flush (out)
	call gki_deactivatewcs (out, AW_CLEAR)
	call pagefile (file, PROMPT)
	call flush (STDOUT)
	call gki_reactivatewcs (out, AW_PAUSE)
end
