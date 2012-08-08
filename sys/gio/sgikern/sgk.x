# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include <chars.h>
include <gki.h>
include "sgk.h"

.help sgk
.nf ---------------------------------------------------------------------------
SGK -- Simple graphics device interface.  The purpose of this interface is
to provide a simple means for interfacing new plotter devices to IRAF/GIO.
The interface works by writing a binary metacode file and then disposing of
it by issuing a command to the host system.

       g_out = sgk_open (device, tty)		# device open
	      sgk_close (g_out)			# device close
	      sgk_flush (g_out)			# flush output

	      sgk_frame (g_out)			# start a new frame
	       sgk_move (g_out, x, y)		# move to (x,y)
	       sgk_draw (g_out, x, y)		# draw a vector to (x,y)
	  sgk_linewidth (g_out, width)		# set line width (>=1)

The procedures comprising the top end of the SGK interface are summarized
above and the code is included in this file.  These procedures could be
rewritten by the user to talk directly to a graphics device if desired,
although the metacode file interface is likely to be simpler in most cases.

The SGK kernel can produce either metacode or bitmap output.  Metacode output
is normally preferred for intelligent plotters and for pen plotters.  Bitmap
output is normally preferred for raster plotters.  The type of output file
to be generated is selected by the graphcap entry for an SGI/SGK device.

The METACODE FORMAT written by the SGK interface is a sequence of 16 bit integer
words containing binary opcodes and data.  The metacode is extremely simple,
consisting of only two drawing instructions (pen up move and pen down draw),
a frame instruction, and an optional set line width instruction.  All text is
rendered into vectors by the SGI kernel hence there are no text drawing
instructions.  The SGK metacode instruction formats are summarized below.

     opcode / data words 

          1  0  0               # frame instruction
          2  X  Y               # move to (x,y)
          3  X  Y               # draw to (x,y)
          4  W  0               # set line width (>= 1, 1=normal, 2=bold)

All opcodes and data words are 16 bit positive integers encoded in the machine
independent MII format, i.e., most significant byte first.  Only 15 bits of
each 16 bit word are actually used.  Coordinates are specified in the range 0
to 32767.  All instructions are zero padded to 3 words to simplify metacode
translation programs.

The BITMAP FORMAT written by the SGK is even simpler than the metacode format.
Output consists of a binary raster file containing one or more bitmaps with no
embedded header information.  All bitmaps in a raster file are of the same
size.  The size is specified in the graphcap entry for the device and may be
passed to the host dispose task on the foreign task command line if desired.
Page offsets may also be passed on the command line, e.g., to position the
plot on the plotter page.

The following graphcap fields apply to both metacode and bitmap devices.

	DD	host command to dispose of metacode file ($F)
	DB	have the kernel print debug messages during execution
	RM	boolean; if present, SGK will delete metacode file
	MF	multiframe count (max frames per job)
	NF	store each frame in a new file (rather than all in one file)
	RO	rotate plot (swap x and y)
	YF	y-flip plot (flip y axis) (done after rotate)

The following additional fields are defined for bitmap devices.

	BI	boolean; presence indicates a bitmapped or raster device
	LO	width in device pixels of a line of size 1.0
	LS	difference in device pixels between line sizes
	PX	physical x size (linelen) of bitmap as stored in memory, bits
	PY	physical y size of bitmap, i.e., number of lines in bitmap
	XO,YO	origin of plotting window in device pixels
	XW,YW	width of plotting window in device pixels
	NB	number of bits to be set in each 8 bit byte output
	BF	bit-flip each byte in bitmap (easier here than later)
	BS	byte swap the bitmap when output (swap every two bytes)
	WS	word swap the bitmap when output (swap every four bytes)

The multiframe count (MF) limits the number of frames per job, where a job
refers to the dispose command submitted to the host to process the frames.
If the new file flag (NF) is absent, all frames will be stored in the same
physical file (this holds for both metacode and bitmap frames).  If the new
file flag (NF) is set, each frame will be stored in a separate file, with
the N files having the names $F.1, $F.2, ... $F.N, where $F is the unique
(root) filename generated from the template given in the DD string.  The $F
is replaced by the root filename, rather than by a list of all the filenames,
to keep the OS command to a reasonable length and to permit the use of host
file templates to perform operate upon the full set of files (and to avoid
having to choose between spaces and commas to delimit the filenames).
For example, if MF=8 and NF=yes, then "$F.[1-8]" will match the file set
on a UNIX host.  The template "$F.*" is less precise but would also work.

The output raster will consist of PY lines each of length PX bits.  If PX is
chosen to be a multiple of 8, there will be PX/8 bytes per line of the output
raster.  Note that the values of PX and PY are arbitrary and should be chosen
to simplify the code of the translator and maximize efficiency.  In particular,
PX and PY do not in general define the maximum physical resolution of the
device, although if NB=8 the value of PX will typically approximate the
physical resolution in X.  If there are multiple bitmap frames per file,
each frame will occupy an integral number of SPP char units of storage in the
output file, with the values of any extra bits at the end of the bitmap being
undefined (a char is 16 bits on most IRAF host machines).

The plot will be rasterized in a logical window XW one-bit pixels wide and YW
pixels high.  The first YO lines of the output raster will be zero, with the
plotting window beginning at line YO+1.  The first XO bits of each output line
will be zeroed, with the plotting window beginning at bit XO+1.  The bytes in
each output line may be bit-flipped if desired, and all of the bits in each
output byte need not be used for pixel data.  If the bit packing factor NB is
set to 8 the plotting window will map into XW bits of storage of each output
line.  If fewer than 8 bits are used in each output byte more than XW physical
bits of storage will be used, e.g., if NB=4, XW*2 bits of storage are required
for a line of the plotting window.  The unused bits are set to zero.  The
translator can later "or" a mask into the zeroed bits, flip the data bits,
or perform any other bytewise operation using simple lookup table mapping
techniques.
.endhelp ----------------------------------------------------------------------

# NOTE -- The mf_physbit lookup table, used to map logical screen bits into
# physical bits in the bitmap (for NB != 8) is equivalenced to the mf_obuf
# array which is not otherwise used for bitmap devices.  The length of the
# mf_obuf array must therefore be >= PX.

define	mf_physbit	mf_obuf		# union these two arrays [[[NOTE]]]
define	BPW		NBITS_INT	# nbits in an integer
define	LEN_FBUF	(2550*3300/BPW)	# max size bitmap / frame buffer
define	LEN_OBUF	3300		# nwords in output buffer
define	SZ_DDSTR	256		# max size graphcap.DD
define	SZ_OSCMD	256		# OS dispose command from graphcap.DD


# SGK_OPEN -- Open the metacode file.  Parse the DD string from the graphcap
# entry for the device to get the file template and OS dispose command.
# Generate a unique file name and open the metacode file as a NEW_FILE.
# Save the dispose command for later.

int procedure sgk_open (device, tty)

char	device[ARB]		# device name [NOT USED]
pointer	tty			# pointer to graphcap descriptor

char	cap[2]
int	len_nodeprefix, byte, off, op, i, j
pointer	sp, raw_ddstr, ddstr, devname, spool, fname, tempfn, val, ip

bool	ttygetb()
real	ttygetr()
int	open(), ttygets(), ttygeti(), gstrcpy(), shifti()
errchk	open, ttygets, ttygeti, ttygetb
include	"sgk.com"

begin
	call smark (sp)
	call salloc (raw_ddstr, SZ_DDSTR, TY_CHAR)
	call salloc (ddstr, SZ_DDSTR, TY_CHAR)
	call salloc (devname, SZ_FNAME, TY_CHAR)
	call salloc (spool, SZ_FNAME, TY_CHAR)
	call salloc (fname, SZ_PATHNAME, TY_CHAR)
	call salloc (tempfn, SZ_PATHNAME, TY_CHAR)
	call salloc (val, SZ_FNAME, TY_CHAR)

	# The DB flag may be set in the graphcap entry for an SGI device to
	# print debug messages during execution.

	mf_debug = ttygetb (tty, "DB")

	# The DD string is used to pass device dependent information to the
	# graphics device driver.

	if (ttygets (tty, "DD", Memc[raw_ddstr], SZ_DDSTR) <= 0)
	    call error (1, "sgikern: missing DD parameter in graphcap")

	# Expand any $(XX) graphcap parameter references in the DD string.
	op = ddstr
	for (ip=raw_ddstr;  Memc[ip] != EOS;  ip=ip+1)
	    if (Memc[ip] == '$' && Memc[ip+1] == '(' && Memc[ip-1] != '\\') {
		# Graphcap parameter substitution.
		call strcpy (Memc[ip+2], cap, 2)
		if (ttygets (tty, cap, Memc[val], SZ_FNAME) <= 0) {
		    call eprintf ("Warning: graphcap field `%s' not found\n")
			call pargstr (cap)
		} else {
		    for (off=val;  Memc[off] == '#';  off=off+1)
			;
		    for (;  Memc[off] != EOS;  off=off+1) {
			Memc[op] = Memc[off]
			op = op + 1
		    }
		}
		ip = ip + 4
		    
	    } else {
		# Ordinary character.
		Memc[op] = Memc[ip]
		op = op + 1
	    }
	Memc[op] = EOS

	# Parse the DD string into the node/device name, temp file name,
	# and host dispose command.

	# Get node and device name (e.g., "node!device,...").
	len_nodeprefix = 0
	ip = ddstr
	for (op=devname;  Memc[ip] != EOS;  ip=ip+1)
	    if (Memc[ip] == ',') {
		if (Memc[ip-1] == '\\') {
		    Memc[op-1] = ','
		    ip = ip - 1
		} else {
		    ip = ip + 1
		    break
		}
	    } else {
		if (Memc[ip] == FNNODE_CHAR)
		    len_nodeprefix = op - devname + 1
		Memc[op] = Memc[ip]
		op = op + 1
	    }
	Memc[op] = EOS

	# Get spoolfile root name.
	op = spool + gstrcpy (Memc[devname], Memc[spool], len_nodeprefix)
	for (;  Memc[ip] != EOS;  ip=ip+1)
	    if (Memc[ip] == ',') {
		if (Memc[ip-1] == '\\') {
		    Memc[op-1] = ','
		    ip = ip - 1
		} else {
		    ip = ip + 1
		    break
		}
	    } else {
		Memc[op] = Memc[ip]
		op = op + 1
	    }
	Memc[op] = EOS

	# Get OS pathname of spoofile.
	call mktemp (Memc[spool], Memc[tempfn], SZ_PATHNAME)
	call fmapfn (Memc[tempfn], mf_fname, SZ_PATHNAME)
	call strupk (mf_fname, mf_fname, SZ_PATHNAME)

	# Get pathname of spoolfile on the remote node.  The call to
	# ki_fmapfn() is currently necessary to translate the filename for
	# the remote node, but may be replaced by the usual fmapfn() in a
	# future version of the kernel interface.

	call ki_fmapfn (Memc[tempfn], Memc[fname], SZ_PATHNAME)
	call strupk (Memc[fname], Memc[fname], SZ_PATHNAME)

	if (mf_debug) {
	    call eprintf ("sgk: open device %s, outfile = %s\n")
		call pargstr (Memc[devname])
		call pargstr (mf_fname)
	}

	# Copy OS command for disposing of metacode file into common, replacing
	# all $F sequences in the command by the OS pathname of the spool file.

	op = gstrcpy (Memc[devname], mf_dispose, len_nodeprefix) + 1

	for (;  Memc[ip] != EOS;  ip=ip+1)
	    if (Memc[ip] == '$' && Memc[ip-1] == '\\') {
		# Escape a $.
		mf_dispose[op-1] = '$'

	    } else if (Memc[ip] == '$' && Memc[ip+1] == 'F') {
		# Filename substitution.
		for (i=fname;  Memc[i] != EOS;  i=i+1) {
		    mf_dispose[op] = Memc[i]
		    op = op + 1
		}
		ip = ip + 1

	    } else {
		# Ordinary character.
		mf_dispose[op] = Memc[ip]
		op = op + 1
	    }

	mf_dispose[op] = EOS

	# Remove (delete) metacode file after issuing OS dispose command?
	mf_delete = ttygetb (tty, "RM")

	# Store each frame in a new file?
	mf_oneperfile = ttygetb (tty, "NF")

	mf_update = false
	mf_frame = 1

	# Open a new metacode file.
	if (mf_oneperfile)
	    call sgk_mkfname (mf_fname, mf_frame, Memc[fname], SZ_FNAME)
	else
	    call strcpy (mf_fname, Memc[fname], SZ_FNAME)

	if (mf_debug) {
	    call eprintf ("sgk: open frame %2d, outfile = %s\n")
		call pargi (mf_frame)
		call pargstr (Memc[fname])
	}
	mf_fd = open (Memc[fname], NEW_FILE, BINARY_FILE)

	# Rotate plot (swap x,y)?  Y-flip plot?
	mf_rotate = ttygetb (tty, "RO")
	mf_yflip  = ttygetb (tty, "YF")

	# Raster (bitmap) or metacode device?
	mf_bitmap = ttygetb (tty, "BI")

	if (mf_bitmap) {
	    # Bitmap output; initialize bitmap parameters.

	    mf_pxsize   = ttygeti (tty, "PX")
	    mf_pysize   = ttygeti (tty, "PY")
	    mf_xorigin  = ttygeti (tty, "XO")
	    mf_yorigin  = ttygeti (tty, "YO")
	    mf_wxsize   = ttygeti (tty, "XW")
	    mf_wysize   = ttygeti (tty, "YW")
	    mf_nbpb	= ttygeti (tty, "NB")
	    mf_swap2	= ttygetb (tty, "BS")
	    mf_swap4	= ttygetb (tty, "WS")

	    mf_lworigin = max (1, ttygeti (tty, "LO"))
	    mf_lwslope  = ttygetr (tty, "LS")
	    mf_lenframe = (mf_pxsize * mf_pysize + BPW-1) / BPW

	    if (mf_wxsize == 0)
		mf_wxsize = mf_pxsize - mf_xorigin
	    if (mf_wysize == 0)
		mf_wysize = mf_pysize - mf_yorigin
	    if (mf_nbpb == 0)
		mf_nbpb = NBITS_BYTE

	    mf_linewidth = mf_lworigin
	    mf_cx = 0
	    mf_cy = 0

	    mf_xmin = mf_xorigin
	    mf_ymin = mf_yorigin
	    mf_xmax = mf_xmin + mf_wxsize - 1
	    mf_ymax = mf_ymin + mf_wysize - 1

	    mf_xscale = real(mf_wxsize) / real(GKI_MAXNDC)
	    mf_yscale = real(mf_wysize) / real(GKI_MAXNDC)

	    if (mf_lenframe > LEN_FBUF)
		call error (1, "sgikern: bitmap too large")

	    # Initialize the bit mask table.  If it is necessary to bit-flip
	    # bytes in the bitmap, we can do that here by flipping each byte
	    # of the word mask.  Bit flipping can be done during rasterization
	    # at no additional cost, but is an expensive operation if done
	    # later with a filter.

	    if (ttygetb (tty, "BF")) {
		do j = 1, (BPW/NBITS_BYTE)
		    do i = 1, NBITS_BYTE {
			off = (j - 1) * NBITS_BYTE
			mf_bitmask[off+i] = shifti (1, off + NBITS_BYTE - i)
		    }
	    } else {
		do i = 1, BPW
		    mf_bitmask[i] = shifti (1, i - 1)
	    }

	    # Initialize the bit offset lookup table.  This gives the physical
	    # x-offset into the lookup table of each addressable x-coordinate
	    # on the device.  If NB is NBITS_BYTE the mapping is one-to-one.
	    # Note that the table contains zero-indexed bit offsets.

	    do i = 1, mf_pxsize {
		byte = (i - 1) / mf_nbpb
		mf_physbit[i] = min (mf_pxsize,
		    byte * NBITS_BYTE + (i - (byte * mf_nbpb))) - 1
	    }

	    if (mf_debug) {
		call eprintf ("bitmap [%d,%d] origin=[%d,%d] wsize=[%d,%d]\n")
		    call pargi (mf_pxsize);  call pargi (mf_pysize)
		    call pargi (mf_xorigin); call pargi (mf_yorigin)
		    call pargi (mf_wxsize);  call pargi (mf_wysize)
	    }

	} else {
	    # Metacode output; initialize the metacode output buffer.
	    mf_op = 1
	    if (mf_debug)
		call eprintf ("metafile device\n")
	}

	call sfree (sp)
	return (mf_fd)
end


# SGK_CLOSE -- Close the metacode spool file and dispose of it to a host system
# metacode translation task.  Delete the spool file when the OS command
# completes, unless it has already been deleted by the task run.

procedure sgk_close (fd)

int	fd			# output stream [NOT USED]

int	i
pointer	sp, fname
int	oscmd()
errchk	sgk_flush, close, oscmd
include	"sgk.com"

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)

	if (mf_debug)
	    call eprintf ("close device\n")

	if (mf_bitmap)
	    call sgk_frame (mf_fd)
	else
	    call sgk_flush (mf_fd)

	if (mf_debug) {
	    call eprintf ("dispose: %s\n")
		call pargstr (mf_dispose)
	}

	if (mf_fd != NULL) {
	    call close (mf_fd)
	    mf_fd = NULL
	}

	# Send the dispose command to the host system.
	if (mf_dispose[1] != EOS)
	    if (oscmd (mf_dispose, "", "", "") != OK)
		call eprintf ("Warning: SGK graphics output dispose error\n")

	# Delete the metacode or raster file if so indicated in the graphcap
	# entry for the device.

	if (mf_delete) {
	    if (mf_debug) {
		call eprintf ("delete metafile %s\n")
		    call pargstr (mf_fname)
	    }
	    if (mf_oneperfile) {
		do i = 1, mf_frame {
		    call sgk_mkfname (mf_fname, i, Memc[fname], SZ_FNAME)
		    iferr (call delete (Memc[fname]))
			;
		}
	    } else iferr (call delete (mf_fname))
		;
	}

	call sfree (sp)
end


# SGK_FLUSH -- Flush any buffered metacode output.

procedure sgk_flush (fd)

int	fd			# output stream [NOT USED]
include	"sgk.com"

begin
	if (!mf_bitmap && mf_op > 1) {
	    if (mf_debug)
		call eprintf ("flush graphics output\n")
	    call miiwrites (mf_fd, mf_obuf, mf_op-1)
	    mf_op = 1
	}

	if (mf_fd != NULL)
	    call flush (mf_fd)
end


# SGK_FRAME -- Output a frame advance instruction.

procedure sgk_frame (fd)

int	fd			# output stream [NOT USED]
include	"sgk.com"

begin
	# Ignore frame commands if frame is empty.
	if (!mf_update)
	    return

	if (mf_debug)
	    call eprintf ("start a new frame\n")

	if (mf_bitmap) {
	    # Write the bitmap to the output raster-file.

	    if (mf_swap2)
		call bswap2 (mf_fbuf, 1, mf_fbuf, 1,
		    mf_lenframe * SZ_INT * SZB_CHAR)
	    if (mf_swap4)
		call bswap4 (mf_fbuf, 1, mf_fbuf, 1,
		    mf_lenframe * SZ_INT * SZB_CHAR)

	    call write (mf_fd, mf_fbuf, mf_lenframe * SZ_INT)

	} else {
	    # Write the SGI frame instruction to the output mcode-file.

	    if (mf_op + SGK_LENMCI > LEN_OBUF) {
		call miiwrites (mf_fd, mf_obuf, mf_op-1)
		mf_op = 1
	    }

	    mf_obuf[mf_op] = SGK_FRAME
	    mf_obuf[mf_op+1] = 0
	    mf_obuf[mf_op+2] = 0
	    mf_op = mf_op + SGK_LENMCI
	}

	mf_frame = mf_frame + 1
	mf_update = false
end


# SGK_MOVE -- Output a pen move instruction.

procedure sgk_move (fd, x, y)

int	fd			# output stream [NOT USED]
int	x, y			# point to move to

include	"sgk.com"

begin
	if (mf_bitmap) {
	    if (mf_rotate) {
		mf_cx = y
		mf_cy = x
	    } else {
		mf_cx = x
		mf_cy = y
	    }

	    if (mf_yflip)
		mf_cy = GKI_MAXNDC - mf_cy

	    # Convert to zero indexed coordinates and clip at boundary.
	    # Allow room for line width shift near boundary.

	    mf_cx = max (mf_xmin, min (mf_xmax,
		int (mf_cx * mf_xscale) + mf_xorigin))
	    mf_cy = max (mf_ymin, min (mf_ymax,
		int (mf_cy * mf_yscale) + mf_yorigin))

	} else {
	    if (mf_op + SGK_LENMCI > LEN_OBUF) {
		call miiwrites (mf_fd, mf_obuf, mf_op-1)
		mf_op = 1
	    }

	    mf_obuf[mf_op] = SGK_MOVE
	    if (mf_rotate) { 
		mf_obuf[mf_op+1] = y
		mf_obuf[mf_op+2] = x
	    } else {
		mf_obuf[mf_op+1] = x
		mf_obuf[mf_op+2] = y
	    }
	    if (mf_yflip)
		mf_obuf[mf_op+2] = GKI_MAXNDC - mf_obuf[mf_op+2]
	    mf_op = mf_op + SGK_LENMCI
	}
end


# SGK_DRAW -- Output a pen draw instruction.

procedure sgk_draw (fd, a_x, a_y)

int	fd			# output stream [NOT USED]
int	a_x, a_y		# point to draw to

char	fname[SZ_FNAME]
int	xshift, yshift, dx, dy
int	new_x, new_y, x1, y1, x2, y2, n, i
int	open()
errchk	open, close
include	"sgk.com"

begin
	if (mf_rotate) {
	    new_x = a_y
	    new_y = a_x
	} else {
	    new_x = a_x
	    new_y = a_y
	}

	if (mf_yflip)
	    new_y = GKI_MAXNDC - new_y

	if (!mf_update) {
	    # We are called when the first drawing instruction is output for a
	    # new frame.  We clear the bitmap or close and open a new frame
	    # file here, rather than at sgk_frame() time, as we do not want
	    # to initialize the frame buffer or open a new frame file unless
	    # we are actually going to write into the frame.

	    # Zero out all the bits in a bitmap.
	    if (mf_bitmap)
		call aclri (mf_fbuf, mf_lenframe)

	    # Open a new frame file if the one frame per file flag is set.
	    if (mf_oneperfile && mf_frame > 1) {
		if (mf_fd != NULL)
		    call close (mf_fd)
		call sgk_mkfname (mf_fname, mf_frame, fname, SZ_FNAME)
		if (mf_debug) {
		    call eprintf ("sgk: open frame %2d, outfile = %s\n")
			call pargi (mf_frame)
			call pargstr (fname)
		}
		mf_fd = open (fname, NEW_FILE, BINARY_FILE)
	    }

	    mf_update = true
	}

	if (mf_bitmap) {
	    # Convert to zero indexed coordinates and clip at boundary.
	    # Allow room for line width shift near boundary.

	    new_x = max (mf_xmin, min (mf_xmax,
		int (new_x * mf_xscale) + mf_xorigin))
	    new_y = max (mf_ymin, min (mf_ymax,
		int (new_y * mf_yscale) + mf_yorigin))

	    if (mf_linewidth <= 1)
		call sgk_vector (mf_cx, mf_cy, new_x, new_y)
	    else {
		# Redraw the vector several times with small normal shifts to
		# produce a wider line.

		xshift = 0
		yshift = 0

		if (abs (new_x - mf_cx) > abs (new_y - mf_cy)) {
		    dx = 0
		    dy = 1
		} else {
		    dx = 1
		    dy = 0
		}

		do i = 1, mf_linewidth {
		    x1 = mf_cx + xshift
		    y1 = mf_cy + yshift
		    x2 = new_x + xshift
		    y2 = new_y + yshift

		    call sgk_vector (x1, y1, x2, y2)

		    n = (i + 1) / 2
		    if (and (i, 1) == 0) {
			xshift =  dx * n
			yshift =  dy * n
		    } else {
			xshift = -dx * n
			yshift = -dy * n
		    }
		}
	    }

	    # Update the current pen position, and set the update flag so that
	    # the bitmap will be written to the output file.

	    mf_cx = new_x
	    mf_cy = new_y

	} else {
	    # Output a metacode draw instruction.
	    if (mf_op + SGK_LENMCI > LEN_OBUF) {
		call miiwrites (mf_fd, mf_obuf, mf_op-1)
		mf_op = 1
	    }

	    mf_obuf[mf_op] = SGK_DRAW
	    mf_obuf[mf_op+1] = new_x
	    mf_obuf[mf_op+2] = new_y
	    mf_op = mf_op + SGK_LENMCI
	}
end


# SGK_VECTOR -- Write a vector (line) of unit width into the bitmap.  The line
# endpoints are expressed in physical device coordinates.

procedure sgk_vector (a_x1, a_y1, a_x2, a_y2)

int	a_x1, a_y1			# start point of line
int	a_x2, a_y2			# end point of line

real	dydx, dxdy
long	fbit, wbit, word
int	wpln, mask, dx, dy, x, y, x1, y1, x2, y2, or()
include	"sgk.com"

begin
	x1 = a_x1; y1 = a_y1
	x2 = a_x2; y2 = a_y2

	dx = x2 - x1
	dy = y2 - y1

	if (abs(dx) > abs(dy)) {
	    if (x1 > x2) {
		x1 = a_x2; x2 = a_x1; dx = -dx
		y1 = a_y2; y2 = a_y1; dy = -dy
	    }

	    if (dy == 0 && mf_nbpb == NBITS_BYTE) {
		# Somewhat optimized code for the case of a horiz. vector.

		fbit = y1 * mf_pxsize + x1
		word = fbit / BPW
		wbit = and (fbit, BPW-1)

		do x = x1, x2 {
		    mf_fbuf[word+1] = or (mf_fbuf[word+1], mf_bitmask[wbit+1])
		    wbit = wbit + 1
		    if (wbit >= BPW) {
			wbit = 0
			word = word + 1
		    }
		}

	    } else {
		# The general case for a mostly-X vector.

		dydx = real(dy) / real(dx)
		do x = x1, x2 {
		    y = int ((x - x1) * dydx) + y1
		    fbit = y * mf_pxsize + mf_physbit[x+1]
		    word = fbit / BPW
		    wbit = and (fbit, BPW-1)
		    mf_fbuf[word+1] = or (mf_fbuf[word+1], mf_bitmask[wbit+1])
		}
	    }

	} else if (dy != 0) {
	    if (y1 > y2) {
		x1 = a_x2; x2 = a_x1; dx = -dx
		y1 = a_y2; y2 = a_y1; dy = -dy
	    }

	    if (dx == 0) {
		# Optimized code for the case of a vertical vector.

		fbit = y1 * mf_pxsize + mf_physbit[x1+1]
		word = fbit / BPW + 1
		wbit = and (fbit, BPW-1)
		wpln = (mf_pxsize + BPW-1) / BPW
		mask = mf_bitmask[wbit+1]

		do y = y1, y2 {
		    mf_fbuf[word] = or (mf_fbuf[word], mask)
		    word = word + wpln
		}

	    } else {
		# The general case of a mostly-Y vector.

		dxdy = real(dx) / real(dy)
		do y = y1, y2 {
		    x = int ((y - y1) * dxdy) + x1
		    fbit = y * mf_pxsize + mf_physbit[x+1]
		    word = fbit / BPW
		    wbit = and (fbit, BPW-1)
		    mf_fbuf[word+1] = or (mf_fbuf[word+1], mf_bitmask[wbit+1])
		}
	    }

	} else {
	    # Plot a single point (dx=dy=0).

	    fbit = y1 * mf_pxsize + mf_physbit[x1+1]
	    word = fbit / BPW
	    wbit = and (fbit, BPW-1)
	    mf_fbuf[word+1] = or (mf_fbuf[word+1], mf_bitmask[wbit+1])
	}
end


# SGK_LINEWIDTH -- Output a line width set instruction.

procedure sgk_linewidth (fd, width)

int	fd			# output stream [NOT USED]
int	width			# new line width

int	gap
include	"sgk.com"

begin
	if (mf_bitmap) {
	    # Set the line width in device pixels.
	    mf_linewidth = max (1, mf_lworigin + int ((width-1) * mf_lwslope))

	    # Set the clipping limits.  Allow for shifting to widen lines.
	    gap = mf_linewidth - 1
	    mf_xmin = mf_xorigin + gap
	    mf_ymin = mf_yorigin + gap
	    mf_xmax = mf_xorigin + (mf_wxsize - 1) - gap
	    mf_ymax = mf_yorigin + (mf_wysize - 1) - gap

	} else {
	    if (mf_op + SGK_LENMCI > LEN_OBUF) {
		call miiwrites (mf_fd, mf_obuf, mf_op-1)
		mf_op = 1
	    }

	    mf_obuf[mf_op] = SGK_SETLW
	    mf_obuf[mf_op+1] = width
	    mf_obuf[mf_op+2] = 0
	    mf_op = mf_op + SGK_LENMCI
	}
end


# SGK_MKFNAME -- Make the name of file N of a multiframe set.

procedure sgk_mkfname (root, num, outstr, maxch)

char	root[ARB]		# root filename
int	num			# file number
char	outstr[maxch]		# receives new filename
int	maxch

begin
	call sprintf (outstr, maxch, "%s.%d")
	    call pargstr (root)
	    call pargi (num)
end
