# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<chars.h>
include	<ctype.h>
include	<fset.h>
include	<gescape.h>
include	<gki.h>
include	<gim.h>
include	"stdgraph.h"

# STGGIM.X -- GIO.GIM gterm imaging escapes for the stdgraph kernel.  The
# routines in this file decode GKI escape instructions and encode serial
# byte sequences which are sent to the server to execute the instruction.
# In some cases the server returns a response which is decoded here and
# encoded as a GKI return value which is returned to the client.

define	SZ_PATBUF	512
define	MAX_ARGS	32
define  TIMEOUT		10000


# SGM_EXECUTE -- Test whether the given instruction is a GIM escape, and
# if so execute it.  Return true if the instruction was recognized and
# executed.

bool procedure sgm_execute (fn, gim, nwords)

int	fn			#I function code
short	gim[ARB]		#I instruction data words
int	nwords			#I length of gim

int	raster
common	/sgmcom/ raster

begin
	switch (fn) {
	case GKI_OPENWS, GKI_CLEAR:
	    call sgm_output ("RI", gim, GIM_RASTERINIT_LEN)
	    return (false)

	case GIM_RASTERINIT:
	    call sgm_output ("RI", gim, GIM_RASTERINIT_LEN)
	case GIM_CREATERASTER:
	    call sgm_output ("CR", gim, GIM_CREATERASTER_LEN)
	case GIM_DESTROYRASTER:
	    call sgm_output ("DR", gim, GIM_DESTROYRASTER_LEN)
	case GIM_QUERYRASTER:
	    call sgm_queryraster (gim)
	case GIM_SETRASTER:
	    raster = gim[GIM_SETRASTER_RN]
	    call sgm_output ("SR", gim, GIM_SETRASTER_LEN)
	case GIM_WRITEPIXELS:
	    call sgm_writepixels (gim)
	case GIM_READPIXELS:
	    call sgm_readpixels (gim)
	case GIM_REFRESHPIXELS:
	    call sgm_output ("RX", gim, GIM_REFRESHPIXELS_LEN)
	case GIM_SETPIXELS:
	    call sgm_output ("SP", gim, GIM_SETPIXELS_LEN)
	case GIM_WRITECMAP:
	    call sgm_writecmap (gim)
	case GIM_READCMAP:
	    call sgm_readcmap (gim)
	case GIM_LOADCMAP:
	    call sgm_output ("LM", gim, GIM_LOADCMAP_LEN)
	case GIM_FREECMAP:
	    call sgm_output ("FL", gim, GIM_FREECMAP_LEN)
	case GIM_WRITEIOMAP:
	    call sgm_iomapwrite (gim)
	case GIM_READIOMAP:
	    call sgm_iomapread (gim)
	case GIM_INITMAPPINGS:
	    call sgm_output ("IM", gim, GIM_INITMAPPINGS_LEN)
	case GIM_FREEMAPPING:
	    call sgm_output ("FM", gim, GIM_FREEMAPPING_LEN)
	case GIM_COPYRASTER:
	    call sgm_output ("CP", gim, GIM_COPYRASTER_LEN)
	case GIM_SETMAPPING:
	    call sgm_output ("SM", gim, GIM_SETMAPPING_LEN)
	case GIM_GETMAPPING:
	    call sgm_getmapping (gim)
	case GIM_ENABLEMAPPING:
	    call sgm_output ("MN", gim, GIM_ENABLEMAPPING_LEN)
	case GIM_DISABLEMAPPING:
	    call sgm_output ("MD", gim, GIM_DISABLEMAPPING_LEN)
	case GIM_REFRESHMAPPING:
	    call sgm_output ("RF", gim, GIM_REFRESHMAPPING_LEN)

	default:
	    return (false)
	}

	return (true)
end


# SGM_WSTRAN -- Transform and output a GIM escape.  Ignore escapes we
# know nothing about.  TRUE is returned if the escape is one which is private
# to the GIM interface.

bool procedure sgm_wstran (fn, gim, rx1,ry1, rx2,ry2)

int	fn			#I escape sequence function opcode
short	gim[ARB]		#I escape instruction data
real	rx1,ry1			#I NDC coords of display rect
real	rx2,ry2			#I NDC coords of display rect

real	scale
pointer	sp, n_gim
bool	status, xflip, yflip
int	width, height, dst, src, dt
int	wx1, wy1, wx2, wy2, p1, p2
int	sx1, sy1, sx2, sy2, snx, sny
int	dx1, dy1, dx2, dy2, dnx, dny
int	n_dx1, n_dy1, n_dx2, n_dy2, n_dnx, n_dny
int	n_sx1, n_sy1, n_sx2, n_sy2
int	w_dx1, w_dy1, w_dx2, w_dy2
bool	sgm_execute()
define	exe_ 91

begin
	switch (fn) {
	case GIM_RASTERINIT, GIM_INITMAPPINGS,
	    GIM_CREATERASTER, GIM_DESTROYRASTER, GIM_QUERYRASTER,
	    GIM_GETMAPPING, GIM_ENABLEMAPPING, GIM_DISABLEMAPPING,
	    GIM_REFRESHMAPPING, GIM_FREEMAPPING,
	    GIM_READPIXELS, GIM_WRITEPIXELS, GIM_REFRESHPIXELS, GIM_SETPIXELS,
	    GIM_WRITECMAP, GIM_READCMAP, GIM_LOADCMAP, GIM_FREECMAP,
	    GIM_WRITEIOMAP, GIM_READIOMAP,
	    GIM_COPYRASTER, GIM_SETRASTER:

	    # These instructions do not require any transformation.
	    status = sgm_execute (fn, gim, 0)

	case GIM_SETMAPPING:
	    # Edit setmapping instructions which write to the display window.
	    # Raster 0 is the display window; only display window coordinates
	    # are affected by the workstation transformation.

	    src = gim[GIM_SETMAPPING_SR]
	    dst = gim[GIM_SETMAPPING_DR]
	    dt  = gim[GIM_SETMAPPING_DT]

	    if (dst == 0 && src != dst) {
		call smark (sp)
		call salloc (n_gim, GIM_SETMAPPING_LEN, TY_SHORT)

		xflip = false
		yflip = false

		# Convert the display rect NDC coordinates to window pixels
		# or GKI units, depending upon which coordinate system is
		# in use.  Note that for NDC the Y axis is flipped relative
		# to display window pixel coordinates.

		if (dt == CT_PIXEL) {
		    call sgm_winsize (width, height)
		    wx1 = rx1 * (width - 1);  wy1 = (1.0 - ry2) * (height - 1)
		    wx2 = rx2 * (width - 1);  wy2 = (1.0 - ry1) * (height - 1)
		} else {
		    width = GKI_MAXNDC + 1;  height = GKI_MAXNDC + 1
		    wx1 = rx1 * (width - 1);  wy1 = ry1 * (height - 1)
		    wx2 = rx2 * (width - 1);  wy2 = ry2 * (height - 1)
		}

		sx1 = gim[GIM_SETMAPPING_SX]
		snx = gim[GIM_SETMAPPING_SW]
		sy1 = gim[GIM_SETMAPPING_SY]
		sny = gim[GIM_SETMAPPING_SH]
		sx2 = sx1 + snx - 1;  sy2 = sy1 + sny - 1 

		dx1 = gim[GIM_SETMAPPING_DX]
		dnx = gim[GIM_SETMAPPING_DW]
		if (dnx < 0) {
		    dnx = -dnx
		    xflip = !xflip
		}
		dy1 = gim[GIM_SETMAPPING_DY]
		dny = gim[GIM_SETMAPPING_DH]
		if (dny < 0) {
		    dny = -dny
		    yflip = !yflip
		}
		dx2 = dx1 + dnx - 1;  dy2 = dy1 + dny - 1 

		# Compute the intersection of the destination (screen) rect
		# of the mapping with the region of the screen WS mapped by
		# the workstation transformation.

		n_dx1 = max (wx1, dx1);  n_dy1 = max (wy1, dy1)
		n_dx2 = min (wx2, dx2);  n_dy2 = min (wy2, dy2)

		# If the rectangles do not intersect set up a null mapping
		# to temporarily disable the mapping.

		n_dnx = n_dx2 - n_dx1 + 1;  n_dny = n_dy2 - n_dy1 + 1
		if (n_dnx <= 0 || n_dny <= 0) {
		    call amovs (gim, Mems[n_gim], GIM_SETMAPPING_LEN)
		    Mems[n_gim+GIM_SETMAPPING_SX-1] = 0
		    Mems[n_gim+GIM_SETMAPPING_SW-1] = 0
		    Mems[n_gim+GIM_SETMAPPING_SY-1] = 0
		    Mems[n_gim+GIM_SETMAPPING_SH-1] = 0
		    Mems[n_gim+GIM_SETMAPPING_DX-1] = 0
		    Mems[n_gim+GIM_SETMAPPING_DW-1] = 0
		    Mems[n_gim+GIM_SETMAPPING_DY-1] = 0
		    Mems[n_gim+GIM_SETMAPPING_DH-1] = 0
		    goto exe_
		}

		# Compute the source rect which maps to the new (intersection)
		# destination rect.

		if (snx == 1 || dnx == 1) {
		    n_sx1 = sx1
		    n_sx2 = sx2
		} else {
		    scale = real(snx - 1) / real(dnx - 1)
		    n_sx1 = max(0, min(GKI_MAXNDC,
			nint((n_dx1 - dx1) * scale + sx1)))
		    n_sx2 = max(0, min(GKI_MAXNDC,
			nint((n_dx2 - dx2) * scale + sx2)))
		    if (xflip) {
			p1 = sx1 + (sx2 - n_sx2)
			p2 = sx2 - (n_sx1 - sx1)
			n_sx1 = p1;  n_sx2 = p2
		    }
		}

		if (sny == 1 || dny == 1) {
		    n_sy1 = sy1
		    n_sy2 = sy2
		} else {
		    scale = real(sny - 1) / real(dny - 1)
		    n_sy1 = max(0, min(GKI_MAXNDC,
			nint((n_dy1 - dy1) * scale + sy1)))
		    n_sy2 = max(0, min(GKI_MAXNDC,
			nint((n_dy2 - dy2) * scale + sy2)))
		    if (yflip) {
			p1 = sy1 + (sy2 - n_sy2)
			p2 = sy2 - (n_sy1 - sy1)
			n_sy1 = p1;  n_sy2 = p2
		    }
		}

		# Scale the destination rect by the amount needed to make the
		# WS rect fill the full display window.

		if (wx1 == wx2) {
		    w_dx1 = 0
		    w_dx1 = width - 1
		} else {
		    scale = real(width - 1) / real(wx2 - wx1)
		    w_dx1 = max(0, min(GKI_MAXNDC,
			nint((n_dx1 - wx1) * scale)))
		    w_dx2 = max(0, min(GKI_MAXNDC,
			nint((n_dx2 - wx1) * scale)))
		}

		if (wy1 == wy2) {
		    w_dy1 = 0
		    w_dy1 = height - 1
		} else {
		    scale = real(height - 1) / real(wy2 - wy1)
		    w_dy1 = max(0, min(GKI_MAXNDC,
			nint((n_dy1 - wy1) * scale)))
		    w_dy2 = max(0, min(GKI_MAXNDC,
			nint((n_dy2 - wy1) * scale)))
		}

		# Construct the edited instruction.
		call amovs (gim, Mems[n_gim], GIM_SETMAPPING_LEN)
		Mems[n_gim+GIM_SETMAPPING_SX-1] = n_sx1
		Mems[n_gim+GIM_SETMAPPING_SW-1] = n_sx2 - n_sx1 + 1
		Mems[n_gim+GIM_SETMAPPING_SY-1] = n_sy1
		Mems[n_gim+GIM_SETMAPPING_SH-1] = n_sy2 - n_sy1 + 1
		Mems[n_gim+GIM_SETMAPPING_DX-1] = w_dx1
		Mems[n_gim+GIM_SETMAPPING_DY-1] = w_dy1

		n_dnx = max(0, min(GKI_MAXNDC, w_dx2 - w_dx1 + 1))
		if (gim[GIM_SETMAPPING_DW] < 0)
		    n_dnx = -n_dnx
		Mems[n_gim+GIM_SETMAPPING_DW-1] = n_dnx

		n_dny = max(0, min(GKI_MAXNDC, w_dy2 - w_dy1 + 1))
		if (gim[GIM_SETMAPPING_DH] < 0)
		    n_dny = -n_dny
		Mems[n_gim+GIM_SETMAPPING_DH-1] = n_dny

exe_
		# Execute the edited instruction.
		status = sgm_execute (fn, Mems[n_gim], 0)
		call sfree (sp)

	    } else
		status = sgm_execute (fn, gim, 0)

	default:
	    status = false
	}

	return (status)
end


# SGM_WSENABLE -- Test if client scaling of graphics drawing instructions is
# enabled.  For the stdgraph kernel, these transformations are disabled if
# the raster is other than zero, in which case the graphics server does the
# scaling.

bool procedure sgm_wsenable (enable)

bool	enable
int	raster
common	/sgmcom/ raster

begin
	enable = (raster == 0)
	return (true)
end


# SGM_SPOOLESC -- Process a GIM escape into a frame buffer.  All GIM escapes 
# are executed when first issued; we just determine whether the escapes are
# preserved in the frame buffer to be executed when the frame is redrawn.
# Ignore escapes we know nothing about.  TRUE is returned if the escape is
# one which is private to the GIM interface, i.e., if the escape has been
# processed fully by sgm_spoolesc.

bool procedure sgm_spoolesc (tr, gki, fn, gim, bp, buftop, delete_fcn)

pointer tr			#I arg to delete_fcn
pointer gki			#I pointer to escape instruction
int	fn			#U escape sequence function opcode
short	gim[ARB]		#U escape instruction data
pointer bp			#I frame buffer pointer
pointer buftop			#I top+1 of buffered data
int	delete_fcn		#I function called to delete an instruction

pointer	ip, itop, esc
int	nleft, length, opcode, escape, mp

begin
	switch (fn) {
	case GIM_RASTERINIT, GIM_INITMAPPINGS, GIM_FREEMAPPING,
	    GIM_CREATERASTER, GIM_DESTROYRASTER, GIM_QUERYRASTER,
	    GIM_GETMAPPING, GIM_ENABLEMAPPING, GIM_DISABLEMAPPING,
	    GIM_REFRESHMAPPING, GIM_WRITEPIXELS, GIM_READPIXELS,
	    GIM_REFRESHPIXELS, GIM_SETPIXELS, GIM_COPYRASTER,
	    GIM_WRITEIOMAP, GIM_READIOMAP, GIM_WRITECMAP, GIM_READCMAP,
	    GIM_LOADCMAP, GIM_FREECMAP:

	    # These escapes are only executed once.
	    call zcall2 (delete_fcn, tr, gki)

	case GIM_SETRASTER:
	    ;  # Preserve this instruction.

	case GIM_SETMAPPING:
	    # This escape is saved in the frame buffer and rexecuted when
	    # the frame is redrawn.  This allows the server to buffer the
	    # image data, but still allows the graphics to be redrawn and
	    # possibly rescaled in cursor mode.  Rexecution of copyraster
	    # after a screen clear will cause any rasters created and written
	    # to with createraster/writepixels to be redrawn on the screen.

	    ip = bp
	    itop = gki

	    while (ip < itop) {
		# Search for the beginning of the next instruction.
		while (Mems[ip] != BOI && ip < itop)
		    ip = ip + 1

		nleft = itop - ip
		if (nleft < 3)
		    break
		else {
		    length = Mems[ip+GKI_HDR_LENGTH-1]
		    if (length > nleft)
			break

		    opcode = Mems[ip+GKI_HDR_OPCODE-1]
		    escape = Mems[ip+GKI_ESCAPE_FN-1]

		    # Disable instruction if same mapping number.
		    if (opcode == GKI_ESCAPE && escape == GIM_SETMAPPING) {
			esc = ip + GKI_ESCAPE_DC - 1
			mp = Mems[esc+GIM_SETMAPPING_MP-1]
			if (mp == gim[GIM_SETMAPPING_MP])
			    Mems[ip+GKI_HDR_OPCODE-1] = GKI_UNKNOWN
		    }

		    ip = ip + length
		}
	    }

	default:
	    return (false)
	}

	return (true)
end


# SGM_WINSIZE -- Get the graphics window size in display pixels.

procedure sgm_winsize (width, height)

int	width, height		#O window size

short	gim_query[GIM_QUERYRASTER_LEN]
short	retval[GIM_RET_QRAS_LEN]

begin
	gim_query[GIM_QUERYRASTER_RN] = 0
	call sgm_query ("QR", gim_query, GIM_QUERYRASTER_LEN,
	    "Qr", retval, GIM_RET_QRAS_LEN)
	width  = retval[GIM_RET_QRAS_NX]
	height = retval[GIM_RET_QRAS_NY]
end


# SGM Private Functions.
# ---------------------------

# SGM_QUERYRASTER -- Return the attributes of a raster.

procedure sgm_queryraster (gim)

short	gim[ARB]			#I encoded instruction
short	retval[GIM_RET_QRAS_LEN]
include	"stdgraph.com"

begin
	call sgm_query ("QR", gim, GIM_QUERYRASTER_LEN,
	    "Qr", retval, GIM_RET_QRAS_LEN)
	call write (g_stream, retval, GIM_RET_QRAS_LEN * SZ_SHORT)
	call flush (g_stream)
end


# SGM_WRITEPIXELS -- Write a block of pixels to a raster.

procedure sgm_writepixels (gim)

short	gim[ARB]			#I encoded instruction

char	bias
pointer	sp, bp
int	nx, ny, npix, i
include	"stdgraph.com"

begin
	# Send the write-pixels escape sequence.
	call sgm_output ("WP", gim, GIM_WRITEPIXELS_LEN)

	# For the moment this code assumes 8 bit pixels.
	nx = gim[GIM_WRITEPIXELS_NX]
	ny = gim[GIM_WRITEPIXELS_NY]
	npix = nx * ny

	call smark (sp)
	call salloc (bp, npix, TY_CHAR)
	bias = 040B

	# Send the pixel data encoded in printable ASCII.
	call achtbc (gim[GIM_WRITEPIXELS_DATA], Memc[bp], npix)
	do i = 1, npix
	    Memc[bp+i-1] = Memc[bp+i-1] + bias
	call write (g_out, Memc[bp], npix)
	call putci (g_out, GS)

	call sfree (sp)
end


# SGM_READPIXELS -- Read a block of pixels from a raster and return it
# to the client.

procedure sgm_readpixels (gim)

short	gim[ARB]			#I encoded instruction

pointer	sp, bp
int	sv_iomode, ch
int	npix, nx, ny, i
short	retval[GIM_RET_RPIX_LEN]
int	fstati(), getci()
include	"stdgraph.com"

begin
	sv_iomode = fstati (g_in, F_IOMODE)
	if (sv_iomode != IO_RAW)
	    call fseti (g_in, F_IOMODE, IO_RAW)

	# Send the read-pixels escape sequence.
	call sgm_output ("RP", gim, GIM_READPIXELS_LEN)
	call flush (g_out)

	# For the moment this code assumes 8 bit pixels.
	nx = gim[GIM_READPIXELS_NX]
	ny = gim[GIM_READPIXELS_NY]
	npix = nx * ny

	call smark (sp)
	call salloc (bp, npix, TY_CHAR)

	# Get the pixel data.  This is a block of pixel data encoded as for
	# writepixels (040 bias), bracked by ESC at the front and a single
	# control character such as \r or \n at the end.

	while (getci (g_in, ch) != EOF)
	    if (ch == ESC)
		break
	for (i=0;  getci (g_in, ch) >= 040B;  )
	    if (i < npix) {
		Memc[bp+i] = ch - 040B
		i = i + 1
	    }
	npix = i

	# Send the RPIX header to the client.
	retval[GIM_RET_RPIX_NP] = npix
	call write (g_stream, retval, GIM_RET_RPIX_LEN * SZ_SHORT)

	# Return the data to the client.
	call achtcb (Memc[bp], Memc[bp], npix)
	call write (g_stream, Memc[bp], (npix + SZB_CHAR-1) / SZB_CHAR)
	call flush (g_stream)

	if (sv_iomode != IO_RAW)
	    call fseti (g_in, F_IOMODE, sv_iomode)
	call sfree (sp)
end


# SGM_WRITECMAP -- Write to a segment of the colormap.

procedure sgm_writecmap (gim)

short	gim[ARB]			#I encoded instruction

short	mask
pointer	sp, bp, op
int	ncells, nchars, ip, i
include	"stdgraph.com"

begin
	call smark (sp)

	# Send the write-colormap escape sequence.
	call sgm_output ("WM", gim, GIM_WRITECMAP_LEN)

	# Each cell consists of a RGB triplet encoded 2 chars per color.
	ncells = gim[GIM_WRITECMAP_NC]
	nchars = ncells * 3 * 2

	call salloc (bp, nchars, TY_CHAR)
	ip = GIM_WRITECMAP_DATA
	op = bp

	mask = 017B
	do i = 1, ncells*3 {
	    Memc[op] =      gim[ip] / 16   + 040B;  op = op + 1
	    Memc[op] = and (gim[ip], mask) + 040B;  op = op + 1
	    ip = ip + 1
	}

	call write (g_out, Memc[bp], nchars)
	call putci (g_out, GS)

	call sfree (sp)
end


# SGM_READCMAP -- Read a segment of the colormap.

procedure sgm_readcmap (gim)

short	gim[ARB]			#I encoded instruction

pointer	sp, bp, cm, ip
int	sv_iomode, ncells, nchars, ch, i
short	retval[GIM_RET_RCMAP_LEN]
int	fstati(), getci()
include	"stdgraph.com"

begin
	sv_iomode = fstati (g_in, F_IOMODE)
	if (sv_iomode != IO_RAW)
	    call fseti (g_in, F_IOMODE, IO_RAW)

	# Send the read-cmap escape sequence.
	call sgm_output ("RM", gim, GIM_READCMAP_LEN)
	call flush (g_out)

	# Each cell consists of a RGB triplet encoded 2 chars per color.
	ncells = gim[GIM_READCMAP_NC]
	nchars = ncells * 3 * 2

	call smark (sp)
	call salloc (bp, nchars, TY_CHAR)
	call salloc (cm, ncells * 3, TY_SHORT)

	# Get the colormap data.  This is a block of RGB colormap triplets
	# encoded 2 bytes per color, bracked by a ESC at the front and a
	# single control character such as \r or \n at the end.

	while (getci (g_in, ch) != EOF)
	    if (ch == ESC)
		break
	for (i=0;  getci (g_in, ch) >= 040B;  )
	    if (i < nchars) {
		Memc[bp+i] = ch - 040B
		i = i + 1
	    }
	ncells = i / (3 * 2)

	# Decode the packed colormap data.
	ip = bp
	do i = 1, ncells * 3 {
	    Mems[cm+i-1] = (Memc[ip] - 040B) * 16 + (Memc[ip+1] - 040B)
	    ip = ip + 2
	}

	# Send the read-cmap header to the client.
	retval[GIM_RET_RCMAP_NC] = ncells
	call write (g_stream, retval, GIM_RET_RCMAP_LEN * SZ_SHORT)

	# Return the colormap data to the client.
	call write (g_stream, Mems[cm], (ncells * 3) * SZ_SHORT)
	call flush (g_stream)

	if (sv_iomode != IO_RAW)
	    call fseti (g_in, F_IOMODE, sv_iomode)
	call sfree (sp)
end


# SGM_IOMAPWRITE -- Write to the iomap.

procedure sgm_iomapwrite (gim)

short	gim[ARB]			#I encoded instruction

short	mask
pointer	sp, bp, op
int	ncells, nchars, ip, i
include	"stdgraph.com"

begin
	call smark (sp)

	# Send the write-iomap escape sequence.
	call sgm_output ("WO", gim, GIM_WRITEIOMAP_LEN)

	# Each cell consists of a single short integer colormap index
	# encoded 2 chars per cell.

	ncells = gim[GIM_WRITEIOMAP_NC]
	nchars = ncells * 2

	call salloc (bp, nchars, TY_CHAR)
	ip = GIM_WRITEIOMAP_DATA
	op = bp

	mask = 017B
	do i = 1, ncells {
	    Memc[op] =      gim[ip] / 16   + 040B;  op = op + 1
	    Memc[op] = and (gim[ip], mask) + 040B;  op = op + 1
	    ip = ip + 1
	}

	call write (g_out, Memc[bp], nchars)
	call putci (g_out, GS)

	call sfree (sp)
end


# SGM_IOMAPREAD -- Read a segment of the iomap.

procedure sgm_iomapread (gim)

short	gim[ARB]			#I encoded instruction

pointer	sp, bp, data, ip
int	sv_iomode, ncells, nchars, ch, i
short	retval[GIM_RET_RIOMAP_LEN]
int	fstati(), getci()
include	"stdgraph.com"

begin
	sv_iomode = fstati (g_in, F_IOMODE)
	if (sv_iomode != IO_RAW)
	    call fseti (g_in, F_IOMODE, IO_RAW)

	# Send the read-iomap escape sequence.
	call sgm_output ("RO", gim, GIM_READIOMAP_LEN)
	call flush (g_out)

	# The data is encoded two bytes per short integer value.
	ncells = gim[GIM_READIOMAP_NC]
	nchars = ncells * 2

	call smark (sp)
	call salloc (bp, nchars, TY_CHAR)
	call salloc (data, ncells, TY_SHORT)

	# Get the iomap data.  This is a block of iomap values encoded
	# 2 bytes per value, bracked by a ESC at the front and a single
	# control character such as \r or \n at the end.

	while (getci (g_in, ch) != EOF)
	    if (ch == ESC)
		break
	for (i=0;  getci (g_in, ch) >= 040B;  )
	    if (i < nchars) {
		Memc[bp+i] = ch - 040B
		i = i + 1
	    }
	ncells = i / 2

	# Decode the packed iomap data.
	ip = bp
	do i = 1, ncells {
	    Mems[data+i-1] = (Memc[ip] - 040B) * 16 + (Memc[ip+1] - 040B)
	    ip = ip + 2
	}

	# Send the read-iomap header to the client.
	retval[GIM_RET_RIOMAP_NC] = ncells
	call write (g_stream, retval, GIM_RET_RIOMAP_LEN * SZ_SHORT)

	# Return the iomap data to the client.
	call write (g_stream, Mems[data], ncells * SZ_SHORT)
	call flush (g_stream)

	if (sv_iomode != IO_RAW)
	    call fseti (g_in, F_IOMODE, sv_iomode)
	call sfree (sp)
end


# SGM_GETMAPPING -- Return the attributes of a mapping.

procedure sgm_getmapping (gim)

short	gim[ARB]			#I encoded instruction
short	retval[GIM_RET_GMAP_LEN]
include	"stdgraph.com"

begin
	call sgm_query ("GM", gim, GIM_GETMAPPING_LEN,
	    "Gm", retval, GIM_RET_GMAP_LEN)
	call write (g_stream, retval, GIM_RET_GMAP_LEN * SZ_SHORT)
	call flush (g_stream)
end


# SGM_OUTPUT -- Format and output a control sequence to the graphics server
# device.

procedure sgm_output (cap, gim, nargs)

char	cap[ARB]			#I graphcap capability name
short	gim[ARB]			#I instruction (array of int args)
int	nargs				#I number of arguments

int	ival, i
pointer	sp, fmt, ctrl
include	"stdgraph.com"
int	ttygets()
errchk	ttygets

begin
	call smark (sp)
	call salloc (fmt, SZ_LINE, TY_CHAR)
	call salloc (ctrl, SZ_LINE, TY_CHAR)

	if (ttygets (g_tty, cap, Memc[fmt], SZ_LINE) > 0) {
	    call sprintf (Memc[ctrl], SZ_LINE, Memc[fmt])
		do i = 1, nargs {
		    # Pass the argument as an integer to avoid INDEF
		    # processing of -32767, a valid GKI value.
		    ival = gim[i]
		    iferr (call pargi (ival))
			;
		}
	    call ttyputs (g_out, g_tty, Memc[ctrl], 1)
	}

	call sfree (sp)
end


# SGM_QUERY -- Output an inquiry control sequence to the server and read and
# decode the server's response.

procedure sgm_query (query_cap, gim, nargs, retval_cap, retval, nout)

char	query_cap[ARB]			#I server query cap name
short	gim[ARB]			#I query instruction (args)
int	nargs				#I number of args for server query
char	retval_cap[ARB]			#I cap name for return value format
short	retval[ARB]			#O decoded output arguments
int	nout				#I number of output arguments

int	index[MAX_ARGS]
pointer	sp, ctrl, patbuf, pat, buf, ip, op
int	sv_iomode, arg, ch, nchars, start, value, ival, i
int	patmake(), patindex(), ttyread(), ctoi()
int	ttygets(), fstati(), gstrcpy()
include	"stdgraph.com"
define	done_ 91
errchk	ttygets

begin
	call smark (sp)
	call salloc (ctrl, SZ_LINE, TY_CHAR)
	call salloc (buf, SZ_LINE, TY_CHAR)
	call salloc (pat, SZ_LINE, TY_CHAR)
	call salloc (patbuf, SZ_PATBUF, TY_CHAR)

	call aclrs (retval, nout)

	# Set raw mode i/o.
	sv_iomode = fstati (g_in, F_IOMODE)
	if (sv_iomode != IO_RAW)
	    call fseti (g_in, F_IOMODE, IO_RAW)

	# Pass the query on to the server.
	if (ttygets (g_tty, query_cap, Memc[pat], SZ_LINE) > 0) {
	    call sprintf (Memc[ctrl], SZ_LINE, Memc[pat])
		do i = 1, nargs {
		    # Pass the argument as an integer to avoid INDEF
		    # processing of -32767, a valid GKI value.
		    ival = gim[i]
		    iferr (call pargi (ival))
			;
		}
	    call ttyputs (g_out, g_tty, Memc[ctrl], 1)
	    call flush (g_out)

	} else
	    goto done_

	# Encode a pattern to match the server's response as given by the
	# pattern in retval_cap.

        if (ttygets (g_tty, retval_cap, Memc[pat], SZ_LINE) <= 0)
	    goto done_

        # Process the retval_cap string, used to specify the format of the
	# string returned by the server, to map the %N fields therein into
        # the pattern strings "%[0-9]*", noting the index positions of the
        # pattern substrings for later decoding.

	call aclri (index, MAX_ARGS)
	arg = 0

	op = buf
	for (ip=pat;  Memc[ip] != EOS;  ip=ip+1) {
	    if (Memc[ip] == '%') {
		if (Memc[ip+1] == '%') {
		    Memc[op] = Memc[ip]
		    op = op + 1
		    ip = ip + 1
		} else {
		    op = op + gstrcpy ("%[0-9]*", Memc[op], ARB)
		    ip = ip + 1

		    # Arguments are %1 ... %9, %a, %b, etc.
		    ch = Memc[ip]
		    if (IS_DIGIT(ch))
			i = TO_INTEG(ch)
		    else if (IS_UPPER(ch))
			i = ch - 'A' + 10
		    else
			i = ch - 'a' + 10

		    arg = arg + 1
		    i = min(MAX_ARGS, max(1, i))
		    index[i] = arg
		}
	    } else if (Memc[ip] == '[') {
		Memc[op] = '\\';  op = op + 1
		Memc[op] = '[' ;  op = op + 1
	    } else {
		Memc[op] = Memc[ip]
		op = op + 1
	    }
	}

	Memc[op] = EOS
	if (patmake (Memc[buf], Memc[patbuf], SZ_PATBUF) >= SZ_PATBUF)
	    goto done_

	# Scan the input stream from the server until data matching the
	# response pattern is received, or a timeout occurs.

	nchars = ttyread (g_in, g_tty,Memc[buf],SZ_LINE,Memc[patbuf], TIMEOUT)
	if (nchars > 0) {
	    do i = 1, nout {
		value = 0
		if (index[i] > 0) {
		    start = patindex (Memc[patbuf], index[i])
		    if (ctoi (Memc[buf], start, value) <= 0)
			value = 0
		}
		retval[i] = value
	    }
	}
done_
	if (sv_iomode != IO_RAW)
	    call fseti (g_in, F_IOMODE, sv_iomode)
	call sfree (sp)
end
