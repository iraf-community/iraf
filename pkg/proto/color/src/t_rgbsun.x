include <imhdr.h>
include <mach.h>

define	RAS_HDRLEN	8		# SunOS4.1 and earlier
define	RAS_MAGIC	1504078485	# SunOS4.1 and earlier
define	RT_STANDARD	1		# SunOS4.1 and earlier
define	RMT_NONE	0		# SunOS4.1 and earlier

# T_RGBSUN -- IRAF to 24-bit RGB Sun Rasterfile
# This format-specific task is primarily used to display color composites
# on an 8-bit display using a task such as XV.
# ** The format of the output Sun rasterfiles is hard-coded into this task,
# ** and thus could diverge from a future Sun format; we do not want to link
# ** with Sun libraries, as this task should be runnable on other machines.

procedure t_rgbsun ()

pointer	rim, gim, bim		# Red, green, blue images
int	rgb			# Output Sun rasterfile
real	rz1, rz2		# Red display range
real	gz1, gz2		# Green display range
real	bz1, bz2		# Blue display range
bool	logmap			# Logartihmic intensity mapping?
bool	swap			# Swap header bytes?

int	i, j, nc, nl, ncrgb, rheader[8], open()
real	rdz, gdz, bdz, v
real	clgetr()
pointer	rbuf, gbuf, bbuf, obuf, rgbbuf, immap(), imgl2r()
bool	clgetb()
pointer	sp, fname

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)

	call clgstr ("red", Memc[fname], SZ_FNAME)
	rim = immap (Memc[fname], READ_ONLY, 0)
	call clgstr ("green", Memc[fname], SZ_FNAME)
	gim = immap (Memc[fname], READ_ONLY, 0)
	call clgstr ("blue", Memc[fname], SZ_FNAME)
	bim = immap (Memc[fname], READ_ONLY, 0)

	i = IM_NDIM(rim)
	nc = IM_LEN(rim,1)
	nl = IM_LEN(rim,2)
	ncrgb = 3 * nc
	ncrgb = ncrgb + mod (ncrgb, 2)
	if (i != 2 || i != IM_NDIM(gim) || i != IM_NDIM(bim))
	    call error (1, "All images must be two dimensional")
	if (nc != IM_LEN(gim,1) || nc != IM_LEN(bim,1))
	    call error (1, "All images must be the same size")
	if (nl != IM_LEN(gim,2) || nl != IM_LEN(bim,2))
	    call error (1, "All images must be the same size")

	call clgstr ("rgb", Memc[fname], SZ_FNAME)
	rgb = open (Memc[fname], NEW_FILE, BINARY_FILE)
	rheader[1] = RAS_MAGIC
	rheader[2] = nc
	rheader[3] = nl
	rheader[4] = 24
	rheader[5] = nc * nl * 3
	rheader[6] = RT_STANDARD
	rheader[7] = RMT_NONE
	rheader[8] = 0

	rz1 = clgetr ("rz1")
	rz2 = clgetr ("rz2")
	gz1 = clgetr ("gz1")
	gz2 = clgetr ("gz2")
	bz1 = clgetr ("bz1")
	bz2 = clgetr ("bz2")
	logmap = clgetb ("logmap")
	swap = clgetb ("swap")

	if (logmap) {
	    rdz = 9. / (rz2 - rz1)
	    gdz = 9. / (gz2 - gz1)
	    bdz = 9. / (bz2 - bz1)
	} else {
	    rdz = 255. / (rz2 - rz1)
	    gdz = 255. / (gz2 - gz1)
	    bdz = 255. / (bz2 - bz1)
	}

	call ipak32 (rheader, rheader, RAS_HDRLEN)
	if (swap)
	    call bswap4 (rheader, 1, rheader, 1, RAS_HDRLEN*SZ_INT32*SZB_CHAR)
	call write (rgb, rheader, RAS_HDRLEN * SZ_INT32)

	call salloc (rgbbuf, ncrgb, TY_CHAR)
	Memc[rgbbuf+ncrgb-1] = 0
	do j = 1, nl {
	    rbuf = imgl2r (rim, j)
	    gbuf = imgl2r (gim, j)
	    bbuf = imgl2r (bim, j)
	    obuf = rgbbuf
	    if (logmap) {
		do i = 1, nc {
		    v = max (1., min (10., 1. + (Memr[rbuf] - rz1) * rdz))
		    Memc[obuf+2] = max (0, min (255, nint (log10 (v) * 255)))
		    v = max (1., min (10., 1. + (Memr[gbuf] - gz1) * gdz))
		    Memc[obuf+1] = max (0, min (255, nint (log10 (v) * 255)))
		    v = max (1., min (10., 1. + (Memr[bbuf] - bz1) * bdz))
		    Memc[obuf] = max (0, min (255, nint (log10 (v) * 255)))
		    rbuf = rbuf + 1
		    gbuf = gbuf + 1
		    bbuf = bbuf + 1
		    obuf = obuf + 3
		}
	    } else {
		do i = 1, nc {
		    Memc[obuf+2] = max (0, min (255,
			nint ((Memr[rbuf] - rz1) * rdz)))
		    Memc[obuf+1] = max (0, min (255,
			nint ((Memr[gbuf] - gz1) * gdz)))
		    Memc[obuf] = max (0, min (255,
			nint ((Memr[bbuf] - bz1) * bdz)))
		    rbuf = rbuf + 1
		    gbuf = gbuf + 1
		    bbuf = bbuf + 1
		    obuf = obuf + 3
		}
	    }
	    call chrpak (Memc[rgbbuf], 1, Memc[rgbbuf], 1, ncrgb)
	    call write (rgb, Memc[rgbbuf], ncrgb / SZB_CHAR)
	}
		
	call close (rgb)
	call imunmap (bim)
	call imunmap (gim)
	call imunmap (rim)

	call sfree (sp)
end
