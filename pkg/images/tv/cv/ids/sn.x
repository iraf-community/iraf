# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <error.h>
include <fio.h>
include <fset.h>
include "ids.h"
include <gki.h>
include <gset.h>
include	<imhdr.h>

define	DIM	512
define	MCXSCALE	64
define	MCYSCALE	64

# snap

procedure t_im()

pointer	gp
char	device[SZ_FNAME]
char	cjunk[SZ_FNAME]

pointer	gopen()
int	dd[LEN_GKIDD]

int	key, fnum, zfac
int	ps, pe
real	x, y
real	xjunk, yjunk
int	clgeti
bool	image, clgetb

begin
	call clgstr("device", device, SZ_FNAME)
	call ids_open (device, dd)
	call gki_inline_kernel (STDIMAGE, dd)
	gp = gopen ( device, NEW_FILE, STDIMAGE)

	call fseti (STDIMAGE, F_TYPE, SPOOL_FILE)
	call fseti (STDIMAGE, F_CANCEL, OK)
	call ids_grstream (STDIMAGE)

	# read first to clear box
	call gseti(gp, G_CURSOR, IDS_BUT_RD)
	call ggcur(gp, xjunk, yjunk, key)

	repeat {
	    if (clgetb ("done?"))
		break

	    zfac = clgeti ("zoom factor")

	    call clgstr ("Set zoom center, press <cr>", cjunk, SZ_FNAME)
	    call gseti (gp, G_CURSOR, 1)
	    call ggcur(gp, x, y, key)
	    call zm(gp, zfac, x, y)

	    image = clgetb("Do you want a picture?")
	    if (image)
		call snapi (gp)
	    else {
	        repeat {
		    ps = clgeti ("starting line")
		    if ( ps == -1)
		        break
		    pe = clgeti ("ending line")
	            call snap (gp, ps, pe)
	        }
	    }
	}


	# all done
	call gclose ( gp )
	call ids_close
end

# zoom

procedure zm(gp, pow, x, y)

int	pow
pointer	gp
real	x, y

short	data[9]

begin
	    data[1] = IDS_ZOOM
	    data[2] = IDS_WRITE
	    data[3] = 3
	    data[4] = IDS_EOD
	    data[5] = IDS_EOD
	    data[6] = 0
	    data[7] = 2**(pow-1)
	    data[8] = x * GKI_MAXNDC
	    data[9] = y * GKI_MAXNDC
	    call gescape ( gp, IDS_CONTROL, data[1], 9)
end

procedure snap (gp, ps, pe)

pointer	gp
int	ps, pe

real	y
short	data[7]
pointer	sp
pointer	sndata
int	i,j

begin
	call smark (sp)
	data[1] = IDS_SNAP
	data[2] = IDS_WRITE
	data[3] = 1
	data[4] = IDS_EOD
	data[5] = IDS_EOD
	data[6] = 0
	data[7] = IDS_SNAP_RGB
	call gescape (gp, IDS_CONTROL, data, 7)

	if (pe < ps) {
	    call eprintf("Can't handle ending position < start \n")
	    return
	}

	call salloc ( sndata, DIM, TY_SHORT)
	call eprintf ("snapping from %d through %d\n")
	    call pargi (ps)
	    call pargi (pe)
	call eprintf ("data values 0-5 255 256 511\n")
	do i = ps, pe {
	    y = real(i)*MCYSCALE / GKI_MAXNDC.
	    call ggcell (gp, Mems[sndata], DIM, 1, 0.0, y, 1.0, y)
	    call eprintf ("r%3d data:")
		call pargi (i)
	    call eprintf (" %5d %5d %5d %5d %5d %5d %5d %5d %5d\n")
		do j = 0, 5
	            call pargs (Mems[sndata+j])
	        call pargs (Mems[sndata+255])
	        call pargs (Mems[sndata+256])
	        call pargs (Mems[sndata+511])
	}
	
	data[1] = IDS_R_SNAPDONE
	call gescape (gp, IDS_RESET, data, 1)

	call sfree (sp)
end

procedure snapi (gp)

pointer	gp

real	y
short	data[7]
pointer	im, immap(), impl2s()
char	fname[SZ_FNAME]
int	i

begin
	call clgstr ("file", fname, SZ_FNAME)
	im = immap(fname, NEW_FILE, 0)
	IM_PIXTYPE(im) = TY_SHORT
	IM_LEN(im,1) = DIM
	IM_LEN(im,2) = DIM

	data[1] = IDS_SNAP
	data[2] = IDS_WRITE
	data[3] = 1
	data[4] = IDS_EOD
	data[5] = IDS_EOD
	data[6] = 0
	data[7] = IDS_SNAP_RGB
	call gescape (gp, IDS_CONTROL, data, 7)

	do i = 0, 511 {
	    if ( mod(i,52) == 0) {
		call eprintf ("%d ")
		    call pargi (100*i/DIM)
		    call flush (STDERR)
	    }
	    y = real(i)*MCYSCALE / GKI_MAXNDC.
	    call ggcell (gp, Mems[impl2s(im,i+1)], 512, 1, 0.0, y, 1.0, y)
	}
	call eprintf ("\n")
	
	call imunmap(im)
	data[1] = IDS_R_SNAPDONE
	call gescape (gp, IDS_RESET, data, 1)
end
