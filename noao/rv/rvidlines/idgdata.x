include	<imhdr.h>
include	<imio.h>
include	<pkg/gtools.h>
include	<smw.h>
include	"identify.h"

define	SZ_TITLE	320	# Size of long string for title.

# ID_GDATA -- Get image data.

procedure id_gdata (id)

pointer	id				# ID pointer

int	i, np1
double	hjd
pointer	sp, str, im, mw, sh

double	smw_c1trand() 
errchk	shdr_open, id_vhelio

begin
	call smark (sp)
	call salloc (str, SZ_TITLE, TY_CHAR)

	sh = ID_SH(id)
	im = IM(sh)
	mw = MW(sh)

	# If format is multispec then header info depends on line.
	if (SMW_FORMAT(mw) == SMW_ES || SMW_FORMAT(mw) == SMW_MS)
	    ID_LINE(id,2) = 1
	call shdr_open (im, mw, ID_LINE(id,1), ID_LINE(id,2),
	    INDEFI, SHDATA, sh)
	ID_AP(id,1) = AP(sh)
	ID_AP(id,2) = ID_LINE(id,2)
	ID_NPTS(id) = SN(sh)
	call id_dbsection (id, Memc[ID_IMAGE(id)], ID_AP(id,1),
	     Memc[ID_SECTION(id)], SZ_FNAME)
	call sprintf (Memc[str], SZ_TITLE, "%s %s%s\n%s")
	    if (ID_TASK(id) == IDENTIFY)
		call pargstr ("identify")
	    else
		call pargstr ("rvidlines")
	    call pargstr (Memc[ID_IMAGE(id)])
	    call pargstr (Memc[ID_SECTION(id)])
		call pargstr (TITLE(sh))
	call gt_sets (ID_GT(id), GTTITLE, Memc[str])

	# Free previous vectors and allocate new vectors.
	call mfree (ID_PIXDATA(id), TY_DOUBLE)

	call malloc (ID_PIXDATA(id), ID_NPTS(id), TY_DOUBLE)

	# Set the physical coordinates.
	np1 = NP1(sh) - 1
	do i = 1, ID_NPTS(id)
	    PIXDATA(id,i) = smw_c1trand (ID_LP(id), double(i+np1))

	# Set the image data
	ID_IMDATA(id) = SY(sh)

	# Set the heliocentric correction.
	if (ID_TASK(id) == RVIDLINES) {
	    call id_vhelio (im, ID_ZHELIO(id), hjd, NULL)
	    ID_ZHELIO(id) = ID_ZHELIO(id) / VLIGHT
	} else
	    ID_ZHELIO(id) = 0

	ID_NEWGRAPH(id) = YES
	ID_NEWCV(id) = YES

	call sfree (sp)
end
