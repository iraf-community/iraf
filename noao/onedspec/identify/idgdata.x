include	<imhdr.h>
include	<imio.h>
include	<pkg/gtools.h>
include	<smw.h>
include	<units.h>
include	"identify.h"

define	SZ_TITLE	320	# Size of long string for title.

# ID_GDATA -- Get image data.

procedure id_gdata (id)

pointer	id				# ID pointer

int	i, np1
pointer	sp, str, im, mw, sh

double	smw_c1trand() 
errchk	shdr_open

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
	if (ID_UN(id) != NULL) {
	    iferr (call shdr_units (sh, UN_UNITS(ID_UN(id))))
		;
	}
	ID_AP(id,1) = AP(sh)
	ID_AP(id,2) = ID_LINE(id,2)
	ID_NPTS(id) = SN(sh)
	call id_dbsection (id, ID_IMAGE(id), ID_AP(id,1),
	     ID_SECTION(id), ID_LENSTRING)
	call sprintf (Memc[str], SZ_TITLE, "identify %s%s\n%s")
	    call pargstr (ID_IMAGE(id))
	    call pargstr (ID_SECTION(id))
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

	ID_NEWGRAPH(id) = YES
	ID_NEWCV(id) = YES

	call sfree (sp)
end
