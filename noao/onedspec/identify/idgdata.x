include	<imhdr.h>
include	<imio.h>
include	<pkg/gtools.h>
include	"identify.h"
include	"../shdr.h"

define	SZ_TITLE	320	# Size of long string for title.

# ID_GDATA -- Get image data.

procedure id_gdata (id)

pointer	id				# ID pointer

int	i
pointer	sp, str, im, mw, sh

double	mw_c1trand() 
errchk	shdr_open

begin
	call smark (sp)
	call salloc (str, SZ_TITLE, TY_CHAR)

	sh = ID_SH(id)
	im = IM(sh)
	mw = MW(sh)

	# If format is multispec then header info depends on line.
	call shdr_open (im, mw, ID_LINE(id), 1, INDEFI, SHDATA, sh)
	ID_AP(id) = AP(sh)
	ID_NPTS(id) = SN(sh)
	if (FORMAT(sh) == MULTISPEC) {
	    call sprintf (Memc[ID_SECTION(id)], SZ_FNAME, " - Ap %d")
		call pargi (AP(sh))
	    call sprintf (Memc[str], SZ_TITLE, "identify %s%s\n%s")
		call pargstr (Memc[ID_IMAGE(id)])
		call pargstr (Memc[ID_SECTION(id)])
		call pargstr (TITLE(sh))
	} else {
	    Memc[ID_SECTION(id)] = EOS
	    if (NDIM(sh) > 1) {
	        switch (DAXIS(sh)) {
	        case 1:
	            call sprintf (Memc[ID_SECTION(id)], SZ_FNAME, "[*,%d]")
		        #call pargi (INDEX1(sh))
			call pargi (ID_AP(id))
	        case 2:
	            call sprintf (Memc[ID_SECTION(id)], SZ_FNAME, "[%d,*]")
		        #call pargi (INDEX1(sh))
			call pargi (ID_AP(id))
	        }
	    }
	    call sprintf (Memc[str], SZ_TITLE, "identify %s%s\n%s")
	        call pargstr (Memc[ID_IMAGE(id)])
	        call pargstr (Memc[ID_SECTION(id)])
	        call pargstr (TITLE(sh))
	}
	call gt_sets (ID_GT(id), GTTITLE, Memc[str])

	# Free previous vectors and allocate new vectors.
	call mfree (ID_PIXDATA(id), TY_DOUBLE)

	call malloc (ID_PIXDATA(id), ID_NPTS(id), TY_DOUBLE)

	# Set the physical coordinates.
	do i = 1, ID_NPTS(id)
	    PIXDATA(id,i) = mw_c1trand (ID_LP(id), double(i))

	# Set the image data
	ID_IMDATA(id) = SY(sh)

	ID_NEWGRAPH(id) = YES
	ID_NEWCV(id) = YES

	call sfree (sp)
end
