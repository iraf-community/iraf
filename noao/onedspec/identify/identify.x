include	<mach.h>
include	<pkg/gtools.h>
include	<pkg/center1d.h>
include	"identify.h"

# T_IDENTIFY -- Identify features

procedure t_identify ()

char	images[SZ_LINE]			# Images
char	str[SZ_LINE]

int	list, clgeti(), clgwrd(), imtopen(), id_getim()
real	clgetr()
pointer	id, gt_init(), id_ll

begin
	# Allocate the basic data structure.
	call id_init (id)

	# Get task parameters.

	call clgstr ("images", images, SZ_LINE)
	call clgstr ("section", Memc[ID_SECTION(id)], SZ_FNAME)
	ID_NSUM(id) = clgeti ("nsum")
	ID_MAXFEATURES(id) = clgeti ("maxfeatures")
	ID_MINSEP(id) = clgetr ("minsep")
	ID_MATCH(id) = clgetr ("match")
	ID_ZWIDTH(id) = clgetr ("zwidth")
	ID_FTYPE(id) = clgwrd ("ftype", str, SZ_LINE, FTYPES)
	ID_FWIDTH(id) = clgetr ("fwidth")
	ID_CRADIUS(id) = clgetr ("cradius")
	ID_THRESHOLD(id) = clgetr ("threshold")
	call clgstr ("database", Memc[ID_DATABASE(id)], SZ_FNAME)
	call clgstr ("coordlist", Memc[ID_COORDLIST(id)], SZ_FNAME)
	ID_LABELS(id) = 1

	# Initialize features data structure.

	ID_GT(id) = gt_init()
	call gt_sets (ID_GT(id), GTTYPE, "line")
	ID_CV(id) = NULL
	ID_CURRENT(id) = 0
	ID_SHIFT(id) = 0.

	# Initialize ICFIT
	call ic_open (ID_IC(id))
	call clgstr ("function", str, SZ_LINE)
	call ic_pstr (ID_IC(id), "function", str)
	call ic_puti (ID_IC(id), "order", clgeti ("order"))
	call clgstr ("sample", str, SZ_LINE)
	call ic_pstr (ID_IC(id), "sample", str)
	call ic_puti (ID_IC(id), "naverage", 1)
	call ic_puti (ID_IC(id), "niterate", clgeti ("niterate"))
	call ic_putr (ID_IC(id), "low", clgetr ("low_reject"))
	call ic_putr (ID_IC(id), "high", clgetr ("high_reject"))
	call ic_putr (ID_IC(id), "grow", clgetr ("grow"))
	call ic_pstr (ID_IC(id), "xlabel", "Feature positions")
	call ic_pstr (ID_IC(id), "xunits", "pixels")
	call ic_pstr (ID_IC(id), "ylabel", "")
	call ic_pkey (ID_IC(id), 1, 'y', 'x')
	call ic_pkey (ID_IC(id), 2, 'y', 'v')
	call ic_pkey (ID_IC(id), 3, 'y', 'r')
	call ic_pkey (ID_IC(id), 4, 'y', 'd')
	call ic_pkey (ID_IC(id), 5, 'y', 'n')
	call ic_puti (ID_IC(id), "key", 3)

	# Get the line list.

	call id_mapll (id_ll, Memc[ID_COORDLIST(id)])

	# Expand the image template and identify features in each image.

	list = imtopen (images)
	while (id_getim (list, Memc[ID_IMAGE(id)], SZ_FNAME) != EOF)
	    call id_identify (id, id_ll)

	# Finish up.

	call id_unmapll (id_ll)
	call gt_free (ID_GT(id))
	call dcvfree (ID_CV(id))
	call ic_closed (ID_IC(id))
	call id_free (id)
	call imtclose (list)
end
