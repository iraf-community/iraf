include	<mach.h>
include	<pkg/gtools.h>
include	"identify.h"

# T_IDENTIFY -- Identify features and determine dispersion solutions

procedure t_identify ()

begin
	call identify (IDENTIFY)
end


# T_RVIDLINES -- Identify features and determine radial velocities

procedure t_rvidlines ()

begin
	call identify (RVIDLINES)
end


# IDENTIFY -- Initialize task

procedure identify (taskid)

int	taskid			#I Task ID

int	list, clscan(), clgeti(), clgwrd(), nscan(), imtopenp(), imtgetim()
real	clgetr()
pointer	sp, str, id, gt_init()

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Allocate the basic data structure.
	call id_init (id, taskid)

	# Get task parameters.
	list = imtopenp ("images")
	if (clscan ("nsum") != EOF) {
	    call gargi (ID_NSUM(id,1))
	    call gargi (ID_NSUM(id,2))
	    if (nscan() == 0)
		call error (1, "Error in 'nsum' parameter")
	    if (nscan() == 1)
		ID_NSUM(id,2) = ID_NSUM(id,1)
	    ID_NSUM(id,1) = max (1, ID_NSUM(id,1))
	    ID_NSUM(id,2) = max (1, ID_NSUM(id,2))
	}
	ID_MAXFEATURES(id) = clgeti ("maxfeatures")
	ID_MINSEP(id) = clgetr ("minsep")
	ID_MATCH(id) = clgetr ("match")
	ID_ZWIDTH(id) = clgetr ("zwidth")
	ID_FTYPE(id) = clgwrd ("ftype", Memc[str], SZ_LINE, FTYPES)
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
	ID_REDSHIFT(id) = 0.

	# Initialize ICFIT
	call ic_open (ID_IC(id))
	if (ID_TASK(id) == IDENTIFY) {
	    call clgstr ("function", Memc[str], SZ_LINE)
	    call ic_pstr (ID_IC(id), "function", Memc[str])
	    call ic_puti (ID_IC(id), "order", clgeti ("order"))
	    call clgstr ("sample", Memc[str], SZ_LINE)
	    call ic_pstr (ID_IC(id), "sample", Memc[str])
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
	}

	# Get the line list.
	call id_mapll (id)

	# Expand the image template and identify features in each image.
	while (imtgetim (list, Memc[ID_IMAGE(id)], SZ_FNAME) != EOF)
	    call id_identify (id)

	# Finish up.
	call smw_daxis (NULL, NULL, 0, 0, 0)
	call id_free (id)
	call imtclose (list)
	call sfree (sp)
end
