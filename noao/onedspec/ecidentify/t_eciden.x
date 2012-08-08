include	<mach.h>
include	<pkg/gtools.h>
include	<pkg/center1d.h>
include	"ecidentify.h"

# T_ECIDENTIFY -- Identify features in echelle format data.
#
# The input data must be in the echelle format produced by APEXTRACT.

procedure t_ecidentify ()

int	images
pointer	ec, gopen(), gt_init(), un_open()
int	clgeti(), clgwrd(), imtopenp(), ec_getim()
real	clgetr()
double	clgetd()

begin
	# Allocate the basic data structure.
	call ec_init (ec)

	# Get task parameters.
	images = imtopenp ("images")
	EC_MAXFEATURES(ec) = clgeti ("maxfeatures")
	EC_MINSEP(ec) = clgetr ("minsep")
	EC_MATCH(ec) = clgetr ("match")
	EC_ZWIDTH(ec) = clgetr ("zwidth")
	EC_FTYPE(ec) = clgwrd ("ftype", Memc[EC_IMAGE(ec)], SZ_FNAME, FTYPES)
	EC_FWIDTH(ec) = clgetr ("fwidth")
	EC_CRADIUS(ec) = clgetr ("cradius")
	EC_THRESHOLD(ec) = clgetr ("threshold")
	call clgstr ("database", Memc[EC_DATABASE(ec)], SZ_FNAME)
	call clgstr ("coordlist", Memc[EC_COORDLIST(ec)], SZ_FNAME)

	# Get the line list.
	call clgstr ("units", Memc[EC_IMAGE(ec)], SZ_FNAME)
	call xt_stripwhite (Memc[EC_IMAGE(ec)])
	if (Memc[EC_IMAGE(ec)] != EOS)
	    EC_UN(ec) = un_open (Memc[EC_IMAGE(ec)])
	call ec_mapll (ec)

	# Initialize graphics and fitting.
	call clgstr ("function", Memc[EC_IMAGE(ec)], SZ_FNAME)
	call ecf_sets ("function", Memc[EC_IMAGE(ec)])
	call ecf_seti ("xorder", clgeti ("xorder"))
	call ecf_seti ("yorder", clgeti ("yorder"))
	call ecf_seti ("niterate", clgeti ("niterate"))
	call ecf_setd ("low", clgetd ("lowreject"))
	call ecf_setd ("high", clgetd ("highreject"))
	call ecf_seti ("xtype", 'p')
	call ecf_seti ("ytype", 'r')
	call clgstr ("graphics", Memc[EC_IMAGE(ec)], SZ_FNAME)
	EC_GP(ec) = gopen (Memc[EC_IMAGE(ec)], NEW_FILE, STDGRAPH)
	EC_GT(ec) = gt_init()
	call gt_sets (EC_GT(ec), GTTYPE, "line")

	# Identify features in each image.
	while (ec_getim (images, Memc[EC_IMAGE(ec)], SZ_FNAME) != EOF)
	    call ec_identify (ec)

	# Finish up.
	call gclose (EC_GP(ec))
	call gt_free (EC_GT(ec))
	call dgsfree (EC_ECF(ec))
	call imtclose (images)
	call ec_unmapll (ec)
	call ec_free (ec)
end
