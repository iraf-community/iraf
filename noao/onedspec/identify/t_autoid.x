include	<error.h>
include	<fset.h>
include	<gset.h>
include	<pkg/gtools.h>
include	<smw.h>
include	"identify.h"

define	ICFITHELP	"noao$lib/scr/idicgfit.key"


# T_AUTOIDENTIFY -- Automatically identify spectral features.

procedure t_autoidentify ()

int	list			# List of images
int	interactive		# Examine identifications interactively?
int	dbwrite			# Write database results?

int	i, fd, hdr, hdr1
pointer	sp, str, aid, id

int	clgeti(), clgwrd(), nscan(), open(), nowhite()
int	imtopenp(), imtgetim(), id_dbcheck()
bool	clgetb(), aid_autoid()
real	clgetr()
pointer	gopen(), gt_init(), un_open()
errchk	open, id_mapll, aid_autoid, aid_init, reidentify

define	done_	10

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Initialize data structures.  Note the AID structure is initialized
	# with CL queries to the AIDPARS pset.

	aid = NULL
	call aid_init (aid, "aidpars")
	call id_init (id)

	# Get query parameters.
	list = imtopenp ("images")
	call aid_sets (aid, "crval", "CL crval")
	call aid_sets (aid, "cdelt", "CL cdelt")

	# Get other parameters and IDENITFY set data structures.
	ID_NSUM(id,1) = clgeti ("nsum")
	call gargi (ID_NSUM(id,2))
	if (nscan() != 2)
	    ID_NSUM(id,2) = ID_NSUM(id,1)
	ID_NSUM(id,1) = max (1, ID_NSUM(id,1))
	ID_NSUM(id,2) = max (1, ID_NSUM(id,2))
	ID_MAXFEATURES(id) = clgetr ("aidpars.ntarget")
	ID_MINSEP(id) = clgetr ("minsep")
	ID_FTYPE(id) = clgwrd ("ftype", Memc[str], SZ_LINE, FTYPES)
	ID_FWIDTH(id) = clgetr ("fwidth")
	ID_CRADIUS(id) = clgetr ("cradius")
	ID_THRESHOLD(id) = clgetr ("threshold")
	ID_MATCH(id) = clgetr ("match")
	ID_ZWIDTH(id) = clgetr ("identify.zwidth")
	ID_LABELS(id) = 1

	call clgstr ("database", ID_DATABASE(id), ID_LENSTRING)
	dbwrite = clgwrd ("dbwrite", Memc[str], SZ_FNAME, "|no|yes|NO|YES|")
	if (dbwrite == 1)
	    dbwrite = 3

	call clgstr ("coordlist", ID_COORDLIST(id), ID_LENSTRING)
	if (nowhite (ID_COORDLIST(id), ID_COORDLIST(id), ID_LENSTRING) == 0) {
	    call clgstr ("coordlist.p_prompt", Memc[str], SZ_LINE)
	    call printf (Memc[str])
	    call flush (STDOUT)
	    call clgstr ("query", ID_COORDLIST(id), ID_LENSTRING)
	}
	call clgstr ("units", Memc[str], SZ_LINE)
	call xt_stripwhite (Memc[str])
	if (Memc[str] != EOS)
	    ID_UN(id) = un_open (Memc[str])
	call id_mapll (id)
	if (ID_LL(id) == NULL)
	    call error (0, "Required coordinate line list not found")

	# Dispersion fitting parameters.
        call ic_open (ID_IC(id))
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
        call ic_puti (ID_IC(id), "key", 5)
	call ic_pstr (ID_IC(id), "help", ICFITHELP)

	# Interactive, graphics, and output parameters.
	interactive = clgwrd ("interactive", Memc[str], SZ_FNAME,
	    "|no|yes|NO|YES|")
	switch (interactive) {
	case 1, 3:
	    ID_GP(id) = NULL
	    interactive = 3
	case 2, 4:
	    # Open graphics
	    call clgstr ("graphics", Memc[str], SZ_LINE)
	    ID_GP(id) = gopen (Memc[str], NEW_FILE+AW_DEFER, STDGRAPH)
	}

	ID_GT(id) = gt_init()
	call gt_sets (ID_GT(id), GTTYPE, "line")
	call fseti (STDOUT, F_FLUSHNL, YES)
	hdr = YES
	hdr1 = YES

	# Log and plot files.
	call calloc (ID_LOGFILES(id), 4, TY_INT)
	if (clgetb ("verbose"))
	    Memi[ID_LOGFILES(id)] = STDOUT
	call clgstr ("logfile", Memc[str], SZ_LINE)
	if (nowhite (Memc[str], Memc[str], SZ_FNAME) > 0) {
	    fd = open (Memc[str], APPEND, TEXT_FILE)
	    Memi[ID_LOGFILES(id)+1] = fd
	}
	call clgstr ("plotfile", Memc[str], SZ_LINE)
	if (nowhite (Memc[str], Memc[str], SZ_FNAME) > 0) {
	    fd = open (Memc[str], APPEND, BINARY_FILE)
	    Memi[ID_LOGFILES(id)+2] = fd
	}

	# Expand the image template and identify features.
	while (imtgetim (list, ID_IMAGE(id), ID_LENSTRING) != EOF) {
	    # Initialize.
	    iferr (call id_map(id)) {
		call erract (EA_WARN)
		next
	    }
	    if (!clgetb ("overwrite")) {
		if (id_dbcheck (id, ID_IMAGE(id), ID_AP(id,1)) == YES) {
		    if (Memi[ID_LOGFILES(id)] != NULL) {
			if (ID_GP(id) != NULL)
			    call gdeactivate (ID_GP(id), 0)
			call fprintf (Memi[ID_LOGFILES(id)],
			    "  %s%s%24t Database entry already exists\n")
			    call pargstr (ID_IMAGE(id))
			    call pargstr (ID_SECTION(id))
		    }
		    goto done_
		}
	    }

	    call id_gdata(id)
	    call id_fitdata(id)
	    call ic_putr (ID_IC(id), "xmin", real (PIXDATA(id,1)))
	    call ic_putr (ID_IC(id), "xmax", real (PIXDATA(id,ID_NPTS(id))))

	    call dcvfree (ID_CV(id))
	    ID_SHIFT(id) = 0.
	    ID_NFEATURES(id) = 0

	    # Automatically identify the features.
	    if (aid_autoid (id, aid))
		ID_NEWDBENTRY(id) = YES
	    else if (Memi[ID_LOGFILES(id)] == NULL)
		call aid_log (id, STDOUT, NO)
	    call aid_log (id, Memi[ID_LOGFILES(id)], hdr)
	    call aid_log (id, Memi[ID_LOGFILES(id)+1], hdr1)

	    # Enter interactive identification mode if desired.
	    if (interactive != 3) {
		if (interactive != 4) {
		    repeat {
			call clgstr ("interactive.p_prompt", Memc[str],
			    SZ_FNAME)
			call printf ("%s%s: %s")
			    call pargstr (ID_IMAGE(id))
			    call pargstr (ID_SECTION(id))
			    call pargstr (Memc[str])
			call flush (STDOUT)
			if (interactive == 1)
			    call clpstr ("query", "no")
			else
			    call clpstr ("query", "yes")
			ifnoerr (interactive = clgwrd ("query", Memc[str],
			    SZ_FNAME, "|no|yes|NO|YES|"))
			break
		    }
		}
		if (interactive == 2 || interactive == 4) {
		    call reidentify (id)
		    call gdeactivate (ID_GP(id), 0)
		}
	    }

	    # Write results to the database.
	    if (ID_NEWDBENTRY(id) == YES) {
		if (dbwrite == 1 || dbwrite == 2) {
		    repeat {
			call clgstr ("dbwrite.p_prompt", Memc[str], SZ_FNAME)
			call printf ("%s%s: %s")
			    call pargstr (ID_IMAGE(id))
			    call pargstr (ID_SECTION(id))
			    call pargstr (Memc[str])
			call flush (STDOUT)
			if (dbwrite == 1)
			    call clpstr ("query", "no")
			else
			    call clpstr ("query", "yes")
			ifnoerr (dbwrite = clgwrd ("query", Memc[str],
			    SZ_FNAME, "|no|yes|NO|YES|"))
			    break
		    }
		}
		if (dbwrite == 2 || dbwrite == 4)
		    call id_dbwrite (id, ID_IMAGE(id), ID_AP(id,1), NO)
	    }

done_	    # Close the database, image, and spectrum data structures.
	    if (ID_DT(id) != NULL)
		call dtunmap (ID_DT(id))
	    call smw_close (MW(ID_SH(id)))
	    call imunmap (IM(ID_SH(id)))
	    call shdr_close (ID_SH(id))
	}

	# Finish up.
	do i = 1, 3 {
	    fd = Memi[ID_LOGFILES(id)+i-1]
	    if (fd != NULL)
		call close (fd)
	}
	call mfree (ID_LOGFILES(id), TY_INT)
	if (ID_GP(id) != NULL)
	    call gclose (ID_GP(id))
	call smw_daxis (NULL, NULL, 0, 0, 0)
	call imtclose (list)
	if (aid != NULL)
	    call aid_free (aid)
	call id_free (id)
	call sfree (sp)
end
