include	<error.h>
include	<mach.h>
include	"apertures.h"

# Data structure for user aperture id table.
define	IDS_LEN		4		# Length of ID structure
define	IDS_NIDS	Memi[$1]	# Number of aperture IDs
define	IDS_APS		Memi[$1+1]	# Aperture numbers (pointer)
define	IDS_BEAMS	Memi[$1+2]	# Beam numbers (pointer)
define	IDS_TITLES	Memi[$1+3]	# Titles (pointer)

# AP_GIDS -- Get user aperture ID's.

procedure ap_gids (ids)

pointer	ids		# ID structure

int	nids, ap, beam, fd, nalloc
double	ra, dec
pointer	sp, key, str, aps, beams, titles, im, list

int	nowhite(), open(), fscan(), nscan()
pointer	immap(), imofnlu(), imgnfn()
errchk	open

begin
	call smark (sp)
	call salloc (key, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	nids = 0
	nalloc = 0

	call apgstr ("apidtable", Memc[key], SZ_FNAME)
	if (nowhite (Memc[key], Memc[key], SZ_FNAME) > 0) {
	    iferr {
		# Read aperture information from an image.
		ifnoerr (im = immap (Memc[key], READ_ONLY, 0)) {
		    list = imofnlu (im, "SLFIB[0-9]*")
		    while (imgnfn (list, Memc[key], SZ_FNAME) != EOF) {
			call imgstr (im, Memc[key], Memc[str], SZ_LINE)
			call sscan (Memc[str])
			call gargi (ap)
			if (nscan() == 0)
			    next
			if (ap < 1) {
			    call imcfnl (list)
			    call imunmap (im)
			    call error (1,
				"Aperture numbers in apidtable must be > 0")
			}
			if (nalloc == 0) {
			    nalloc = 50
			    call malloc (aps, nalloc, TY_INT)
			    call malloc (beams, nalloc, TY_INT)
			    call malloc (titles, nalloc, TY_POINTER)
			} else if (nids == nalloc) {
			    nalloc = nalloc + 50
			    call realloc (aps, nalloc, TY_INT)
			    call realloc (beams, nalloc, TY_INT)
			    call realloc (titles, nalloc, TY_POINTER)
			}
			Memi[aps+nids] = ap
			call gargi (Memi[beams+nids])
			call gargd (ra)
			call gargd (dec)
			if (nscan() != 4) {
			    call reset_scan ()
			    call gargi (ap)
			    call gargi (beam)
			    Memc[str] = EOS
			    call gargstr (Memc[str], SZ_LINE)	
			    call xt_stripwhite (Memc[str])
			    if (Memc[str] == EOS)
				Memi[titles+nids] = NULL
			    else {
				call malloc (Memi[titles+nids], SZ_APTITLE,
				    TY_CHAR)
				call strcpy (Memc[str], Memc[Memi[titles+nids]],
				    SZ_APTITLE)
			    }
			} else {
			    Memc[str] = EOS
			    call gargstr (Memc[str], SZ_LINE)	
			    call xt_stripwhite (Memc[str])
			    call malloc (Memi[titles+nids], SZ_APTITLE, TY_CHAR)
			    if (Memc[str] == EOS) {
				call sprintf (Memc[Memi[titles+nids]],
				    SZ_APTITLE, "(%.2h %.2h)")
				    call pargd (ra)
				    call pargd (dec)
			    } else {
				call sprintf (Memc[Memi[titles+nids]],
				    SZ_APTITLE, "%s (%.2h %.2h)")
				    call pargstr (Memc[str])
				    call pargd (ra)
				    call pargd (dec)
			    }
			}
			nids = nids + 1
		    }	
		    call imcfnl (list)
		    call imunmap (im)

		# Read aperture information from a file.
		} else {
		    fd = open (Memc[key], READ_ONLY, TEXT_FILE)
		    while (fscan (fd) != EOF) {
			call gargi (ap)
			if (nscan() == 0)
			    next
			if (ap < 1) {
			    call close (fd)
			    call error (1,
				"Aperture numbers in apidtable must be > 0")
			}
			if (nalloc == 0) {
			    nalloc = 50
			    call malloc (aps, nalloc, TY_INT)
			    call malloc (beams, nalloc, TY_INT)
			    call malloc (titles, nalloc, TY_POINTER)
			} else if (nids == nalloc) {
			    nalloc = nalloc + 50
			    call realloc (aps, nalloc, TY_INT)
			    call realloc (beams, nalloc, TY_INT)
			    call realloc (titles, nalloc, TY_POINTER)
			}
			Memi[aps+nids] = ap
			Memi[beams+nids] = ap
			Memc[str] = EOS
			call gargi (beam)
			if (nscan() == 2)
			    Memi[beams+nids] = beam
			call gargstr (Memc[str], SZ_LINE)	
			call xt_stripwhite (Memc[str])
			if (Memc[str] == EOS)
			    Memi[titles+nids] = NULL
			else {
			    call malloc (Memi[titles+nids], SZ_APTITLE, TY_CHAR)
			    call strcpy (Memc[str], Memc[Memi[titles+nids]],
				SZ_APTITLE)
			}
			nids = nids + 1
		    }	
		    call close (fd)
		}
	    } then
		call erract (EA_WARN)
	}

	if (nalloc > nids) {
	    call realloc (aps, nids, TY_INT)
	    call realloc (beams, nids, TY_INT)
	    call realloc (titles, nids, TY_INT)
	}

	if (nids > 0) {
	    call malloc (ids, IDS_LEN, TY_STRUCT)
	    IDS_NIDS(ids) = nids
	    IDS_APS(ids) = aps
	    IDS_BEAMS(ids) = beams
	    IDS_TITLES(ids) = titles
	}

	call sfree (sp)
end


procedure ap_fids (ids)

pointer	ids		# ID structure
int	i

begin
	if (ids != NULL) {
	    do i = 1, IDS_NIDS(ids)
		call mfree (Memi[IDS_TITLES(ids)+i-1], TY_CHAR)
	    call mfree (IDS_APS(ids), TY_INT)
	    call mfree (IDS_BEAMS(ids), TY_INT)
	    call mfree (IDS_TITLES(ids), TY_POINTER)
	    call mfree (ids, TY_STRUCT)
	}
end



# AP_IDS -- Set aperture IDs
# Do not allow negative or zero aperture numbers.

procedure ap_ids (aps, naps, ids)

pointer	aps[ARB]	# Aperture pointers
int	naps		# Number of apertures
int	ids		# ID structure

int	i, j, k, l, m,  axis, nids, ap, beam, skip, nused
real	maxsep, apgetr()
pointer	sp, used, a, b

begin
	if (naps < 1)
	    return

	axis = AP_AXIS(aps[1])
	maxsep = apgetr ("maxsep")

	# Dereference ID structure pointers.
	if (ids != NULL) {
	    nids = IDS_NIDS(ids)
	    a = IDS_APS(ids)
	    b = IDS_BEAMS(ids)
	} else
	    nids = 0

	# Make a list of used aperture numbers
	call smark (sp)
	call salloc (used, naps, TY_INT)
	nused = 0
	do i = 1, naps
	    if (!IS_INDEFI(AP_ID(aps[i]))) {
		Memi[used+nused] = AP_ID(aps[i])
		nused = nused + 1
	    }

	# Find first aperture with a defined aperture number.
	for (i=1; i<=naps && IS_INDEFI(AP_ID(aps[i])); i=i+1)
	    ;

	# If there are no defined aperture numbers start with 1 or first
	# aperture in the ID table.

	if (i > naps) {
	    i = 1
	    if (nids > 0) {
		ap = Memi[a]
		beam = Memi[b]
	    } else {
		ap = i
		beam = ap
	    }
	    AP_ID(aps[i]) = ap
	    AP_BEAM(aps[i]) = beam
	    Memi[used+nused] = ap
	    nused = nused + 1
	} else {
	    ap = AP_ID(aps[i])
	    for (l = 1; l <= nids && ap != Memi[a+l-1]; l = l + 1)
		;
	    if (l <= nids)
		AP_BEAM(aps[i]) = Memi[b+l-1]
	    else
		AP_BEAM(aps[i]) = ap
	}

	# Work backwards through the undefined apertures.
	for (j = i - 1; j > 0; j = j - 1) {
	    skip = abs (AP_CEN(aps[j],axis)-AP_CEN(aps[j+1],axis)) / maxsep
	    if (ids != NULL) {
	        ap = AP_ID(aps[j+1])
		for (l = 1; l <= nids && ap != Memi[a+l-1]; l = l + 1)
		    ;
		if (nids <= naps)
		    skip = 0
		m = l - skip
		if (l > nids) {
		    l = 1
		    for (k = 2; k <= nids; k = k + 1)
		        if (abs (ap - Memi[a+k-1]) < abs (ap - Memi[a+l-1]))
			    l = k
		    m = l - skip + 1
		}
		repeat {
		    m = m - 1
		    if (m > 0) {
			ap = Memi[a+m-1]
			beam = Memi[b+m-1]
		    } else {
			ap = Memi[a+l-1] + m
			beam = max (0, Memi[b+l-1] + m)
		    }
		    if (ap == 0)
			next
		    for (k = 0; k < nused && abs(ap) != Memi[used+k]; k = k + 1)
			;
		    if (k == nused)
			break
		 }
	    } else {
	        ap = AP_ID(aps[j+1]) - skip
	        repeat {
		    ap = ap - 1
		    beam = abs (ap)
		    if (ap == 0)
			next
		    for (k = 0; k < nused && abs(ap) != Memi[used+k]; k = k + 1)
		        ;
		    if (k == nused)
		        break
	        }
	    }
	    ap = abs (ap)
	    AP_ID(aps[j]) = ap
	    AP_BEAM(aps[j]) = beam
	    Memi[used+nused] = ap
	    nused = nused + 1
	}

	# Work forwards through the undefined apertures.
	for (i = i + 1; i <= naps; i = i + 1) {
	    if (IS_INDEFI(AP_ID(aps[i]))) {
	        skip = abs (AP_CEN(aps[i],axis)-AP_CEN(aps[i-1],axis)) / maxsep
	        if (nids > 0) {
	            ap = AP_ID(aps[i-1])
		    for (l = 1; l <= nids && ap != Memi[a+l-1]; l = l + 1)
		        ;
		    if (nids <= naps)
		        skip = 0
		    m = l + skip
		    if (l > nids) {
		        l = 1
		        for (k = 2; k <= nids; k = k + 1)
		            if (abs (ap-Memi[a+k-1]) < abs (ap-Memi[a+l-1]))
			        l = k
		        m = l + skip - 1
		    }
		    m = nids - m + 1
		    repeat {
		        m = m - 1
		        if (m > 0) {
			    ap = Memi[a+nids-m]
			    beam = Memi[b+nids-m]
			} else {
			    ap = Memi[a+l-1] - m
			    beam = max (0, Memi[b+l-1] - m)
			}
			if (ap == 0)
			    next
		        for (k=0; k<nused && abs(ap)!=Memi[used+k]; k=k+1)
			    ;
		        if (k == nused)
			    break
		     }
	        } else {
	            ap = AP_ID(aps[i-1]) + skip
	            repeat {
		        ap = ap + 1
		        beam = abs (ap)
			if (ap == 0)
			    next
		        for (k=0; k<nused && abs(ap)!=Memi[used+k]; k=k+1)
		            ;
		        if (k == nused)
		            break
	            }
	        }
		ap = abs(ap)
	        AP_ID(aps[i]) = ap
	        AP_BEAM(aps[i]) = beam
	        Memi[used+nused] = ap
	        nused = nused + 1
	    }
	}

	call sfree (sp)
end


procedure ap_titles (aps, naps, ids)

pointer	aps[ARB]	# Aperture pointers
int	naps		# Number of apertures
pointer	ids		# ID structure

int	i, j, nids
pointer	a, titles, title

begin
	if (ids == NULL)
	    return

	nids = IDS_NIDS(ids)
	a = IDS_APS(ids)
	titles = IDS_TITLES(ids)

	do i = 1, naps {
	    if (AP_TITLE(aps[i]) != NULL)
		next
	    do j = 1, nids {
		if (AP_ID(aps[i]) == Memi[a+j-1]) {
		    title = Memi[titles+j-1]
	            if (title != NULL) {
	                if (AP_TITLE(aps[i]) == NULL)
		            call malloc (AP_TITLE(aps[i]), SZ_APTITLE, TY_CHAR)
	                call strcpy (Memc[title], Memc[AP_TITLE(aps[i])],
			    SZ_APTITLE)
	            } else if (AP_TITLE(aps[i]) != NULL)
	                call mfree (AP_TITLE(aps[i]), TY_CHAR)
		}
	    }
	}
end
