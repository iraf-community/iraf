include <math.h>
include <pkg/cq.h>
include <pkg/skywcs.h>
include "../../lib/astrom.h"

# AT_TQUERY -- Extract catalog objects from a text file stored in a results
# query structure.

pointer procedure at_tquery (at, cq, cres, hdrtext, nlines, fieldno)

pointer	at			#I the astrometry package descriptor
pointer	cq			#I the astrometric catalog descriptor
pointer	cres			#I the input catalog results descriptor
char	hdrtext[ARB]		#I the catalog header test
int	nlines			#I the number of lines in the header text
int	fieldno			#I the region number

double	rac, decc, ra, dec, rawidth, decwidth, ra1, ra2, dec1, dec2, dist
double	tra, trac
pointer	 sp, csystem, raname, decname, funits, tmpname, line
pointer	res, ccoo, mw
int	i, fd, strfd, stat, units
pointer	cq_fquery()
int	cq_rstati(), at_rcregion(), open(), strlen(), cq_hinfo(), sk_decwcs()
int	stropen(), getline(), strdic(), cq_gvald(), cq_grecord(), sk_stati()
bool	streq()

begin
	# Return if the input catalog is undefined or contains no records.
	if (cres == NULL)
	    return (NULL)
	if (cq_rstati (cres, CQRNRECS) <= 0)
	    return (NULL)

	# Return if the header is undefined.
	if (nlines <= 0 || hdrtext[1] == EOS)
	    return (NULL)

	# Get the region to be extracted.
	if (at_rcregion (at, cres, fieldno, rac, decc, rawidth,
	    decwidth) == ERR)
	    return (NULL)

	# Compute the ra and dec limits.
	call at_rclimits (rac, decc, rawidth, decwidth, ra1, ra2, dec1, dec2)

	# Get some working space.
	call smark (sp)
	call salloc (csystem, CQ_SZ_FNAME, TY_CHAR)
	call salloc (tmpname, SZ_FNAME, TY_CHAR)
	call salloc (raname, CQ_SZ_FNAME, TY_CHAR)
	call salloc (decname, CQ_SZ_FNAME, TY_CHAR)
	call salloc (funits, CQ_SZ_FUNITS, TY_CHAR)
	call salloc (line, SZ_LINE, TY_CHAR)

	# Open the catalog coordinate system.
        if (cq_hinfo (cres, "csystem", Memc[csystem], SZ_FNAME) <= 0)
            Memc[csystem] = EOS
        if (Memc[csystem] == EOS || streq (Memc[csystem], "INDEF"))
            call strcpy ("DEF_CATSYSTEM", Memc[csystem], SZ_FNAME)

        # Open the query coordinate system data structure.
        stat = sk_decwcs (Memc[csystem], mw, ccoo, NULL)
        if (stat == ERR || mw != NULL) {
            if (mw != NULL)
                call mw_close (mw)
            call sk_close (ccoo)
            call sfree (sp)
            return (NULL)
        }

	# Open the temporary results file.
	call mktemp ("res", Memc[tmpname], SZ_FNAME)
	fd = open (Memc[tmpname], NEW_FILE, TEXT_FILE)

	# Write the file header to the temporary results file.
	strfd = stropen (hdrtext, strlen(hdrtext), READ_ONLY) 
	call fprintf (fd, "# BEGIN CATALOG HEADER\n")
	while (getline (strfd, Memc[line]) != EOF) {
	    call fprintf (fd, "# %s")
		call pargstr (Memc[line])
	}
	call fprintf (fd, "# END CATALOG HEADER\n#\n")
	call strclose (strfd)

	# Determine the names of the ra and dec columns.
	iferr (call at_stats (at, FIRA, Memc[raname], CQ_SZ_FNAME))
	    call strcpy ("ra", Memc[raname], CQ_SZ_FNAME)
	iferr (call at_stats (at, FIDEC, Memc[decname], CQ_SZ_FNAME))
	    call strcpy ("dec", Memc[decname], CQ_SZ_FNAME)

	# Determine the units of the ra and dec keywords.
	call cq_funits (cres, Memc[raname], Memc[funits], CQ_SZ_QPUNITS) 
        units = strdic (Memc[funits], Memc[funits], CQ_SZ_FUNITS,
            SKY_LNG_UNITLIST)
        if (units > 0)
            call sk_seti (ccoo, S_NLNGUNITS, units)
	call cq_funits (cres, Memc[decname], Memc[funits], CQ_SZ_QPUNITS) 
        units = strdic (Memc[funits], Memc[funits], CQ_SZ_FUNITS,
            SKY_LAT_UNITLIST)
        if (units > 0)
            call sk_seti (ccoo, S_NLATUNITS, units)

	# Loop over the catalog records selecting those that match
	# the region description.
	do i = 1, cq_rstati (cres, CQRNRECS) {

	    # Decode the coordinates.
	    if (cq_gvald (cres, i, Memc[raname], ra) <= 0)
		next
	    if (cq_gvald (cres, i, Memc[decname], dec) <= 0)
		next

	    # Determine the coordinate units.
	    switch (sk_stati(ccoo, S_NLNGUNITS)) {
	    case SKY_HOURS:
		ra = 15.0d0 * ra
	    case SKY_DEGREES:
		;
	    case SKY_RADIANS:
		ra = DRADTODEG(ra)
	    default:
		;
	    }
	    switch (sk_stati(ccoo, S_NLATUNITS)) {
	    case SKY_HOURS:
		dec = 15.0d0 * dec
	    case SKY_DEGREES:
		;
	    case SKY_RADIANS:
		dec = DRADTODEG(dec)
	    default:
		;
	    }

	    # Test the limits
	    if (dec < dec1 || dec > dec2)
		next
	    if (ra1 < ra2) {
		if (ra < ra1 || ra > ra2)
		    next
	    } else {
		if (ra > ra2 && ra < ra1)
		    next
	    }

            # Check the longitude coordinate distance to remove pathologies
            # in longitude or latitude strips involving the pole. This is
            # an extra test of my own.
            if (ra1 < ra2) {
                dist = abs (ra - rac)
            } else {
                if (ra > ra1)
                    tra = ra - 360.0d0
                else
                    tra = ra
                if (rac > ra1)
                    trac = rac - 360.0d0
                else
                    trac = rac
                dist = abs (tra - trac)
            }
            if (abs (2.0d0 * dist *cos(DEGTORAD(dec))) > rawidth)
                next

	    # Record has been selected.
	    if (cq_grecord (cres, Memc[line], SZ_LINE, i) <= 0)
		next
	    call putline (fd, Memc[line])
	}

	# Close the tmeporary file.
	call close (fd)

	# Query the temporary file and then delete it.
	res = cq_fquery (cq, Memc[tmpname], hdrtext) 
	call delete (Memc[tmpname])

	# Clean up.
	call sfree (sp)

	return (res)
end
