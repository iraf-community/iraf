include <mach.h>
include <fset.h>
include <pkg/cq.h>

define	DEF_SZPARS	15
define	DEF_SZBUF	28800

# T_ADUMPIM -- Image survey access debugging routines.

procedure t_adumpim()

pointer	sp, imsurvey, output, ra, dec, size, imdb, query, buf, str
pointer	qpname, qpvalue, qpunits, qpformats, cq, url, host
int	i, nqpars, parno, nchars, port

pointer	cq_map()
int	cq_setcat(), cq_nqpars(), cq_gqparn(),	url_get(), access(), cq_fgeti()
bool	streq()
errchk	cq_fgeti(), cg_fgstr()

begin
	# Allocate some working space.
	call smark (sp)
	call salloc (imsurvey, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (imdb, SZ_FNAME, TY_CHAR)
	call salloc (ra, DEF_SZPARS, TY_CHAR)
	call salloc (dec, DEF_SZPARS, TY_CHAR)
	call salloc (size, DEF_SZPARS, TY_CHAR)
	call salloc (query, SZ_LINE, TY_CHAR)
	call salloc (qpname, CQ_SZ_QPNAME, TY_CHAR)
	call salloc (qpvalue, CQ_SZ_QPVALUE, TY_CHAR)
	call salloc (qpunits, CQ_SZ_QPUNITS, TY_CHAR)
	call salloc (qpformats, CQ_SZ_QPFMTS, TY_CHAR)
	call salloc (buf, 2*DEF_SZBUF, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Get the parameters.
	call clgstr ("imsurvey", Memc[imsurvey], SZ_FNAME)
	call clgstr ("output", Memc[output], SZ_FNAME)
	call clgstr ("ra", Memc[ra], DEF_SZPARS)
	call clgstr ("dec", Memc[dec], DEF_SZPARS)
	call clgstr ("size", Memc[size], DEF_SZPARS)
	call clgstr ("imdb", Memc[imdb], SZ_FNAME)

	# Map the image surfvey configuration file.
	cq = cq_map (Memc[imdb], READ_ONLY)
	if (cq == NULL) {
	    call eprintf ("Cannot open image survey configuration file %s\n")
		call pargstr (Memc[imdb])
	    call sfree (sp)
	    return
	}

	# Locate the image survey record.
	if (cq_setcat (cq, Memc[imsurvey]) <= 0) {
	    call eprintf ("Cannot locate image survey record %s\n")
		call pargstr (Memc[imsurvey])
	    call cq_unmap (cq)
	    call sfree (sp)
	    return
	}


        # Format the query without worrying about coordinate systems,
        # or formats. Just assume that the user types in ra, dec, and
        # fov in the form expected by the server.
        iferr (call cq_fgstr (cq, "url", Memc[query], SZ_LINE)) {
            call cq_fgwrd (cq, "host", Memc[host], SZ_LINE)
            call cq_fgwrd (cq, "request", Memc[str], SZ_LINE)
            port = cq_fgeti (cq, "port")

            call sprintf (Memc[query], SZ_LINE, "%s://%s%s")
                if (port == 80)
                    call pargstr ("http")
                else
                    call pargstr ("https")
                call pargstr (Memc[host])
                call pargstr (Memc[str])
        }

        nqpars = cq_nqpars (cq)
        call sprintf (Memc[url], SZ_LINE, Memc[query])
        do i = 1, nqpars {
            # Get description of each query parameter.
            parno = cq_gqparn (cq, i, Memc[qpname], CQ_SZ_QPNAME,
                Memc[qpvalue], CQ_SZ_QPVALUE, Memc[qpunits], CQ_SZ_QPUNITS,
                Memc[qpformats], CQ_SZ_QPFMTS)

            # Pass the parameters to the query string.
            if (streq (Memc[qpname], "ra")) {
                call pargstr (Memc[ra])
            } else if (streq (Memc[qpname], "dec")) {
                call pargstr (Memc[dec])
            } else if (streq (Memc[qpname], "width")) {
                call pargstr (Memc[size])
            } else if (streq (Memc[qpname], "xwidth")) {
                call pargstr (Memc[size])
            } else if (streq (Memc[qpname], "ywidth")) {
                call pargstr (Memc[size])
            } else if (streq (Memc[qpname], "width")) {
                call pargstr (Memc[size])
            } else if (streq (Memc[qpname], "height")) {
                call pargstr (Memc[size])
            } else if (streq (Memc[qpname], "radius")) {
                call pargstr (Memc[size])
            } else if (streq (Memc[qpname], "fov")) {
                call pargstr (Memc[size])
            } else {
                call pargstr (Memc[qpvalue])
            }
        }

        # Overwrite an existing output file.
        if (access (Memc[output], 0, 0) == YES)
            call delete (Memc[output])

call eprintf("url: '%s'\nout: '%s'\n");
call pargstr(Memc[url]);call pargstr(Memc[output])
        nchars = url_get (Memc[url], Memc[output], NULL)

	# Unmap the database.
	call cq_unmap (cq)

	call sfree (sp)
end
