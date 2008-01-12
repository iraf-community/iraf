include <mach.h>
include <fset.h>
include <pkg/cq.h>

define	DEF_SZPARS	15
define	DEF_SZBUF	28800

# T_ADUMPIM -- Image survey access debugging routines.

procedure t_adumpim()

pointer	sp, imsurvey, output, ra, dec, size, imdb, query, buf, str
pointer	qpname, qpvalue, qpunits, qpformats, cq
int	i, fd, ofd, nqpars, parno, nchars
bool	done

pointer	cq_map()
int	cq_setcat(), ndopen(), open(), cq_nqpars(), cq_gqparn(), strlen()
int	getline(), read()
bool	streq()
errchk	cq_fgeti()

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

	# Connect to the HTTP server.
	call cq_fgwrd (cq, "address", Memc[str], SZ_LINE)
	iferr (fd = ndopen (Memc[str], READ_WRITE)) {
	    call eprintf ("Cannot access image server %s at host %s\n")
		call pargstr (Memc[imsurvey])
		call pargstr (Memc[str])
	    call cq_unmap (cq)
	    call sfree (sp)
	    return
	}

	# Open the output file.
        #ofd = open (Memc[output], NEW_FILE, BINARY_FILE)
        ofd = open (Memc[output], NEW_FILE, TEXT_FILE)

        # Format the query without worrying about coordinate systems,
        # or formats. Just assume that the user types in ra, dec, and
        # size in the form expected by the server.

        call cq_fgstr (cq, "query", Memc[query], SZ_LINE)
        nqpars = cq_nqpars (cq)
        call sprintf (Memc[str], SZ_LINE, Memc[query])
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
            } else if (streq (Memc[qpname], "radius")) {
                call pargstr (Memc[size])
            } else {
                call pargstr (Memc[qpvalue])
            }

        }

	# Send the query. Note that since the communication mode and output
	# file type are binary the command must be converted from IRAF chars
	# type byte chars. Leave as text for now since it works for FITS
	# files.

        nchars = strlen (Memc[str])
        #call chrpak (Memc[str], 1, Memc[str], 1, nchars)
        #call awriteb (fd, Memc[str], nchars, 1)
        #nread = awaitb (fd)
	call write (fd, Memc[str], nchars)
	call flush (fd)
	call fseti (fd, F_CANCEL, OK)

	# Read the reply. Skip the HTTP header assuming it ends with a \n or
	# a \r\n.
        call cq_fgstr (cq, "protocol", Memc[str], SZ_LINE)
	if (streq (Memc[str], "http")) {
	    repeat {
	        nchars = getline (fd, Memc[buf])
	        if (nchars <= 0)
		    break
	        Memc[buf+nchars] = EOS
	    } until ((Memc[buf] == '\r' && Memc[buf+1] == '\n') ||
	        (Memc[buf] == '\n'))
	}

	repeat {
	    nchars = read (fd, Memc[buf], DEF_SZBUF)
	    if (nchars > 0) {
		Memc[buf+nchars] = EOS
		call write (ofd, Memc[buf], nchars)
		done = false
	    } else {
		done =true
	    }
	 } until (done)

	call flush (ofd)

	# Close the output image.
	call close (ofd)

	# Close the network connection.
	call close (fd)

	# Unmap the database.
	call cq_unmap (cq)

	call sfree (sp)
end
