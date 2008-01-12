include "cqdef.h"
include "cq.h"


# CQ_ISTATI -- Get an integer image results parameter.

int procedure cq_istati (res, param)

pointer res                     #I pointer to the results descriptor
int     param                   #I the integer parameter to be retrieved

begin
        switch (param) {
	case CQINQPARS:
            return (CQ_INQPARS(res))
        case CQIMTYPE:
            return (CQ_IMTYPE(res))
        case CQWCS:
            return (CQ_WCS(res))
        case CQNWCS:
            return (CQ_NWCS(res))
        case CQNIMPARS:
            return (CQ_NIMPARS(res))
        default:
            call error (0, "Error fetching integer image results parameter")
        }
end


# CQ_ISTATR -- Get a real image results parameter.

real procedure cq_istatr (res, param)

pointer res                     #I pointer to the image results descriptor
int     param                   #I the real image parameter to be retrieved

begin
        switch (param) {
        default:
            call error (0, "Error fetching real results parameter")
        }
end


# CQ_ISTATD -- Get a double precision image results parameter.

double procedure cq_istatd (res, param)

pointer res                     #I pointer to the image results descriptor
int     param                   #I the double parameter to be retrieved

begin
        switch (param) {
        default:
            call error (0, "Error fetching double results parameter")
        }
end


# CQ_ISTATS -- Get a string image results parameter.

procedure cq_istats (res, param, str, maxch)

pointer res                     #I pointer to the results descriptor
int     param                   #I the string parameter to be retrieved
char    str[ARB]                #O the output string parameter
int     maxch                   #I the maximum size of the string parameter

begin
        switch (param) {
        case CQIQPNAMES:
            call strcpy (Memc[CQ_IQPNAMES(res)], str, maxch)
        case CQIQPVALUES:
            call strcpy (Memc[CQ_IQPVALUES(res)], str, maxch)
        case CQIQPUNITS:
            call strcpy (Memc[CQ_IQPUNITS(res)], str, maxch)
        case CQIMCATDB:
            call strcpy (CQ_IMCATDB(res), str, maxch)
        case CQIMCATNAME:
            call strcpy (CQ_IMCATNAME(res), str, maxch)
        case CQIMADDRESS:
            call strcpy (CQ_IMADDRESS(res), str, maxch)
        case CQIMQUERY:
            call strcpy (CQ_IMQUERY(res), str, maxch)
        case CQIMNAME:
            call strcpy (CQ_IMNAME(res), str, maxch)
        default:
            call error (0, "Error fetching string results parameter")
        }
end


# CQ_ISTATT -- Get a text list results parameter. A text list is a
# string with items separated from each other by newlines.

int procedure cq_istatt (res, param, str, maxch)

pointer res                     #I pointer to the results descriptor
int     param                   #I the list parameter to be retrieved
char    str[ARB]                #O the output string parameter
int     maxch                   #I the maximum size of the string parameter

pointer	sp, tstr
int     i, fd
int     stropen(), cq_wrdstr()

begin
        switch (param) {

        case CQIQPNAMES:
	    call smark (sp)
	    call salloc (tstr, CQ_SZ_QPNAME, TY_CHAR)
            fd = stropen (str, maxch, NEW_FILE)
	    str[1] = EOS
            do i = 1, CQ_INQPARS(res) {
		if (cq_wrdstr (i, Memc[tstr], CQ_SZ_QPNAME,
		    Memc[CQ_IQPNAMES(res)]) > 0) {
                    call fprintf (fd, "%s\n")
                        call pargstr (Memc[tstr])
		}
            }
            call close (fd)
	    call sfree (sp)
            return (CQ_INQPARS(res))

        case CQIQPVALUES:
	    call smark (sp)
	    call salloc (tstr, CQ_SZ_QPVALUE, TY_CHAR)
            fd = stropen (str, maxch, NEW_FILE)
	    str[1] = EOS
            do i = 1, CQ_INQPARS(res) {
		if (cq_wrdstr (i, Memc[tstr], CQ_SZ_QPVALUE,
		    Memc[CQ_IQPVALUES(res)]) > 0) {
                    call fprintf (fd, "%s\n")
                        call pargstr (Memc[tstr])
		}
            }
            call close (fd)
	    call sfree (sp)
            return (CQ_INQPARS(res))

        case CQIQPUNITS:
	    call smark (sp)
	    call salloc (tstr, CQ_SZ_QPUNITS, TY_CHAR)
            fd = stropen (str, maxch, NEW_FILE)
	    str[1] = EOS
            do i = 1, CQ_INQPARS(res) {
		if (cq_wrdstr (i, Memc[tstr], CQ_SZ_QPUNITS,
		    Memc[CQ_IQPUNITS(res)]) > 0) {
                    call fprintf (fd, "%s\n")
                        call pargstr (Memc[tstr])
		}
            }
            call close (fd)
	    call sfree (sp)
            return (CQ_INQPARS(res))

        default:
            call error (0, "Error fetching list image results parameter")
        }
end
