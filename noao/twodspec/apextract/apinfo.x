include	"apertures.h"

# AP_INFO -- Print information about an aperture.

procedure ap_info (ap)

pointer	ap		# Aperture pointer

int	n, ic_geti(), strlen()
real	ic_getr()
pointer	sp, str1, str2

begin
	call smark (sp)

	if (AP_IC(ap) != NULL) {
	    call salloc (str1, SZ_LINE, TY_CHAR)
	    call salloc (str2, SZ_LINE, TY_CHAR)

	    n = 0
	    call ic_gstr (AP_IC(ap), "function", Memc[str1], SZ_LINE)
	    call sprintf (Memc[str2], SZ_LINE, "background: func=%s ord=%d")
		call pargstr (Memc[str1])
		call pargi (ic_geti (AP_IC(ap), "order"))
	    n = strlen (Memc[str2])
	    call printf ("%s")
		call pargstr (Memc[str2])

	    call ic_gstr (AP_IC(ap), "sample", Memc[str1], SZ_LINE)
	    if (Memc[str1] != '*') {
	        call sprintf (Memc[str2], SZ_LINE, " sample=\"%s\"")
		    call pargstr (Memc[str1])
	        n = n + strlen (Memc[str2])
		if (n > 80) {
		    call printf ("\n\t")
		    n = 8 + strlen (Memc[str2])
		}
		call printf ("%s")
		    call pargstr (Memc[str2])
	    }
	    if (ic_geti (AP_IC(ap), "naverage") != 1) {
		call sprintf (Memc[str2], SZ_LINE, " nav=%d")
		    call pargi (ic_geti (AP_IC(ap), "naverage"))
	        n = n + strlen (Memc[str2])
		if (n > 80) {
		    call printf ("\n\t")
		    n = 8 + strlen (Memc[str2])
		}
		call printf ("%s")
		    call pargstr (Memc[str2])
	    }
	    if (ic_geti (AP_IC(ap), "niterate") > 0) {
		call sprintf (Memc[str2], SZ_LINE, " nit=%d")
		    call pargi (ic_geti (AP_IC(ap), "niterate"))
	        n = n + strlen (Memc[str2])
		if (n > 80) {
		    call printf ("\n\t")
		    n = 8 + strlen (Memc[str2])
		}
		call printf ("%s")
		    call pargstr (Memc[str2])
		call sprintf (Memc[str2], SZ_LINE, " low=%3.1f")
		    call pargr (ic_getr (AP_IC(ap), "low"))
	        n = n + strlen (Memc[str2])
		if (n > 80) {
		    call printf ("\n\t")
		    n = 8 + strlen (Memc[str2])
		}
		call printf ("%s")
		    call pargstr (Memc[str2])
		call sprintf (Memc[str2], SZ_LINE, " high=%3.1f")
		    call pargr (ic_getr (AP_IC(ap), "high"))
	        n = n + strlen (Memc[str2])
		if (n > 80) {
		    call printf ("\n\t")
		    n = 8 + strlen (Memc[str2])
		}
		call printf ("%s")
		    call pargstr (Memc[str2])
	        if (ic_getr (AP_IC(ap), "grow") > 0) {
		    call sprintf (Memc[str2], SZ_LINE, " grow=%d")
		        call pargr (ic_getr (AP_IC(ap), "grow"))
	            n = n + strlen (Memc[str2])
		    if (n > 80) {
		        call printf ("\n\t")
		        n = 8 + strlen (Memc[str2])
		    }
		    call printf ("%s")
		        call pargstr (Memc[str2])
	        }
	    }
	    call printf ("\n")
	}

	call sfree (sp)
end
