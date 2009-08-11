include	<smw.h>


# SMW_SAPID -- Set aperture id.

procedure smw_sapid (smw, index1, index2, apid)

pointer	smw				#I SMW pointer
long	index1				#I Spectrum index
long	index2				#I Spectrum index
char	apid[ARB]			#I Aperture id

size_t	sz_val
pointer	ptr
bool	streq()
errchk	malloc

begin
	switch (SMW_FORMAT(smw)) {
	case SMW_ND:
	    call strcpy (apid, Memc[SMW_APID(smw)], SZ_LINE)
	case SMW_ES, SMW_MS:
	    if (index1 < 0 || index1 > SMW_NSPEC(smw))
		call error (1, "smw_sapid: index out of bounds")

	    if (index1 == 0)
		call strcpy (apid, Memc[SMW_APID(smw)], SZ_LINE)

	    else {
		ptr = Memp[SMW_APIDS(smw)+index1-1]
		if (streq (apid, Memc[SMW_APID(smw)]))
		    call mfree (ptr, TY_CHAR) 
		else {
		    if (ptr == NULL) {
			sz_val = SZ_LINE
			call malloc (ptr, sz_val, TY_CHAR)
		    }
		    call strcpy (apid, Memc[ptr], SZ_LINE)
		}
		Memp[SMW_APIDS(smw)+index1-1] = ptr
	    }
	}
end
