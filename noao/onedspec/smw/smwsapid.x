include	<smw.h>


# SMW_SAPID -- Set aperture id.

procedure smw_sapid (smw, index1, index2, apid)

pointer	smw				#I SMW pointer
int	index1				#I Spectrum index
int	index2				#I Spectrum index
char	apid[ARB]			#I Aperture id

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
		ptr = Memi[SMW_APIDS(smw)+index1-1]
		if (streq (apid, Memc[SMW_APID(smw)]))
		    call mfree (ptr, TY_CHAR) 
		else {
		    if (ptr == NULL)
			call malloc (ptr, SZ_LINE, TY_CHAR)
		    call strcpy (apid, Memc[ptr], SZ_LINE)
		}
		Memi[SMW_APIDS(smw)+index1-1] = ptr
	    }
	}
end
