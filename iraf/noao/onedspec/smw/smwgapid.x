include	<smw.h>


# SMW_GAPID -- Get aperture id.

procedure smw_gapid (smw, index1, index2, apid, maxchar)

pointer	smw				#I SMW pointer
int	index1				#I Spectrum index
int	index2				#I Spectrum index
char	apid[maxchar]			#O Aperture id
int	maxchar				#I Maximum number of characters

pointer	ptr

begin
	switch (SMW_FORMAT(smw)) {
	case SMW_ND:
	    call strcpy (Memc[SMW_APID(smw)], apid, maxchar)
	case SMW_ES, SMW_MS:
	    if (index1 < 0 || index1 > SMW_NSPEC(smw))
		call error (1, "smw_gapid: index out of bounds")

	    ptr = Memi[SMW_APIDS(smw)+index1-1]
	    if (index1 == 0 || ptr == NULL)
		call strcpy (Memc[SMW_APID(smw)], apid, maxchar)
	    else
		call strcpy (Memc[ptr], apid, maxchar)
	}
end
