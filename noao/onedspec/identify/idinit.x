include	<gset.h>
include	<math/curfit.h>
include	"identify.h"

# ID_INIT -- Allocate identify structure

procedure id_init (id)

pointer	id			#O ID pointer

pointer	stopen()
errchk	stopen

begin
	call calloc (id, ID_LENSTRUCT, TY_STRUCT)

	ID_NALLOC(id) = 20
	ID_NFEATURES(id) = 0
	ID_CURRENT(id) = 0
	ID_DT(id) = NULL
	ID_STP(id) = stopen ("identify", 100, 10*ID_LENSTRUCT, 10*SZ_LINE)

	if (ID_NALLOC(id) > 0) {
	    call malloc (ID_PIX(id), ID_NALLOC(id), TY_DOUBLE)
	    call malloc (ID_FIT(id), ID_NALLOC(id), TY_DOUBLE)
	    call malloc (ID_USER(id), ID_NALLOC(id), TY_DOUBLE)
	    call malloc (ID_WTS(id), ID_NALLOC(id), TY_DOUBLE)
	    call malloc (ID_FWIDTHS(id), ID_NALLOC(id), TY_REAL)
	    call malloc (ID_FTYPES(id), ID_NALLOC(id), TY_INT)
	    call calloc (ID_LABEL(id), ID_NALLOC(id), TY_POINTER)
	}
end


# ID_FREE -- Free identify structure.

procedure id_free (id)

pointer	id			#I ID pointer

int	i
pointer	ptr

begin
	if (id == NULL)
	    return

	call id_free1 (id)

	call mfree (ID_APS(id), TY_INT)

	ptr = ID_LABEL(id)
	do i = 1, ID_NFEATURES(id) {
	    call mfree (Memi[ptr], TY_CHAR)
	    ptr = ptr + 1
	}

	call mfree (ID_PIX(id), TY_DOUBLE)
	call mfree (ID_FIT(id), TY_DOUBLE)
	call mfree (ID_USER(id), TY_DOUBLE)
	call mfree (ID_WTS(id), TY_DOUBLE)
	call mfree (ID_FWIDTHS(id), TY_REAL)
	call mfree (ID_FTYPES(id), TY_INT)
	call mfree (ID_LABEL(id), TY_POINTER)

	if (ID_DT(id) != NULL)
	    call dtunmap (ID_DT(id))
	call id_unmapll (id)
	call stclose (ID_STP(id))
	call gt_free (ID_GT(id))
	call dcvfree (ID_CV(id))
	call ic_closed (ID_IC(id))
	if (ID_UN(id) != NULL)
	    call un_close (ID_UN(id))

	call mfree (id, TY_STRUCT)
end


# ID_FREE1 -- Free saved identify structures.

procedure id_free1 (id)

pointer	id				# ID pointer

int	i
pointer	stp, sid, ptr, sthead(), stnext(), stopen()

begin
	stp = ID_STP(id)
	for (sid = sthead(stp); sid != NULL; sid = stnext (stp, sid)) {
	    ptr = ID_LABEL(sid)
	    do i = 1, ID_NFEATURES(sid) {
		call mfree (Memi[ptr], TY_CHAR)
		ptr = ptr + 1
	    }

	    call mfree (ID_PIX(sid), TY_DOUBLE)
	    call mfree (ID_FIT(sid), TY_DOUBLE)
	    call mfree (ID_USER(sid), TY_DOUBLE)
	    call mfree (ID_WTS(sid), TY_DOUBLE)
	    call mfree (ID_FWIDTHS(sid), TY_REAL)
	    call mfree (ID_FTYPES(sid), TY_INT)
	    call mfree (ID_LABEL(sid), TY_POINTER)
	    if (ID_CV(sid) != NULL)
	        call dcvfree (ID_CV(sid))
	    if (ID_IC(sid) != NULL)
	        call ic_closed (ID_IC(sid))
	}
	if (sthead(stp) != NULL) {
	    call stclose (stp)
	    ID_STP(id) = stopen ("identify", 100, 10*ID_LENSTRUCT, 10*SZ_LINE)
	}
end


# ID_SAVEID -- Save identify information by key.

procedure id_saveid (id, key)

pointer	id		#I IDENTIFY structure
char	key[ARB]	#I Key to use in saving information

pointer	sid, stfind(), stenter()

begin
	sid = stfind (ID_STP(id), key)
	if (sid == NULL) {
	    sid = stenter (ID_STP(id), key, ID_LENSTRUCT)
	    call aclri (Memi[sid], ID_LENSTRUCT)
	}
	call strcpy (key, ID_SAVEID(id), ID_LENSTRING)
	call id_sid (id, sid)
end


# ID_GETID -- Get saved identify information by key.
# Return NULL if not found.

pointer procedure id_getid (id, key)

pointer	id		#I IDENTIFY structure
char	key[ARB]	#I Key to use in saving information

int	sid, stfind()

begin
	sid = stfind (ID_STP(id), key)
	if (sid != NULL)
	    call id_gid (id, sid)

	return (sid)
end


# ID_SAVEAP -- Save identify information by aperture.

procedure id_saveap (id)

pointer	id		# IDENTIFY structure

begin
	call sprintf (ID_SAVEID(id), ID_LENSTRING, "aperture %d %d")
	    call pargi (ID_AP(id,1))
	    call pargi (ID_AP(id,2))
	call id_saveid (id, ID_SAVEID(id))
end


# ID_GETAP -- Get saved identify information by aperture.
# Return NULL if not found.

pointer procedure id_getap (id)

pointer	id		# IDENTIFY structure

int	sid, stfind()

begin
	call sprintf (ID_SAVEID(id), ID_LENSTRING, "aperture %d %d")
	    call pargi (ID_AP(id,1))
	    call pargi (ID_AP(id,2))

	# Check if saved.
	sid = stfind (ID_STP(id), ID_SAVEID(id))
	if (sid != NULL)
	    call id_gid (id, sid)

	return (sid)
end


# ID_SID -- Save parts of IDENTIFY structure.

procedure id_sid (id, sid)

pointer	id		#I IDENTIFY structure
pointer	sid		#I IDENTIFY save structure

int	i, j, dcvstati(), strlen()
pointer	sp, coeffs, ptr1, ptr2

begin
	if (sid == NULL)
	    return

	# Allocate or reallocate memory for features and copy them.
	if (ID_NFEATURES(id) > 0) {
	    if (ID_NALLOC(sid) == 0) {
	        call malloc (ID_PIX(sid), ID_NFEATURES(id), TY_DOUBLE)
	        call malloc (ID_FIT(sid), ID_NFEATURES(id), TY_DOUBLE)
	        call malloc (ID_USER(sid), ID_NFEATURES(id), TY_DOUBLE)
	        call malloc (ID_WTS(sid), ID_NFEATURES(id), TY_DOUBLE)
	        call malloc (ID_FWIDTHS(sid), ID_NFEATURES(id), TY_REAL)
	        call malloc (ID_FTYPES(sid), ID_NFEATURES(id), TY_INT)
	        call calloc (ID_LABEL(sid), ID_NFEATURES(id), TY_POINTER)
	    } else if (ID_NALLOC(sid) != ID_NFEATURES(id)) {
	        call realloc (ID_PIX(sid), ID_NFEATURES(id), TY_DOUBLE)
	        call realloc (ID_FIT(sid), ID_NFEATURES(id), TY_DOUBLE)
	        call realloc (ID_USER(sid), ID_NFEATURES(id), TY_DOUBLE)
	        call realloc (ID_WTS(sid), ID_NFEATURES(id), TY_DOUBLE)
	        call realloc (ID_FWIDTHS(sid), ID_NFEATURES(id), TY_REAL)
	        call realloc (ID_FTYPES(sid), ID_NFEATURES(id), TY_INT)
	        call realloc (ID_LABEL(sid), ID_NFEATURES(id), TY_POINTER)

		j = ID_NALLOC(sid)
		i = ID_NFEATURES(id) - j
		if (i > 0)
		    call aclri (Memi[ID_LABEL(sid)+j], i)
	    }
	    call amovd (PIX(id,1), PIX(sid,1), ID_NFEATURES(id))
	    call amovd (FIT(id,1), FIT(sid,1), ID_NFEATURES(id))
	    call amovd (USER(id,1), USER(sid,1), ID_NFEATURES(id))
	    call amovd (WTS(id,1), WTS(sid,1), ID_NFEATURES(id))
	    call amovr (FWIDTH(id,1), FWIDTH(sid,1), ID_NFEATURES(id))
	    call amovi (FTYPE(id,1), FTYPE(sid,1), ID_NFEATURES(id))

	    ptr1 = ID_LABEL(id) 
	    ptr2 = ID_LABEL(sid) 
	    do i = 1, ID_NFEATURES(id) {
		call mfree (Memi[ptr2], TY_CHAR)
		if (Memi[ptr1] != NULL) {
		    j = strlen (Memc[Memi[ptr1]])
		    call malloc (Memi[ptr2], j, TY_CHAR)
		    call strcpy (Memc[Memi[ptr1]], Memc[Memi[ptr2]], j)
		}
		ptr1 = ptr1 + 1
		ptr2 = ptr2 + 1
	    }

	    ID_NALLOC(sid) = ID_NFEATURES(id)
	}

	# Use a SAVE and RESTORE to copy the CURFIT data.
	if (ID_CV(sid) != NULL)
	    call dcvfree (ID_CV(sid))
	if (ID_CV(id) != NULL) {
	    call smark (sp)
	    i = dcvstati (ID_CV(id), CVNSAVE)
	    call salloc (coeffs, i, TY_DOUBLE)
	    call dcvsave (ID_CV(id), Memd[coeffs])
	    call dcvrestore (ID_CV(sid), Memd[coeffs])
	    call sfree (sp)

	    if (ID_IC(sid) == NULL)
		call ic_open (ID_IC(sid))
	    call ic_copy (ID_IC(id), ID_IC(sid))
	}

	call strcpy (ID_SAVEID(id), ID_SAVEID(sid), ID_LENSTRING)
        ID_LINE(sid,1) = ID_LINE(id,1)
        ID_LINE(sid,2) = ID_LINE(id,2)
        ID_AP(sid,1) = ID_AP(id,1)
        ID_AP(sid,2) = ID_AP(id,2)
        ID_NFEATURES(sid) = ID_NFEATURES(id)
        ID_SHIFT(sid) = ID_SHIFT(id)
        ID_CURRENT(sid) = ID_CURRENT(id)

        ID_NEWFEATURES(sid) = ID_NEWFEATURES(id)
        ID_NEWCV(sid) = ID_NEWCV(id)
        ID_NEWDBENTRY(sid) = ID_NEWDBENTRY(id)
end


# ID_GID -- Restore saved identify information.

procedure id_gid (id, sid)

pointer	id		#I IDENTIFY structure
int	sid		#I IDENTIFY save structure

int	i, j, dcvstati(), strlen()
pointer	sp, coeffs, ptr1, ptr2

begin
	if (sid == NULL)
	    return

	# Reallocate memory for features and copy them.
	if (ID_NFEATURES(sid) > 0) {
	    if (ID_NALLOC(sid) != ID_NALLOC(id)) {
	        call realloc (ID_PIX(id), ID_NALLOC(sid), TY_DOUBLE)
	        call realloc (ID_FIT(id), ID_NALLOC(sid), TY_DOUBLE)
	        call realloc (ID_USER(id), ID_NALLOC(sid), TY_DOUBLE)
	        call realloc (ID_WTS(id), ID_NALLOC(sid), TY_DOUBLE)
	        call realloc (ID_FWIDTHS(id), ID_NALLOC(sid), TY_REAL)
	        call realloc (ID_FTYPES(id), ID_NALLOC(sid), TY_INT)
	        call realloc (ID_LABEL(id), ID_NALLOC(sid), TY_POINTER)

		j = ID_NALLOC(id)
		i = ID_NALLOC(sid) - j
		if (i > 0)
		    call aclri (Memi[ID_LABEL(id)+j], i)
	    }
	    call amovd (PIX(sid,1), PIX(id,1), ID_NFEATURES(sid))
	    call amovd (FIT(sid,1), FIT(id,1), ID_NFEATURES(sid))
	    call amovd (USER(sid,1), USER(id,1), ID_NFEATURES(sid))
	    call amovd (WTS(sid,1), WTS(id,1), ID_NFEATURES(sid))
	    call amovr (FWIDTH(sid,1), FWIDTH(id,1), ID_NFEATURES(sid))
	    call amovi (FTYPE(sid,1), FTYPE(id,1), ID_NFEATURES(sid))

	    ptr1 = ID_LABEL(sid) 
	    ptr2 = ID_LABEL(id) 
	    do i = 1, ID_NFEATURES(sid) {
		call mfree (Memi[ptr2], TY_CHAR)
		if (Memi[ptr1] != NULL) {
		    j = strlen (Memc[Memi[ptr1]])
		    call malloc (Memi[ptr2], j, TY_CHAR)
		    call strcpy (Memc[Memi[ptr1]], Memc[Memi[ptr2]], j)
		}
		ptr1 = ptr1 + 1
		ptr2 = ptr2 + 1
	    }

	    ID_NALLOC(id) = ID_NALLOC(sid)
	    ID_NFEATURES(id) = ID_NFEATURES(sid)
	    ID_NEWFEATURES(id) = ID_NEWFEATURES(sid)
	    ID_CURRENT(id) = ID_CURRENT(sid)
	    ID_NEWDBENTRY(id) = ID_NEWDBENTRY(sid)
	}

	# Use a SAVE and RESTORE to copy the CURFIT data.
	ID_SHIFT(id) = ID_SHIFT(sid)
	if (ID_CV(sid) != NULL) {
	    if (ID_CV(id) != NULL)
	        call dcvfree (ID_CV(id))
	    call smark (sp)
	    i = dcvstati (ID_CV(sid), CVNSAVE)
	    call salloc (coeffs, i, TY_DOUBLE)
	    call dcvsave (ID_CV(sid), Memd[coeffs])
	    call dcvrestore (ID_CV(id), Memd[coeffs])
	    call sfree (sp)

	    call ic_copy (ID_IC(sid), ID_IC(id))

	    ID_NEWCV(id) = ID_NEWCV(sid)
	    ID_NEWDBENTRY(id) = ID_NEWDBENTRY(sid)

	    call id_fitdata (id)
	    call id_fitfeatures (id)
	}

	call strcpy (ID_SAVEID(sid), ID_SAVEID(id), ID_LENSTRING)
	ID_LINE(id,1) = ID_LINE(sid,1)
	ID_LINE(id,2) = ID_LINE(sid,2)
	ID_AP(id,1) = ID_AP(sid,1)
	ID_AP(id,2) = ID_AP(sid,2)
end
