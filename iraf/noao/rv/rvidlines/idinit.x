include	<gset.h>
include	<math/curfit.h>
include	"identify.h"

# ID_INIT -- Allocate identify structure

procedure id_init (id, taskid)

int	taskid			#I Task ID
pointer	id			#O ID pointer

begin
	call calloc (id, LEN_IDSTRUCT, TY_STRUCT)

	ID_TASK(id) = taskid
	ID_NALLOC(id) = 20
	ID_NFEATURES(id) = 0
	ID_CURRENT(id) = 0
	ID_NID(id) = 0
	ID_DT(id) = NULL

	call malloc (ID_IMAGE(id), SZ_FNAME, TY_CHAR)
	call malloc (ID_SECTION(id), SZ_FNAME, TY_CHAR)
	call malloc (ID_DATABASE(id), SZ_FNAME, TY_CHAR)
	call malloc (ID_COORDLIST(id), SZ_FNAME, TY_CHAR)

	call malloc (ID_PIX(id), ID_NALLOC(id), TY_DOUBLE)
	call malloc (ID_FIT(id), ID_NALLOC(id), TY_DOUBLE)
	call malloc (ID_USER(id), ID_NALLOC(id), TY_DOUBLE)
	call malloc (ID_WTS(id), ID_NALLOC(id), TY_DOUBLE)
	call malloc (ID_FWIDTHS(id), ID_NALLOC(id), TY_REAL)
	call malloc (ID_FTYPES(id), ID_NALLOC(id), TY_INT)
	call calloc (ID_LABEL(id), ID_NALLOC(id), TY_POINTER)
end


# ID_FREE -- Free identify structure.

procedure id_free (id)

pointer	id				# ID pointer

int	i
pointer	ptr

begin
	call mfree (ID_IMAGE(id), TY_CHAR)
	call mfree (ID_SECTION(id), TY_CHAR)
	call mfree (ID_DATABASE(id), TY_CHAR)
	call mfree (ID_COORDLIST(id), TY_CHAR)
	call mfree (ID_APS(id), TY_INT)

	ptr = ID_LABEL(id)
	do i = 1, ID_NALLOC(id) {
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
	call id_free1 (id)
	call id_unmapll (id)
	call gt_free (ID_GT(id))
	call dcvfree (ID_CV(id))
	call ic_closed (ID_IC(id))

	call mfree (id, TY_STRUCT)
end


# ID_FREE1 -- Free saved identify structures.

procedure id_free1 (id)

pointer	id				# ID pointer

int	i, j
pointer	id1, ptr

begin
	for (i = 0; i < ID_NID(id); i = i + 1) {
	    id1 = Memi[ID_ID(id)+i]

	    ptr = ID_LABEL(id1)
	    do j = 1, ID_NALLOC(id1) {
		call mfree (Memi[ptr], TY_CHAR)
		ptr = ptr + 1
	    }

	    call mfree (ID_PIX(id1), TY_DOUBLE)
	    call mfree (ID_FIT(id1), TY_DOUBLE)
	    call mfree (ID_USER(id1), TY_DOUBLE)
	    call mfree (ID_WTS(id1), TY_DOUBLE)
	    call mfree (ID_FWIDTHS(id1), TY_REAL)
	    call mfree (ID_FTYPES(id1), TY_INT)
	    call mfree (ID_LABEL(id1), TY_POINTER)
	    if (ID_CV(id1) != NULL)
	        call dcvfree (ID_CV(id1))
	    if (ID_IC(id1) != NULL)
	        call ic_closed (ID_IC(id1))
	    call shdr_close (ID_SH(id1))
	    call mfree (id1, TY_STRUCT)
	}
	call mfree (ID_ID(id), TY_POINTER)

	ID_NID(id) = 0
end


# ID_SAVEID -- Save identify information by line.

procedure id_saveid (id, line)

pointer	id		# IDENTIFY structure
int	line[2]		# Save as line

int	i
pointer	id1

begin
	# Check if already saved.  If not saved allocate memory.
	for (i = 1; i <= ID_NID(id); i = i + 1) {
	    id1 = Memi[ID_ID(id)+i-1]
	    if (ID_LINE(id1,1) == line[1] && ID_LINE(id1,2) == line[2])
		break
	}

	call id_sid (id, i)
end



# ID_SID -- Save identify information by index.

procedure id_sid (id, n)

pointer	id		# IDENTIFY structure
int	n		# Save as index

int	i, j, dcvstati(), strlen()
pointer	sp, id1, coeffs, ptr1, ptr2

begin
	if (n > ID_NID(id)) {
	    if (n == 1)
	        call malloc (ID_ID(id), 1, TY_POINTER)
	    else
	        call realloc (ID_ID(id), n, TY_POINTER)
	    call calloc (id1, LEN_IDSTRUCT, TY_STRUCT)
	    Memi[ID_ID(id)+n-1] = id1
	    ID_NID(id) = n
	} else
	    id1 = Memi[ID_ID(id)+n-1]

	# Allocate or reallocate memory for features and copy them.
	if (ID_NFEATURES(id) > 0) {
	    if (ID_NALLOC(id1) == 0) {
	        call malloc (ID_PIX(id1), ID_NFEATURES(id), TY_DOUBLE)
	        call malloc (ID_FIT(id1), ID_NFEATURES(id), TY_DOUBLE)
	        call malloc (ID_USER(id1), ID_NFEATURES(id), TY_DOUBLE)
	        call malloc (ID_WTS(id1), ID_NFEATURES(id), TY_DOUBLE)
	        call malloc (ID_FWIDTHS(id1), ID_NFEATURES(id), TY_REAL)
	        call malloc (ID_FTYPES(id1), ID_NFEATURES(id), TY_INT)
	        call calloc (ID_LABEL(id1), ID_NFEATURES(id), TY_POINTER)
	    } else if (ID_NALLOC(id1) != ID_NFEATURES(id)) {
	        call realloc (ID_PIX(id1), ID_NFEATURES(id), TY_DOUBLE)
	        call realloc (ID_FIT(id1), ID_NFEATURES(id), TY_DOUBLE)
	        call realloc (ID_USER(id1), ID_NFEATURES(id), TY_DOUBLE)
	        call realloc (ID_WTS(id1), ID_NFEATURES(id), TY_DOUBLE)
	        call realloc (ID_FWIDTHS(id1), ID_NFEATURES(id), TY_REAL)
	        call realloc (ID_FTYPES(id1), ID_NFEATURES(id), TY_INT)
	        call realloc (ID_LABEL(id1), ID_NFEATURES(id), TY_POINTER)

		j = ID_NALLOC(id1)
		i = ID_NFEATURES(id) - j
		if (i > 0)
		    call aclri (Memi[ID_LABEL(id1)+j], i)
	    }
	    call amovd (PIX(id,1), PIX(id1,1), ID_NFEATURES(id))
	    call amovd (FIT(id,1), FIT(id1,1), ID_NFEATURES(id))
	    call amovd (USER(id,1), USER(id1,1), ID_NFEATURES(id))
	    call amovd (WTS(id,1), WTS(id1,1), ID_NFEATURES(id))
	    call amovr (FWIDTH(id,1), FWIDTH(id1,1), ID_NFEATURES(id))
	    call amovi (FTYPE(id,1), FTYPE(id1,1), ID_NFEATURES(id))

	    ptr1 = ID_LABEL(id) 
	    ptr2 = ID_LABEL(id1) 
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

	    ID_NALLOC(id1) = ID_NFEATURES(id)
	}

	# Use a SAVE and RESTORE to copy the CURFIT data.
	if (ID_CV(id1) != NULL)
	    call dcvfree (ID_CV(id1))
	if (ID_CV(id) != NULL) {
	    call smark (sp)
	    i = dcvstati (ID_CV(id), CVNSAVE)
	    call salloc (coeffs, i, TY_DOUBLE)
	    call dcvsave (ID_CV(id), Memd[coeffs])
	    call dcvrestore (ID_CV(id1), Memd[coeffs])
	    call sfree (sp)

	    if (ID_IC(id1) == NULL)
		call ic_open (ID_IC(id1))
	    call ic_copy (ID_IC(id), ID_IC(id1))
	}

	#ID_LINE(id1,1) = line[1]
	#ID_LINE(id1,2) = line[2]
	ID_LINE(id1,1) = ID_LINE(id,1)
	ID_LINE(id1,2) = ID_LINE(id,2)
	ID_AP(id1,1) = ID_AP(id,1)
	ID_AP(id1,2) = ID_AP(id,2)
	ID_NFEATURES(id1) = ID_NFEATURES(id)
	ID_SHIFT(id1) = ID_SHIFT(id)
	ID_REDSHIFT(id1) = ID_REDSHIFT(id)
	ID_RMSRED(id1) = ID_RMSRED(id)
	ID_ZHELIO(id1) = ID_ZHELIO(id)
	ID_CURRENT(id1) = ID_CURRENT(id)

	ID_NEWFEATURES(id1) = ID_NEWFEATURES(id)
	ID_NEWCV(id1) = ID_NEWCV(id)
	ID_NEWDBENTRY(id1) = ID_NEWDBENTRY(id)
end


# ID_GID -- Get saved identify information for specified line.

int procedure id_gid (id, line)

pointer	id		# IDENTIFY structure
int	line[2]		# Line number to get

int	i, id_getid()

begin
	# Check if saved.
	for (i = 1; i <= ID_NID(id); i = i + 1) {
	    if (ID_LINE(Memi[ID_ID(id)+i-1],1) == line[1] &&
	        ID_LINE(Memi[ID_ID(id)+i-1],2) == line[2])
		break
	}

	return (id_getid (id, i))
end


# ID_GETID -- Get saved identify information for specified index.

int procedure id_getid (id, n)

pointer	id		# IDENTIFY structure
int	n		# Index of saved features to be returned

int	i, j, dcvstati(), strlen()
pointer	sp, id1, coeffs, ptr1, ptr2

begin
	if (n < 1 || n > ID_NID(id))
	    return (EOF)

	id1 = Memi[ID_ID(id)+n-1]

	# Reallocate memory for features and copy them.
	if (ID_NFEATURES(id1) > 0) {
	    if (ID_NALLOC(id1) != ID_NALLOC(id)) {
	        call realloc (ID_PIX(id), ID_NALLOC(id1), TY_DOUBLE)
	        call realloc (ID_FIT(id), ID_NALLOC(id1), TY_DOUBLE)
	        call realloc (ID_USER(id), ID_NALLOC(id1), TY_DOUBLE)
	        call realloc (ID_WTS(id), ID_NALLOC(id1), TY_DOUBLE)
	        call realloc (ID_FWIDTHS(id), ID_NALLOC(id1), TY_REAL)
	        call realloc (ID_FTYPES(id), ID_NALLOC(id1), TY_INT)
	        call realloc (ID_LABEL(id), ID_NALLOC(id1), TY_POINTER)

		j = ID_NALLOC(id)
		i = ID_NALLOC(id1) - j
		if (i > 0)
		    call aclri (Memi[ID_LABEL(id)+j], i)
	    }
	    call amovd (PIX(id1,1), PIX(id,1), ID_NFEATURES(id1))
	    call amovd (FIT(id1,1), FIT(id,1), ID_NFEATURES(id1))
	    call amovd (USER(id1,1), USER(id,1), ID_NFEATURES(id1))
	    call amovd (WTS(id1,1), WTS(id,1), ID_NFEATURES(id1))
	    call amovr (FWIDTH(id1,1), FWIDTH(id,1), ID_NFEATURES(id1))
	    call amovi (FTYPE(id1,1), FTYPE(id,1), ID_NFEATURES(id1))

	    ptr1 = ID_LABEL(id1) 
	    ptr2 = ID_LABEL(id) 
	    do i = 1, ID_NFEATURES(id1) {
		call mfree (Memi[ptr2], TY_CHAR)
		if (Memi[ptr1] != NULL) {
		    j = strlen (Memc[Memi[ptr1]])
		    call malloc (Memi[ptr2], j, TY_CHAR)
		    call strcpy (Memc[Memi[ptr1]], Memc[Memi[ptr2]], j)
		}
		ptr1 = ptr1 + 1
		ptr2 = ptr2 + 1
	    }

	    ID_NALLOC(id) = ID_NALLOC(id1)
	    ID_NFEATURES(id) = ID_NFEATURES(id1)
	    ID_NEWFEATURES(id) = ID_NEWFEATURES(id1)
	    ID_CURRENT(id) = ID_CURRENT(id1)
	    ID_NEWDBENTRY(id) = ID_NEWDBENTRY(id1)
	}

	# Use a SAVE and RESTORE to copy the CURFIT data.
	if (ID_CV(id1) != NULL) {
	    if (ID_CV(id) != NULL)
	        call dcvfree (ID_CV(id))
	    call smark (sp)
	    i = dcvstati (ID_CV(id1), CVNSAVE)
	    call salloc (coeffs, i, TY_DOUBLE)
	    call dcvsave (ID_CV(id1), Memd[coeffs])
	    call dcvrestore (ID_CV(id), Memd[coeffs])
	    call sfree (sp)

	    call ic_copy (ID_IC(id1), ID_IC(id))

	    ID_SHIFT(id) = ID_SHIFT(id1)
	    ID_REDSHIFT(id) = ID_REDSHIFT(id1)
	    ID_RMSRED(id) = ID_RMSRED(id1)
	    ID_ZHELIO(id) = ID_ZHELIO(id1)
	    ID_NEWCV(id) = ID_NEWCV(id1)
	    ID_NEWDBENTRY(id) = ID_NEWDBENTRY(id1)
	}

	ID_LINE(id,1) = ID_LINE(id1,1)
	ID_LINE(id,2) = ID_LINE(id1,2)
	ID_AP(id,1) = ID_AP(id1,1)
	ID_AP(id,2) = ID_AP(id1,2)

	return (OK)
end
