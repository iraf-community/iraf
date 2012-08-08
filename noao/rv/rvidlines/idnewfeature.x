include	<mach.h>
include	"identify.h"

# ID_NEWFEATURE -- Allocate and initialize memory for a new feature.

procedure id_newfeature (id, pix, fit, user, wt, width, type, label)

pointer	id			# ID pointer
double	pix			# Pixel coordinate
double	fit			# Fit coordinate
double	user			# User coordinate
double	wt			# Feature weight
real	width			# Feature width
int	type			# Feature type
pointer	label			# Pointer to feature label

int	i, current, strlen()
double	delta

define	NALLOC	20		# Length of additional allocations

begin
	if (IS_INDEFD (pix))
	    return

	delta = MAX_REAL
	do i = 1, ID_NFEATURES(id) {
	    if (abs (pix - PIX(id,i)) < delta) {
		delta = abs (pix - PIX(id,i))
		current = i
	    }
	}

	if (delta >= ID_MINSEP(id)) {
	    ID_NFEATURES(id) = ID_NFEATURES(id) + 1
	    if (ID_NALLOC(id) < ID_NFEATURES(id)) {
	        ID_NALLOC(id) = ID_NALLOC(id) + NALLOC
	        call realloc (ID_PIX(id), ID_NALLOC(id), TY_DOUBLE)
	        call realloc (ID_FIT(id), ID_NALLOC(id), TY_DOUBLE)
	        call realloc (ID_USER(id), ID_NALLOC(id), TY_DOUBLE)
	        call realloc (ID_WTS(id), ID_NALLOC(id), TY_DOUBLE)
	        call realloc (ID_FWIDTHS(id), ID_NALLOC(id), TY_REAL)
	        call realloc (ID_FTYPES(id), ID_NALLOC(id), TY_INT)
	        call realloc (ID_LABEL(id), ID_NALLOC(id), TY_POINTER)
		call aclri (Memi[ID_LABEL(id)+ID_NALLOC(id)-NALLOC], NALLOC)
	    }
	    for (current=ID_NFEATURES(id); (current>1)&&(pix<PIX(id,current-1));
		    current=current-1) {
	        PIX(id,current) = PIX(id,current-1)
	        FIT(id,current) = FIT(id,current-1)
	        USER(id,current) = USER(id,current-1)
	        WTS(id,current) = WTS(id,current-1)
	        FWIDTH(id,current) = FWIDTH(id,current-1)
	        FTYPE(id,current) = FTYPE(id,current-1)
		Memi[ID_LABEL(id)+current-1] = Memi[ID_LABEL(id)+current-2]
	    }
	    PIX(id,current) = pix
	    FIT(id,current) = fit
	    USER(id,current) = user
	    WTS(id,current) = wt
	    FWIDTH(id,current) = width
	    FTYPE(id,current) = type
	    if (label != NULL) {
		i = strlen (Memc[label])
		call malloc (Memi[ID_LABEL(id)+current-1], i, TY_CHAR)
		call strcpy (Memc[label], Memc[Memi[ID_LABEL(id)+current-1]], i)
	    } else
		Memi[ID_LABEL(id)+current-1] = NULL
	    ID_NEWFEATURES(id) = YES
	} else if (abs (fit-user) < abs (FIT(id,current)-USER(id,current))) {
	    PIX(id,current) = pix
	    FIT(id,current) = fit
	    USER(id,current) = user
	    WTS(id,current) = wt
	    FWIDTH(id,current) = width
	    FTYPE(id,current) = type
	    if (label != NULL) {
		i = strlen (Memc[label])
		call malloc (Memi[ID_LABEL(id)+current-1], i, TY_CHAR)
		call strcpy (Memc[label], Memc[Memi[ID_LABEL(id)+current-1]], i)
	    } else
		Memi[ID_LABEL(id)+current-1] = NULL
	    ID_NEWFEATURES(id) = YES
	}

	ID_CURRENT(id) = current
end
