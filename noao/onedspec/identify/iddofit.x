include	<units.h>
include	"identify.h"

# ID_DOFIT -- Fit a function to the features.  Eliminate INDEF points.

procedure id_dofit (id, interactive)

pointer	id			# ID pointer
int	interactive		# Interactive fit?

int	i, j, k, nfit, ic_geti()
pointer	gt1, sp, x, y, wts, rejpts, str, gt_init()

begin
	if (ID_NFEATURES(id) == 0) {
	    if (ID_CV(id) != NULL) {
	        call dcvfree (ID_CV(id))
		ID_SHIFT(id) = 0.
	        ID_NEWGRAPH(id) = YES
	        ID_NEWCV(id) = YES
	    }
	    return
	}
	    
	call smark (sp)
	call salloc (x, ID_NFEATURES(id), TY_DOUBLE)
	call salloc (y, ID_NFEATURES(id), TY_DOUBLE)
	call salloc (wts, ID_NFEATURES(id), TY_DOUBLE)

	nfit = 0
	do i = 1, ID_NFEATURES(id) {
	    if (IS_INDEFD (PIX(id,i)) || IS_INDEFD (USER(id,i)))
		next
	    Memd[x+nfit] = PIX(id,i)
	    Memd[y+nfit] = USER(id,i)
	    Memd[wts+nfit] = max (1D0, WTS(id,i))
	    nfit = nfit + 1
	}

	if (nfit > 1) {
	    if (ID_UN(id) != NULL) {
		call ic_pstr (ID_IC(id), "ylabel", UN_LABEL(ID_UN(id)))
		call ic_pstr (ID_IC(id), "yunits", UN_UNITS(ID_UN(id)))
	    }
	    if (interactive == YES) {
		call salloc (str, SZ_LINE, TY_CHAR)
		gt1 = gt_init()
		call icg_fitd (ID_IC(id), ID_GP(id), "cursor", gt1, ID_CV(id),
		    Memd[x], Memd[y], Memd[wts], nfit)
		call gt_free (gt1)
	    } else
		call ic_fitd (ID_IC(id), ID_CV(id), Memd[x], Memd[y], Memd[wts],
		    nfit, YES, YES, YES, YES)

	    if (ic_geti (ID_IC(id), "nreject") > 0 &&
		ic_geti (ID_IC(id), "nfit") == nfit)
		rejpts = ic_geti (ID_IC(id), "rejpts")
	    else
		rejpts = NULL

	    j = 0
	    k = 0
	    do i = 1, ID_NFEATURES(id) {
	    	if (IS_INDEFD (PIX(id,i)) || IS_INDEFD (USER(id,i))) {
		    j = j + 1
	    	    PIX(id,j) = PIX(id,i)
	    	    FIT(id,j) = FIT(id,i)
	    	    USER(id,j) = USER(id,i)
	    	    WTS(id,j) = WTS(id,i)
	    	    FWIDTH(id,j) = FWIDTH(id,i)
	    	    FTYPE(id,j) = FTYPE(id,i)
		    call mfree (Memi[ID_LABEL(id)+j-1], TY_CHAR)
		    Memi[ID_LABEL(id)+j-1] = Memi[ID_LABEL(id)+i-1]
		} else {
		    if (Memd[wts+k] != 0.) {
		        j = j + 1
	    	        PIX(id,j) = Memd[x+k]
	    	        FIT(id,j) = FIT(id,i)
	    	        USER(id,j) = Memd[y+k]
	    	        WTS(id,j) = Memd[wts+k]
		        if (rejpts != NULL)
			    if (Memi[rejpts+k] == YES)
			        WTS(id,j) = 0.
	    	        FWIDTH(id,j) = FWIDTH(id,i)
	    	        FTYPE(id,j) = FTYPE(id,i)
			Memi[ID_LABEL(id)+j-1] = Memi[ID_LABEL(id)+i-1]
		    }
		    k = k + 1
		}
	    }
	    do i = j+1, ID_NFEATURES(id)
		Memi[ID_LABEL(id)+i-1] = NULL
	    ID_NFEATURES(id) = j

	    ID_SHIFT(id) = 0.
	    ID_NEWCV(id) = YES
	    ID_NEWGRAPH(id) = YES
	} else {
	    if (ID_CV(id) != NULL) {
	        call dcvfree (ID_CV(id))
		ID_SHIFT(id) = 0.
	        ID_NEWCV(id) = YES
	        ID_NEWGRAPH(id) = YES
	    }
	}

	call sfree (sp)
end
