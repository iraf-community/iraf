include	"identify.h"

# ID_DELETE -- Delete a feature.

procedure id_delete (id, feature)

pointer	id			# ID pointer
int	feature			# Feature to be deleted

int	i

begin
	call mfree (Memi[ID_LABEL(id)+feature-1], TY_CHAR)
	do i = feature + 1, ID_NFEATURES(id) {
	    PIX(id,i-1) = PIX(id,i)
	    FIT(id,i-1) = FIT(id,i)
	    USER(id,i-1) = USER(id,i)
	    WTS(id,i-1) = WTS(id,i)
	    FWIDTH(id,i-1) = FWIDTH(id,i)
	    FTYPE(id,i-1) = FTYPE(id,i)
	    Memi[ID_LABEL(id)+i-2] = Memi[ID_LABEL(id)+i-1]
	}
	Memi[ID_LABEL(id)+ID_NFEATURES(id)-1] = NULL
	ID_NFEATURES(id) = ID_NFEATURES(id) - 1
	ID_NEWFEATURES(id) = YES
end
