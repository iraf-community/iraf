include	<gset.h>
include	"identify.h"

# ID_INIT -- Allocate and identify structure

procedure id_init (id)

pointer	id			# ID pointer

begin
	call calloc (id, LEN_IDSTRUCT, TY_STRUCT)

	ID_NALLOC(id) = 20
	ID_NFEATURES(id) = 0
	ID_CURRENT(id) = 0

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
end


# ID_FREE -- Free identify structure.

procedure id_free (id)

pointer	id				# ID pointer

begin
	call mfree (ID_IMAGE(id), TY_CHAR)
	call mfree (ID_SECTION(id), TY_CHAR)
	call mfree (ID_DATABASE(id), TY_CHAR)
	call mfree (ID_COORDLIST(id), TY_CHAR)

	call mfree (ID_PIX(id), TY_DOUBLE)
	call mfree (ID_FIT(id), TY_DOUBLE)
	call mfree (ID_USER(id), TY_DOUBLE)
	call mfree (ID_WTS(id), TY_DOUBLE)
	call mfree (ID_FWIDTHS(id), TY_REAL)
	call mfree (ID_FTYPES(id), TY_INT)

	call mfree (id, TY_STRUCT)
end
