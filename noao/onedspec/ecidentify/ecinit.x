include	<gset.h>
include	"ecidentify.h"

# EC_INIT -- Allocate and initialize the identify structure.

procedure ec_init (ec)

pointer	ec			# ID pointer

begin
	call calloc (ec, LEN_EC, TY_STRUCT)

	EC_NALLOC(ec) = 20
	EC_NFEATURES(ec) = 0
	EC_CURRENT(ec) = 0
	EC_NLINES(ec) = 0
	EC_LL(ec) = NULL
	EC_ECF(ec) = NULL
	EC_LABELS(ec) = 1

	call malloc (EC_IMAGE(ec), SZ_FNAME, TY_CHAR)
	call malloc (EC_DATABASE(ec), SZ_FNAME, TY_CHAR)
	call malloc (EC_COORDLIST(ec), SZ_FNAME, TY_CHAR)

	call malloc (EC_APNUM(ec), EC_NALLOC(ec), TY_INT)
	call malloc (EC_LINENUM(ec), EC_NALLOC(ec), TY_INT)
	call malloc (EC_PIX(ec), EC_NALLOC(ec), TY_DOUBLE)
	call malloc (EC_ORD(ec), EC_NALLOC(ec), TY_INT)
	call malloc (EC_FIT(ec), EC_NALLOC(ec), TY_DOUBLE)
	call malloc (EC_USER(ec), EC_NALLOC(ec), TY_DOUBLE)
	call malloc (EC_FWIDTHS(ec), EC_NALLOC(ec), TY_REAL)
	call malloc (EC_FTYPES(ec), EC_NALLOC(ec), TY_INT)
end


# EC_FREE -- Free identify structure.

procedure ec_free (ec)

pointer	ec				# ID pointer
int	i

begin
	if (EC_UN(ec) != NULL)
	    call un_close (EC_UN(ec))
	do i = 1, EC_NLINES(ec)
	    call shdr_close (SH(ec,i))
	call mfree (EC_SHS(ec), TY_POINTER)

	call mfree (EC_IMAGE(ec), TY_CHAR)
	call mfree (EC_DATABASE(ec), TY_CHAR)
	call mfree (EC_COORDLIST(ec), TY_CHAR)

	call mfree (EC_APNUM(ec), TY_INT)
	call mfree (EC_LINENUM(ec), TY_INT)
	call mfree (EC_PIX(ec), TY_DOUBLE)
	call mfree (EC_ORD(ec), TY_INT)
	call mfree (EC_FIT(ec), TY_DOUBLE)
	call mfree (EC_USER(ec), TY_DOUBLE)
	call mfree (EC_FWIDTHS(ec), TY_REAL)
	call mfree (EC_FTYPES(ec), TY_INT)

	call mfree (ec, TY_STRUCT)
end
