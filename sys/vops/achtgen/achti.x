# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ACHT_ -- Convert an array of type _ to some other datatype.
# Data types are BUcsilrdx.

procedure achti (a, b, nelem, ty_b)

int	a[ARB]
char	b[ARB]
int	nelem
int	ty_b

begin
	switch (ty_b) {
	case TY_UBYTE:
	    call achtib (a, b, nelem)
	case TY_USHORT:
	    call achtiu (a, b, nelem)
	case TY_CHAR:
	    call achtic (a, b, nelem)
	case TY_SHORT:
	    call achtis (a, b, nelem)
	case TY_INT, TY_POINTER, TY_STRUCT:
	    call achtii (a, b, nelem)
	case TY_LONG:
	    call achtil (a, b, nelem)
	case TY_REAL:
	    call achtir (a, b, nelem)
	case TY_DOUBLE:
	    call achtid (a, b, nelem)
	case TY_COMPLEX:
	    call achtix (a, b, nelem)
	}
end
