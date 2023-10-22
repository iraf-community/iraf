
task tbbnll = t_tbbnll


# T_TBBNLL -- Test the TBNLL procedures that convert an INDEF value to a
# char array.  The intent here is to test the effect of the 'equivalence'
# statements on whether the values are properly aligned for the platform.
# The input and output values should be printed as the same values.
#
# Compile:      xc zztest.e -ltbtables


procedure t_tbbnll ()

short	sbuf
bool	bbuf
char    out[SZ_FNAME]

begin
        call pbuf ("in real", INDEFR, SZ_REAL)
	call tbbeqr (INDEFR, out)
        call pbuf ("out real", out, SZ_REAL)

        call pbuf ("in double", INDEFD, SZ_REAL)
	call tbbeqd (INDEFD, out)
        call pbuf ("out double", out, SZ_REAL)

        call pbuf ("in int", INDEFI, SZ_INT32)
	call tbbeqi (INDEFI, out)
        call pbuf ("out int", out, SZ_INT32)

        sbuf = INDEFS
        call pbuf ("in short", sbuf, SZ_SHORT)
	call tbbeqs (sbuf, out)
        call pbuf ("out short", out, SZ_SHORT)

        bbuf = false
        call pbuf ("in bool", bbuf, SZ_BOOL)
	call tbbeqb (bbuf, out)
        call pbuf ("out bool", out, SZ_BOOL)
end

procedure pbuf (what, out, sz)
char    what[ARB]
char    out[ARB]
int     sz
int     i
begin
    call eprintf ("%-16s\t")
        call pargstr(what)
    do i = 1, sz {
        call eprintf ("%d ")
            call pargc (out[i])
    }
    call eprintf ("\n")
end
