include <evvexpr.h>
include "exbltins.h"
include "export.h"

procedure zze_prstruct (whence, ex)

char	whence[SZ_FNAME]
pointer	ex
int	i

begin
	call eprintf ("%s:\n") ; call pargstr (whence)
	call eprintf ("\tformat=%s %s outflags=%d interleave=%d bswap=%s\n")
	    switch (EX_FORMAT(ex)) {
	    case FMT_RAW: 	call pargstr ("FMT_RAW")
	    case FMT_LIST: 	call pargstr ("FMT_LIST")
	    case FMT_BUILTIN: 	call pargstr ("FMT_BUILTIN")
	    default: 		call pargstr ("ERR")
	    }
	    switch (EX_BLTIN(ex)) {
	    case EPS:		call pargstr ("(eps)")
	    case GIF:		call pargstr ("(gif)")
	    case PGM:		call pargstr ("(pgm)")
	    case PPM:		call pargstr ("(ppm)")
	    case RAS:		call pargstr ("(ras)")
	    case RGB:		call pargstr ("(rgb)")
	    case XWD:		call pargstr ("(xwd)")
	    default:		call pargstr ("")
	    }
	    call pargi (EX_OUTFLAGS(ex))
	    call pargi (EX_INTERLEAVE(ex))
	    switch(EX_BSWAP(ex)) {
	    case S_NONE: 	call pargstr ("S_NONE")
	    case S_ALL: 	call pargstr ("S_ALL")
	    case S_I2: 		call pargstr ("S_I2")
	    case S_I4: 		call pargstr ("S_I4")
	    default: 		call pargstr ("ERR")
	    }
	call eprintf ("\touttype=%s header='%s' verbose=%d\n")
	    switch(EX_OUTTYPE(ex)) {
	    case TY_SHORT: 	call pargstr ("TY_SHORT")
	    case TY_INT: 	call pargstr ("TY_INT")
	    case TY_LONG: 	call pargstr ("TY_LONG")
	    case TY_REAL: 	call pargstr ("TY_REAL")
	    case TY_DOUBLE: 	call pargstr ("TY_DOUBLE")
	    default: 		call pargstr ("ERR")
	    }
	    switch(EX_HEADER(ex)) {
	    case HDR_NONE: 	call pargstr ("HDR_NONE")
	    case HDR_SHORT: 	call pargstr ("HDR_SHORT")
	    case HDR_LONG: 	call pargstr ("HDR_LONG")
	    case HDR_USER: 	call pargstr ("HDR_USER")
	    default: 		call pargstr ("ERR")
	    }
	    call pargi (EX_VERBOSE(ex))
	call eprintf ("\toutbands (%d):\n") ; call pargi (EX_NEXPR(ex))
	do i = 1, EX_NEXPR(ex)
	    call zze_proband (ex, i)
	call eprintf ("\tocols=%d orows=%d:\n")
	    call pargi (EX_OCOLS(ex)) ; call pargi (EX_OROWS(ex))
	call eprintf ("\tnimages=%d nimops=%d ncols=%d nlines=%d:\n")
	    call pargi (EX_NIMAGES(ex))
	    call pargi (EX_NIMOPS(ex))
	    call pargi (EX_NCOLS(ex))
	    call pargi (EX_NLINES(ex))
	do i = 1, MAX_OPERANDS {
	    if (IMOP(ex,i) != NULL) {
	        call eprintf ("\t    ") ; call zze_prop (IMOP(ex,i))
	    }
	}

	call eprintf ("\tuser header = '%s'   LUT file = '%s'\n")
	    call pargstr (HDRFILE(ex))
	    call pargstr (LUTFILE(ex))
	call eprintf ("\tEPS dpi = %g  scale = %g  ncolors = %d\n")
	    call pargr (EX_PSDPI(ex))
	    call pargr (EX_PSSCALE(ex))
	    call pargi (EX_NCOLORS(ex))
	call eprintf ("\tbrightness = %g  contrast = %g\n")
	    call pargr (EX_BRIGHTNESS(ex))
	    call pargr (EX_CONTRAST(ex))
	call flush (STDERR)
end


procedure zze_proband (ex, band)

pointer	ex
int	band

begin
	call eprintf ("\t   ob=%d w=%d h=%d expr='%s'\n")
	    call pargi (OBANDS(ex,band))
	    call pargi (OB_WIDTH(OBANDS(ex,band)))
	    call pargi (OB_HEIGHT(OBANDS(ex,band)))
	    call pargstr (O_EXPR(ex,band))
end


procedure zze_prop (o)

pointer	o			
char    buf[8]
int     type, ex_ptype()

begin
	if (o == NULL) 
	    return

        call sprintf (buf, 8, " buirnx")
        type = ex_ptype(IO_TYPE(o), IO_NBYTES(o))
        call eprintf("(o=%d im=%d band=%d tag=%s (t='%c' N=%d=>%s) Np=%d %d)\n")
            call pargi (o)
            call pargi (IO_IMPTR(o))
            call pargi (IO_BAND(o))
	    if (IO_TAG(o) == NULL) call pargstr ("")
	    else 		   call pargstr (OP_TAG(o))
            #call pargc (buf[IO_TYPE(o)+1])
            call pargc (IO_TYPE(o))
            call pargi (IO_NBYTES(o))
            switch (type) {
            case TY_UBYTE:    call pargstr ("TY_UBYTE")
            case TY_USHORT:   call pargstr ("TY_USHORT")
            case TY_SHORT:    call pargstr ("TY_SHORT")
            case TY_INT:      call pargstr ("TY_INT")
            case TY_LONG:     call pargstr ("TY_LONG")
            case TY_REAL:     call pargstr ("TY_REAL")
            case TY_DOUBLE:   call pargstr ("TY_DOUBLE")
            default:          call pargstr ("ERR")
            }
            call pargi (IO_NPIX(o))
            call pargi (IO_DATA(o))
        call flush (STDERR)
end


procedure zze_pevop (o)

pointer	o

begin
	call eprintf ("o=%d type=%d len=%d flags=%d ")
	    call pargi (o)
	    call pargi (O_TYPE(o))
	    call pargi (O_LEN(o))
	    call pargi (O_FLAGS(o))
	switch (O_TYPE(o)) {
	case TY_CHAR: 	call eprintf ("val='%s'\n") ; call pargstr (O_VALC(o))
	case TY_SHORT: 	call eprintf ("val=%d\n") ; call pargs (O_VALS(o))
	case TY_INT: 	call eprintf ("val=%d\n") ; call pargi (O_VALI(o))
	case TY_LONG: 	call eprintf ("val=%d\n") ; call pargl (O_VALL(o))
	case TY_REAL: 	call eprintf ("val=%g\n") ; call pargr (O_VALR(o))
	case TY_DOUBLE: call eprintf ("val=%g\n") ; call pargd (O_VALD(o))
        default: 	call eprintf ("ptr=%d\n") ; call pargi (O_VALP(o))
	}
	call flush (STDERR)
end
