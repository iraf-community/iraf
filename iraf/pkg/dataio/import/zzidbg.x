include <evvexpr.h>
include "import.h"

procedure zzi_prstruct (whence, ip)

char	whence[SZ_FNAME]
pointer	ip
int	i

begin
	call eprintf ("%s:\n") ; call pargstr (whence)
	call eprintf ("\tformat=%s interleave=%d bswap=%s\n")
	    switch (IP_FORMAT(ip)) {
	    case IP_NONE: 	call pargstr ("IP_NONE")
	    case IP_SENSE: 	call pargstr ("IP_SENSE")
	    case IP_NAME: 	call pargstr ("IP_NAME")
	    case IP_BUILTIN: 	call pargstr ("IP_BUILTIN")
	    default: 		call pargstr ("ERR")
	    }
	    call pargi (IP_INTERLEAVE(ip))
	    switch(IP_SWAP(ip)) {
	    case S_NONE: 	call pargstr ("S_NONE")
	    case S_ALL: 	call pargstr ("S_ALL")
	    case S_I2: 		call pargstr ("S_I2")
	    case S_I4: 		call pargstr ("S_I4")
	    default: 		call pargstr ("ERR")
	    }
	call eprintf ("\thskip=%d tskip=%d bskip=%d lskip=%d lpad=%d\n")
	    call pargi (IP_HSKIP(ip))
	    call pargi (IP_TSKIP(ip))
	    call pargi (IP_BSKIP(ip))
	    call pargi (IP_LSKIP(ip))
	    call pargi (IP_LPAD(ip))
	call eprintf ("\tndim=%s  dims=(%d,%d,%d,%d,%d,%d,%d)\n")
	    call pargi (IP_NDIM(ip))
	    do i = 1, 7
	        call pargi (IP_AXLEN(ip,i))

	call eprintf ("\toutput=%s outtype=%s imheader='%s' verbose=%d\n")
	    switch(IP_OUTPUT(ip)) {
	    case IP_NONE: 	call pargstr ("IP_NONE")
	    case IP_IMAGE: 	call pargstr ("IP_IMAGE")
	    case IP_LIST: 	call pargstr ("IP_LIST")
	    case IP_INFO: 	call pargstr ("IP_INFO")
	    default: 		call pargstr ("ERR")
	    }
	    switch(IP_OUTTYPE(ip)) {
	    case TY_SHORT: 	call pargstr ("TY_SHORT")
	    case TY_INT: 	call pargstr ("TY_INT")
	    case TY_LONG: 	call pargstr ("TY_LONG")
	    case TY_REAL: 	call pargstr ("TY_REAL")
	    case TY_DOUBLE: 	call pargstr ("TY_DOUBLE")
	    default: 		call pargstr ("ERR")
	    }
	    if (IP_IMHEADER(ip) == NULL)
	        call pargstr ("")
	    else
	        call pargstr (Memc[IP_IMHEADER(ip)])
	    call pargi (IP_VERBOSE(ip))
	call eprintf ("\tpixtype:\n")
	do i = 1, IP_NPIXT(ip) {
	    call eprintf ("\t  ")
	    call zzi_prop (PTYPE(ip,i))
	}
	call eprintf ("\toutbands:\n")
	do i = 1, IP_NBANDS(IP) {
	    call eprintf ("\t  ")
	    call zzi_proband (ip, i)
	}
	call flush (STDERR)
end


procedure zzi_proband (ip,band)

pointer	ip			
int	band

begin
	call eprintf ("ob=%d expr='%s' op->")
	    call pargi (OBANDS(ip,band))
	    call pargstr (O_EXPR(ip,band))
	call zzi_prop (O_OP(ip,band))
end


procedure zzi_prop (o)

pointer	o			
char	buf[8]
int	type, ip_ptype()

begin
	call sprintf (buf, 8, " buirnx")
        type = ip_ptype(IO_TYPE(o), IO_NBYTES(o))
	call eprintf ("(o=%d expr='%s' tag='%s' (t='%c' N=%d=>%s) Np=%d  %d)\n")
	    call pargi (o)
	    call pargstr (Memc[OB_EXPR(o)])
	    call pargstr (OP_TAG(o))
	    call pargc (buf[IO_TYPE(o)+1])
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


procedure zzi_pevop (o)

pointer	o

begin
	call eprintf ("o=%d type=%d len=%d flags=%d ")
	    call pargi (o)
	    call pargi (O_TYPE(o))
	    call pargi (O_LEN(o))
	    call pargi (O_FLAGS(o))
	switch (O_TYPE(o)) {
	case TY_CHAR:
	    call eprintf ("val='%s'\n") ; call pargstr (O_VALC(o))
	case TY_SHORT:
	    call eprintf ("val=%d\n") ; call pargs (O_VALS(o))
	case TY_INT:
	    call eprintf ("val=%d\n") ; call pargi (O_VALI(o))
	case TY_LONG:
	    call eprintf ("val=%d\n") ; call pargl (O_VALL(o))
	case TY_REAL:
	    call eprintf ("val=%g\n") ; call pargr (O_VALR(o))
	case TY_DOUBLE:
	    call eprintf ("val=%g\n") ; call pargd (O_VALD(o))
        default:
	    call eprintf ("ptr=%d\n") ; call pargi (O_VALP(o))
	}
	call flush (STDERR)
end
