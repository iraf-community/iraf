include <mach.h>
include "export.h"
include "exbltins.h"


# EXb_BUILTIN - Process a builtin format.  

procedure exb_process_image (ex)

pointer	ex				#i task struct pointer

begin
	# Branch to the appropriate procedure for processing.
	switch (EX_BLTIN(ex)) {
	case EPS:			# Encapsulated PostScript
	    call ex_eps (ex)
	case GIF:			# GIF
	    call ex_gif (ex)
	case IMH:			# IRAF OIF
	    call ex_iraf (ex)
	case MIFF:			# ImageMagick MIFF file
	    call ex_miff (ex)
	case PGM:			# PBMplus PGM (grayscale) file
	    call ex_pgm (ex)
	case PPM:			# PBMplus PPM (RGB) file
	    call ex_ppm (ex)
	case RAS:			# Sun rasterfile
	    call ex_ras (ex)
	case RGB:			# SGI RGB format file
	    call ex_rgb (ex)
	case XWD:			# X11 Window Dump
	    call ex_xwd (ex)
	case VICAR:			# JPL VICAR2 format image
	    call ex_vicar (ex)
	default:
	    call error (0, "Unrecognized format")
	}
end


# EXB_CHKPARS - Check the parameters for the builtin parameters.

int procedure exb_chkpars (ex)

pointer ex                              #i task struct pointer

int	legal, fmt

begin
        # Do a quick check that the number of expressions is valid for
        # the requested format.
        legal = NO
        fmt = EX_BLTIN(ex)
        switch (EX_NEXPR(ex)) {
        case 1:
            # PPM is the only format required to have 3 expressions.
            if (fmt != PPM)
                legal = YES
        case 3:
            if (fmt == PPM || fmt == RAS || fmt == RGB ||
                fmt == XWD || fmt == EPS || fmt == MIFF)
                    legal = YES
        case 4:
            if (fmt == RAS || fmt == XWD)
                legal = YES
        case EX_UNDEFINED:             	# let it slide for now....
            legal = YES
        default:
            if (bitset (EX_OUTFLAGS(ex), OF_BAND))
                legal = YES
        }
        if (legal == NO) {
            call error (1, "Wrong no. of expressions for requested format")
            return (ERR)
        }

        # Check the bswap param. If it's set but ignored by a given format
        # warn the user.
        if (EX_BSWAP(ex) != S_NONE && (fmt != RAS && fmt != XWD)) {
            call eprintf ("Warning: `bswap' parameter will be ignored")
            return (ERR)
        }

	return (OK)
end


# EXB_DO_FORMAT - Process a builtin task format parameter and set appropriate
# flags.

procedure exb_do_format (ex, format)

pointer ex                              #i task struct pointer
char    format[ARB]                     #i format parameter value

char    fmt[SZ_FNAME]
int     strdic()

begin
        switch (strdic (format, fmt, SZ_FNAME, EX_FORMATS)) {
        case EPS, EPSI, EPI, EPSF, PS:
            EX_BLTIN(ex) = EPS
            EX_COLOR(ex) = YES
        case GIF, GIFF:
            EX_BLTIN(ex) = GIF
            EX_COLOR(ex) = YES
        case IMH, IRAF:
            EX_BLTIN(ex) = IMH
            EX_COLOR(ex) = NO
        case MIFF:
            EX_BLTIN(ex) = MIFF
            EX_COLOR(ex) = YES
        case PGM:
            EX_BLTIN(ex) = PGM
            EX_COLOR(ex) = NO
        case PPM:
            EX_BLTIN(ex) = PPM
            EX_COLOR(ex) = NO
        case RAS, SUN, SUNRAS:
            EX_BLTIN(ex) = RAS
            EX_COLOR(ex) = YES
        case RGB, SGI, IRIS:
            EX_BLTIN(ex) = RGB
            EX_COLOR(ex) = NO
        case XWD, X11:
            EX_BLTIN(ex) = XWD
            EX_COLOR(ex) = YES
        case VICAR:
            EX_BLTIN(ex) = VICAR
            EX_COLOR(ex) = NO
        default:
            call error (2, "Unknown format.")
        }
end


# EXB_PNAME -  Print verbose name of the format.

procedure exb_pname (ex)

pointer ex                              #i task struct pointer

begin
        switch (EX_BLTIN(ex)) {
        case EPS:
	    call pargstr ("Encapsulated PostScript")
        case GIF:
	    call pargstr ("GIF")
        case MIFF:
	    call pargstr ("ImageMagick MIFF")
        case PGM:
	    call pargstr ("PGM")
        case PPM:
	    call pargstr ("PPM")
        case RAS:
	    call pargstr ("Sun Rasterfile")
        case RGB:
	    call pargstr ("SGI RGB")
        case XWD:
	    call pargstr ("X11 Window Dump")
        case VICAR:
	    call pargstr ("JPL VICAR2 Image")
        default:
	     call pargstr ("")
        }
end


# EXB_PENDIAN - Print byte order of the format.

procedure exb_pendian (ex)

pointer ex                              #i task struct pointer

begin
        switch (EX_BLTIN(ex)) {
        case GIF:    
	    call pargstr ("Least Significant Byte First")
        default:
            if (EX_BSWAP(ex) == 0 && (BYTE_SWAP2==NO || BYTE_SWAP4==NO))
               call pargstr ("Most Significant Byte First")
            else
                call pargstr ("Least Significant Byte First")
        }
end


# EXB_PSTORAGE - Print pixel storage type of the format.

procedure exb_pstorage (ex)

pointer ex                              #i task struct pointer

int	flags

begin
        switch (EX_BLTIN(ex)) {
        case GIF:    
	    call pargstr ("LZW compressed bytes")
        case RGB:    
	    call pargstr ("Band interleaved")
        default:
	    flags = EX_OUTFLAGS(ex)
            if (bitset(flags, OF_BAND) || bitset(flags,BAND_STORAGE))
                call pargstr ("Band Interleaved")
            else if (bitset(flags, OF_LINE) || bitset(flags,LINE_STORAGE))
                call pargstr ("Line Interleaved")
            else if (bitset(flags,PIXEL_STORAGE))
                call pargstr ("Pixel Interleaved")
            else
                call pargstr ("Unknown")
        }
end


# EXB_FMT_EXT - Print the name of the builtin format.  The returned pointer
# must be freed by the calling procedure.

pointer procedure exb_fmt_ext (ex)

pointer	ex				#i task struct pointer

pointer	suf

begin
	call malloc (suf, SZ_FNAME, TY_CHAR)

        switch (EX_BLTIN(ex)) {
        case EPS:    call strcpy (".eps",  Memc[suf], SZ_FNAME)
        case GIF:    call strcpy (".gif",  Memc[suf], SZ_FNAME)
        case IMH:    call strcpy (".imh",  Memc[suf], SZ_FNAME)
        case MIFF:   call strcpy (".miff", Memc[suf], SZ_FNAME)
        case PGM:    call strcpy (".pgm",  Memc[suf], SZ_FNAME)
        case PPM:    call strcpy (".ppm",  Memc[suf], SZ_FNAME)
        case RAS:    call strcpy (".ras",  Memc[suf], SZ_FNAME)
        case RGB:    call strcpy (".rgb",  Memc[suf], SZ_FNAME)
        case XWD:    call strcpy (".xwd",  Memc[suf], SZ_FNAME)
        case VICAR:  call strcpy (".vic",  Memc[suf], SZ_FNAME)
        default:     Memc[suf] = EOS
	}

	return (suf)
end
