include <evvexpr.h>
include <imhdr.h>
include <mach.h>
include <fset.h>
include "../export.h"
include "../exbltins.h"


define	SZ_EPSSTRUCT	    5
define	EPS_ITEMSPERLINE    Memi[$1]		# no. of items per line
define	EPS_HPTR	    Memi[$1+1]		# ptr to hex digit string
define	EPS_BPTR	    Memi[$1+2]		# ptr to output buffer
define	EPS_BCNT	    Memi[$1+3]		# index into output buffer
define	HEXSTR		    Memc[EPS_HPTR($1)+$2]
define	BUF		    Memc[EPS_BPTR($1)+$2-1]

define	LINEWID		    36			# hexstr pixels per line
define	HEXITS         	    "0123456789abcdef"  # hex digits
define	MARGIN		    0.95		# defaults for 300 dpi
define	PAGEWID	    	    612
define	PAGEHGT	    	    762
define	SZ_EPSBUF	    8192
define	SZ_TRAILER	    31


# EX_EPS - Write the output image to an Encasulated PostScript file.

procedure ex_eps (ex)

pointer	ex				#i task struct pointer

pointer	eps
pointer	bptr
int	fd, len, flags

int	strlen()
bool	streq()

begin
        # Check to see that we have the correct number of expressions to
        # write this format.
        flags = EX_OUTFLAGS(ex)
        if ((EX_NEXPR(ex) != 1 && !bitset(flags, OF_BAND)) && EX_NEXPR(ex) != 3)
            call error (7, "Invalid number of expressions for EPS file.")

	# Set some of the output parameters.
	call ex_do_outtype (ex, "b1")
        EX_OUTFLAGS(ex) = or (EX_OUTFLAGS(ex), OF_FLIPY)

	# Allocate the EPS structure.
	iferr (call calloc (eps, SZ_EPSSTRUCT, TY_STRUCT))
	    call error (0, "Error allocating eps structure.")
	call calloc (EPS_HPTR(eps), 17, TY_CHAR)
	call calloc (EPS_BPTR(eps), SZ_EPSBUF+SZ_TRAILER, TY_CHAR)
	call strcpy (HEXITS, Memc[EPS_HPTR(eps)], 17)
	EPS_BCNT(eps) = 1

	# Now write out the header and image data.
	fd = EX_FD(ex)
	call fseti (fd, F_ADVICE, SEQUENTIAL)
	if (bitset (flags, OF_CMAP)) {
	    if (streq (CMAPFILE(ex),"grayscale") || 
		streq (CMAPFILE(ex),"greyscale")) {
	    	    call eps_header (ex, eps, NO)
	    	    call eps_gray (ex, eps, fd, false, true)
	    } else {
	        call eps_header (ex, eps, YES)
	        call eps_gray (ex, eps, fd, true, false)
	    }

	} else if (EX_NEXPR(ex) == 1 || bitset (flags, OF_BAND)) {
	    call eps_header (ex, eps, NO)
	    call eps_gray (ex, eps, fd, false, false)

	} else if (EX_NEXPR(ex) == 3) {
	    call eps_header (ex, eps, YES)
	    call eps_rgb (ex, eps, fd)
	}

	# Flush the remaining pixels in the buffer.
	call calloc (bptr, SZ_EPSBUF, TY_CHAR)

	if (mod (EPS_BCNT(eps),2) == 0) {
	    call amovc ("\ngrestore showpage\n%%Trailer\n\0", 
		BUF(eps,EPS_BCNT(eps)), SZ_TRAILER)
	} else {
	    call amovc ("\ngrestore  showpage\n%%Trailer\n", 
		BUF(eps,EPS_BCNT(eps)), SZ_TRAILER)
	}
	len = strlen (BUF(eps,1))
	call strpak (BUF(eps,1), Memc[bptr], len)
	call write (fd, Memc[bptr], len / SZB_CHAR)
	call flush (fd)

	# Write the EPS trailer and clean up the pointers.
	call mfree (EPS_HPTR(eps), TY_CHAR)
	call mfree (EPS_BPTR(eps), TY_CHAR)
	call mfree (eps, TY_STRUCT)
	call mfree (bptr, TY_CHAR)
end


# EPS_GRAY - Write a grayscale EPS file.

procedure eps_gray (ex, eps, fd, use_cmap, is_gray)

pointer	ex				#i task struct pointer
pointer	eps				#i postscript struct pointer
int	fd				#i output file descriptor
bool	use_cmap			#i write a false color image?
bool	is_gray				#i is this a grayscale cmap?

pointer	op, bop, out, cm
int	i, j, k, line, percent
int	len, orow, type

pointer	ex_evaluate(), ex_chtype()

begin
	# Now process the expressions and write the image.
        type = EX_OUTTYPE(ex)
        percent = 0
        orow = 0
	cm = EX_CMAP(ex)
	call malloc (out, EX_OCOLS(ex)+2, TY_SHORT)
        do i = 1, EX_NEXPR(ex) {

            # Process each line in the image.
            do j = 1, O_HEIGHT(ex,i) {

                # See if we're flipping the image.
                if (bitset (EX_OUTFLAGS(ex), OF_FLIPY))
                    line = EX_NLINES(ex) - j + 1
                else
                    line = j

                # Get pixels from image(s).
                call ex_getpix (ex, line)

                # Evaluate expression.
                op = ex_evaluate (ex, O_EXPR(ex,i))

                # Convert to the output pixel type.
                bop = ex_chtype (ex, op, type)

                # Write evaluated pixels.
		call achtbs (Memc[bop], Mems[out], O_LEN(op))
		len = O_LEN(op) - 1
		if (is_gray) {
            	    # Write a single color index as the grayscale value.
            	    do k = 0, len
                	call eps_putval (eps, fd, CMAP(cm,EX_RED,Mems[out+k]+1))
		} else if (use_cmap) {
            	    # Write index values as RGB triplets.
            	    do k = 0, len {
                	call eps_putval (eps, fd, 
			    CMAP(cm,EX_RED,  Mems[out+k]+1))
               		call eps_putval (eps, fd, 
			    CMAP(cm,EX_GREEN,Mems[out+k]+1))
                	call eps_putval (eps, fd, 
			    CMAP(cm,EX_BLUE, Mems[out+k]+1))
            	    }
		} else {
		    do k = 0, len
		        call eps_putval (eps, fd, Mems[out+k])
		}

                # Clean up the pointers.
                call mfree (bop, TY_CHAR)
                call evvfree (op)

                # Print percent done if being verbose
                orow = orow + 1
                #if (EX_VERBOSE(ex) == YES)
		    call ex_pstat (ex, orow, percent)
            }
        }
        call mfree (out, TY_SHORT)
end


# EPS_RGB - Write a RGB true color EPS file.

procedure eps_rgb (ex, eps, fd)

pointer	ex				#i task struct pointer
pointer	eps				#i postscript struct pointer
int	fd				#i output file descriptor

pointer	op, bop, out
int	i, j, k, line, percent, orow, type

pointer	ex_evaluate(), ex_chtype()

begin
	# Now process the expressions and write the image.
        type = EX_OUTTYPE(ex)
        percent = 0
        orow = 0
	call malloc (out, EX_OCOLS(ex)+2, TY_SHORT)
        do j = 1, EX_NLINES(ex) {

            # See if we're flipping the image.
            if (bitset (EX_OUTFLAGS(ex), OF_FLIPY))
                line = EX_NLINES(ex) - j + 1
            else
                line = j

            # Get pixels from image(s).
            call ex_getpix (ex, line)

            # Process each line in the image.
            do i = 1, EX_NEXPR(ex) {

                # Evaluate expression.
                op = ex_evaluate (ex, O_EXPR(ex,i))

                # Convert to the output pixel type.
                bop = ex_chtype (ex, op, type)

                # Write evaluated pixels.
		call achtbs (Memc[bop], Mems[out], O_LEN(op))
		do k = 1, O_LEN(op)
		    call eps_putval (eps, fd, Mems[out+k-1])

                # Clean up the pointers.
                call mfree (bop, TY_CHAR)
                call evvfree (op)
            }

            # Print percent done if being verbose
            orow = orow + 1
            #if (EX_VERBOSE(ex) == YES)
	        call ex_pstat (ex, orow, percent)
        }
        call mfree (out, TY_SHORT)
end


# EPS_HEADER - Write the EPS header block.

procedure eps_header (ex, eps, color)

pointer	ex				#i task struct pointer
pointer	eps				#i EPS struct pointer
int	color				#i is this a color image?

int	bp, fd, cols, rows, dpi, len
int	icols, irows, devpix, turnflag
real    scale, pixfac, scols, srows, llx, lly

int	strlen(), stropen()

begin
	fd = EX_FD(ex)
	turnflag = NO
	dpi = EX_PSDPI(ex)
	scale = EX_PSSCALE(ex)
	cols = EX_OCOLS(ex)
	rows = EX_OROWS(ex)

	# Open the buffer as a string file and print to it.
	bp = stropen (BUF(eps,1), SZ_EPSBUF, TEXT_FILE)

	# See if we need to rotate the image to fit on the page.
	icols = cols
	irows = rows
	if (cols > rows && (scale * cols) > int (PAGEWID * MARGIN)) {
	    turnflag = YES
	    cols = irows
	    rows = icols
	}

	# Figure out size.
	devpix = dpi / 72.0 + 0.5        	# device pixels per unit, approx
	pixfac = 72.0 / dpi * devpix    	# 1, approx.
	scols = scale * cols * pixfac
	srows = scale * rows * pixfac

	if ( scols > PAGEWID * MARGIN || srows > PAGEHGT * MARGIN ) {
	    if ( scols > PAGEWID * MARGIN ) {
	        scale = scale * PAGEWID / scols * MARGIN
	        scols = scale * cols * pixfac
	        srows = scale * rows * pixfac
	    }
	    if ( srows > PAGEHGT * MARGIN ) {
	        scale = scale * PAGEHGT / srows * MARGIN
	        scols = scale * cols * pixfac
	        srows = scale * rows * pixfac
	    }
	    if (EX_VERBOSE(ex) == YES) {
	        call printf ("\tImage too large for page, rescaled to %g\n")
		    call pargr (scale)
		call flush (STDOUT)
	    }
	}

	# Center it on the page.
	llx = (PAGEWID - scols) / 2
	lly = (PAGEHGT - srows) / 2

	call fprintf  (bp, "%%!PS-Adobe-2.0 EPSF-2.0\n")
	call fprintf  (bp, "%%%%Creator: IRAF EXPORT task\n")
	call fprintf  (bp, "%%%%Title: %s\n")
	    call pargstr (BFNAME(ex))
	call fprintf  (bp, "%%%%Pages: 1\n")
	call fprintf  (bp, "%%%%BoundingBox: %d %d %d %d\n")
	    call pargi (int (llx + 0.5))
	    call pargi (int (lly + 0.5))
	    call pargi (int (llx + scols))
	    call pargi (int (lly + srows))
	call fprintf  (bp, "%%%%EndComments\n")

	call fprintf (bp, "/readstring {\n")                # s -- s
	call fprintf (bp, "  currentfile exch readhexstring pop\n")
	call fprintf (bp, "} bind def\n")

	if (color == YES && !bitset (EX_OUTFLAGS(ex),OF_CMAP)) {
	    call eps_defcol (bp, icols)

	    call fprintf (bp, "/rpicstr %d string def\n")
	        call pargi (icols)
	    call fprintf (bp, "/gpicstr %d string def\n")
	        call pargi (icols)
	    call fprintf (bp, "/bpicstr %d string def\n")
	        call pargi (icols)

	} else if (color == YES && bitset (EX_OUTFLAGS(ex),OF_CMAP)) {
	    call eps_defcol (bp, icols)

	} else {
	    call fprintf (bp, "/picstr %d string def\n")
	        call pargi (icols)
	}

	call fprintf (bp, "%%%%EndProlog\n")
	call fprintf (bp, "%%%%Page: 1 1\n")
	call fprintf (bp, "gsave\n")
	call fprintf (bp, "%g %g translate\n")
	    call pargr (llx)
	    call pargr (lly)
	call fprintf (bp, "%g %g scale\n")
	    call pargr (scols)
	    call pargr (srows)

	if (turnflag == YES) {
	    call fprintf (bp, 
		"0.5 0.5 translate  90 rotate  -0.5 -0.5 translate\n")
	}

	call fprintf (bp, "%d %d 8\n")
	    call pargi (icols)
	    call pargi (irows)
	call fprintf (bp, "[ %d 0 0 -%d 0 %d ]\n")
	    call pargi (icols)
	    call pargi (irows)
	    call pargi (irows)
	if (color == YES) {
	    if (bitset (EX_OUTFLAGS(ex), OF_CMAP)) {
	        call fprintf (bp, "{currentfile pix readhexstring pop}\n")
	        call fprintf (bp, "false 3 colorimage")
	    } else {
	        call fprintf (bp, "{ rpicstr readstring }\n")
	        call fprintf (bp, "{ gpicstr readstring }\n")
	        call fprintf (bp, "{ bpicstr readstring }\n")
	        call fprintf (bp, "true 3  colorimage")
	    }
	} else {
	    call fprintf (bp, "{ picstr readstring }\n")
	    call fprintf (bp, "image")
	}
	call flush (bp)
	call strclose (bp)

	# See if we need to pad the string to write it out correctly.
	len = strlen(BUF(eps,1))
	if (mod(len,2) == 1) {
	    BUF(eps,len+1) = '\n'
	} else {
	    BUF(eps,len+1) = ' '
	    BUF(eps,len+2) = '\n'
	}

	# Now write the contents of the string buffer to the output file.
	len = strlen(BUF(eps,1))
	call strpak (BUF(eps,1), BUF(eps,1), len)
	call write (fd, BUF(eps,1), len / SZB_CHAR)
	call aclrc (BUF(eps,1), SZ_EPSBUF)
	EPS_ITEMSPERLINE(eps) = 0
end


# EPS_DEFCOL - Write out code that checks if the PostScript device in question
# knows about the 'colorimage' operator.  If it doesn't, it defines
# 'colorimage' in terms of image (ie, generates a greyscale image from
# RGB data).

procedure eps_defcol (fd, len)

int	fd				#i output file descriptor
int	len				#i length of a scanline

begin
  	call fprintf (fd, "%% build a temporary dictionary\n")
  	call fprintf (fd, "20 dict begin\n\n")
    	call fprintf (fd, 
	    "%% define string to hold a scanline's worth of data\n")
    	call fprintf (fd, "/pix %d string def\n\n")
	    call pargi (len)

	call fprintf (fd, "\n")
	call fprintf (fd, "%% define 'colorimage' if it isn't defined\n")
	call fprintf (fd, 
	    "/colorimage where   %% do we know about 'colorimage'?\n")
	call fprintf (fd, 
	    "  { pop }           %% yes: pop off the 'dict' returned\n")
	call fprintf (fd, "  {                 %% no:  define one\n")
	call fprintf (fd, "    /colortogray {  %% define an RGB->I function\n")
	call fprintf (fd, 
	    "      /rgbdata exch store    %% call input 'rgbdata'\n")
	call fprintf (fd, "      rgbdata length 3 idiv\n")
	call fprintf (fd, "      /npixls exch store\n")
	call fprintf (fd, "      /rgbindx 0 store\n")
	call fprintf (fd, 
	    "      /grays npixls string store  %% str to hold the result\n")
	call fprintf (fd, "      0 1 npixls 1 sub {\n")
	call fprintf (fd, "        grays exch\n")
	call fprintf (fd, 
	    "        rgbdata rgbindx       get 20 mul    %% Red\n")
	call fprintf (fd, 
	    "        rgbdata rgbindx 1 add get 32 mul    %% Green\n")
	call fprintf (fd, 
	    "        rgbdata rgbindx 2 add get 12 mul    %% Blue\n")
	call fprintf (fd, 
	    "        add add 64 idiv      %% I = .5G + .31R + .18B\n")
	call fprintf (fd, "        put\n")
	call fprintf (fd, "        /rgbindx rgbindx 3 add store\n")
	call fprintf (fd, "      } for\n")
	call fprintf (fd, "      grays\n")
	call fprintf (fd, "    } bind def\n\n")

	call fprintf (fd, "    %% Utility procedure for colorimage operator.\n")
	call fprintf (fd, 
	    "    %% This procedure takes two procedures off the\n")
	call fprintf (fd, 
	    "    %% stack and merges them into a single procedure.\n\n")
	
	call fprintf (fd, "    /mergeprocs { %% def\n")
	call fprintf (fd, "      dup length\n")
	call fprintf (fd, "      3 -1 roll\n")
	call fprintf (fd, "      dup\n")
	call fprintf (fd, "      length\n")
	call fprintf (fd, "      dup\n")
	call fprintf (fd, "      5 1 roll\n")
	call fprintf (fd, "      3 -1 roll\n")
	call fprintf (fd, "      add\n")
	call fprintf (fd, "      array cvx\n")
	call fprintf (fd, "      dup\n")
	call fprintf (fd, "      3 -1 roll\n")
	call fprintf (fd, "      0 exch\n")
	call fprintf (fd, "      putinterval\n")
	call fprintf (fd, "      dup\n")
	call fprintf (fd, "      4 2 roll\n")
	call fprintf (fd, "      putinterval\n")
	call fprintf (fd, "    } bind def\n\n")

	call fprintf (fd, "    /colorimage { %% def\n")
	call fprintf (fd, "      pop pop     %% remove 'false 3' operands\n")
	call fprintf (fd, "      {colortogray} mergeprocs\n")
	call fprintf (fd, "      image\n")
	call fprintf (fd, "    } bind def\n")
	call fprintf (fd, "  } ifelse          %% end of 'false' case\n")
	call fprintf (fd, "\n\n")
	call flush (fd)
end


# EPS_PUTVAL - Put a pixel value to the output file.

procedure eps_putval (eps, fd, sval)

pointer	eps				#i EPS struct pointer
int	fd				#i output file descriptor
short	sval				#i value to write

int	val, index
char	ch, nl, sp
int	shifti()

begin
	# Force value to 8-bit range.
        #val = max (0, min (255, sval))
        val = sval

	if (EPS_ITEMSPERLINE(eps) >= LINEWID) {
	    sp = ' '
	    call eps_putc (eps, fd, sp)
	    nl = '\n'
	    call eps_putc (eps, fd, nl)
	    EPS_ITEMSPERLINE(eps) = 0
	}

	# Get the hex string equivalent of the byte.
	index = shifti (val, -4)    			# get left 4 bits
	ch = HEXSTR(eps,index)
	call eps_putc (eps, fd, ch)

	index = and (val, 0FX)    			# get right 4 bits
	ch = HEXSTR(eps,index)
	call eps_putc (eps, fd, ch)

	EPS_ITEMSPERLINE(eps) = EPS_ITEMSPERLINE(eps) + 1
end


# EPS_PUTC - Put a character to the buffer.  This routine also flushes the
# accumulated buffer to disk once it fills.

procedure eps_putc (eps, fd, ch)

pointer	eps				#i EPS struct pointer
int	fd				#i file descriptor
char	ch				#i character to 'write'

begin
	BUF(eps,EPS_BCNT(eps)) = ch
	EPS_BCNT(eps) = EPS_BCNT(eps) + 1

	# If we're getting close to a full buffer, write it out.
	# Leave some space at the end for the epilogue.
	if (EPS_BCNT(eps) > SZ_EPSBUF-64) {
	    call strpak (BUF(eps,1), BUF(eps,1), EPS_BCNT(eps))
	    call write (fd, BUF(eps,1), EPS_BCNT(eps) / SZB_CHAR)
	    #call aclrc (BUF(eps,1), SZ_EPSBUF)
	    EPS_BCNT(eps) = 1
	}
end
