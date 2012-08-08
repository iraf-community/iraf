# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <error.h>
include <imhdr.h>
include <fset.h>

# T_CHPIXTYPE -- Change the pixel type of a list of images from the specified
# old pixel type to the new pixel type. The input images to be converted can
# be slected by pixel type. Conversion from one pixel type to another is
# direct and may involve loss of precision and dynamic range. Mapping of
# floating point numbers to integer numbers is done by truncation.


define  CHP_ALL         1               # All types
define  CHP_USHORT      2               # Unsigned short integer
define  CHP_SHORT       3               # Short integers
define  CHP_INT         4               # Integers
define  CHP_LONG        5               # Long integers
define  CHP_REAL        6               # Reals
define  CHP_DOUBLE      7               # Doubles
define  CHP_COMPLEX     8               # Complex

define	CHP_TYSTR	"|all|ushort|short|int|long|real|double|complex|"

procedure t_chpixtype()

pointer imtlist1                # Input image list
pointer imtlist2                # Output image list

pointer image1                  # Input image
pointer image2                  # Output image
pointer imtemp                  # Temporary file

int     list1, list2, intype, outtype, verbose
pointer im1, im2, sp, instr, outstr, imstr
bool    clgetb()
int     imtopen(), imtgetim(), imtlen(), clgwrd(), chp_gettype(), btoi()
pointer immap()

errchk	xt_mkimtemp, immap, imunmap, xt_delimtemp, chp_pixtype

begin
        call fseti (STDOUT, F_FLUSHNL, YES)

        # Allocate temporary space.
        call smark (sp)
        call salloc (imtlist1, SZ_FNAME, TY_CHAR)
        call salloc (imtlist2, SZ_FNAME, TY_CHAR)
        call salloc (image1, SZ_FNAME, TY_CHAR)
        call salloc (image2, SZ_FNAME, TY_CHAR)
        call salloc (imtemp, SZ_FNAME, TY_CHAR)
        call salloc (instr, SZ_LINE, TY_CHAR)
        call salloc (outstr, SZ_LINE, TY_CHAR)
        call salloc (imstr, SZ_LINE, TY_CHAR)

        # Get task parameters.
        call clgstr ("input", Memc[imtlist1], SZ_FNAME)
        call clgstr ("output", Memc[imtlist2], SZ_FNAME)

        # Get the input and output pixel types.
        intype =  clgwrd ("oldpixtype", Memc[instr], SZ_LINE, CHP_TYSTR)
        outtype = clgwrd ("newpixtype", Memc[outstr], SZ_LINE, CHP_TYSTR)
        verbose = btoi (clgetb ("verbose"))

        list1 = imtopen (Memc[imtlist1])
        list2 = imtopen (Memc[imtlist2])
        if (imtlen (list1) != imtlen (list2)) {
            call imtclose (list1)
            call imtclose (list2)
            call error (0, "Number of input and output images not the same.")
        }

        # Loop over the set of input and output images
        while ((imtgetim (list1, Memc[image1], SZ_FNAME) != EOF) &&
              (imtgetim (list2, Memc[image2], SZ_FNAME) != EOF)) {
            
            iferr {

                # Open the input and output images.
                call xt_mkimtemp (Memc[image1], Memc[image2], Memc[imtemp],
                    SZ_FNAME)
                im1 = immap (Memc[image1], READ_ONLY, 0)
                if (intype == CHP_ALL || IM_PIXTYPE(im1) == chp_gettype(intype))
                    im2 = immap (Memc[image2], NEW_COPY, im1)
                else
                    im2 = NULL

                # Change the pixel type.
                call chp_enctype (IM_PIXTYPE(im1), Memc[imstr], SZ_LINE)
                if (im2 == NULL) {
                    if (verbose == YES) {
                        call printf ("Cannot change Image: %s (%s) -> ")
                            call pargstr (Memc[image1])
                            call pargstr (Memc[imstr])
			call printf ("Image: %s (%s)\n")
                            call pargstr (Memc[imtemp])
                            call pargstr (Memc[outstr])
                    }
                } else {
                    if (verbose == YES) {
                        call printf ("Image: %s (%s) -> Image: %s (%s)\n")
                            call pargstr (Memc[image1])
                            call pargstr (Memc[imstr])
                            call pargstr (Memc[imtemp])
                            call pargstr (Memc[outstr])
                    }
                    call chp_pixtype (im1, im2, chp_gettype (outtype))
                }

                # Close up the input and output images.
                call imunmap (im1)
                if (im2 != NULL) {
                    call imunmap (im2)
                    call xt_delimtemp (Memc[image2], Memc[imtemp])
                }

            } then {
                call eprintf ("Error converting %s (%s) -> (%s)\n")
                    call pargstr (Memc[image1])
                    call pargstr (Memc[imstr])
                    call pargstr (Memc[outstr])
                call erract (EA_WARN)
            }
        }

        call imtclose (list1)
        call imtclose (list2)

        call sfree (sp)
end


# CHP_PIXTYPE -- Change pixel types using line sequential image i/o.

procedure chp_pixtype (im1, im2, outtype)

pointer im1             # pointer to the input image
pointer im2             # pointer to the output image
int     outtype         # output pixel type

int     ncols
long    v1[IM_MAXDIM], v2[IM_MAXDIM]
pointer buf1, buf2
int     imgnls(), imgnli(), imgnll(), imgnlr(), imgnld(), imgnlx()
int     impnls(), impnli(), impnll(), impnlr(), impnld(), impnlx()

errchk	imgnls, imgnli, imgnll, imgnlr, imgnld, imgnlx
errchk	impnls, impnli, impnll, impnlr, impnld, impnlx

begin
        ncols = IM_LEN(im1, 1)

        IM_PIXTYPE(im2) = outtype
        call amovkl (long(1), v1, IM_MAXDIM)
        call amovkl (long(1), v2, IM_MAXDIM)

        switch (outtype) {
        case TY_USHORT:
            while (impnll(im2,buf2,v2) != EOF && imgnll(im1,buf1,v1) != EOF)
                call amovl (Meml[buf1], Meml[buf2], ncols)
        case TY_SHORT:
            while (impnls(im2,buf2,v2) != EOF && imgnls(im1,buf1,v1) != EOF)
                call amovs (Mems[buf1], Mems[buf2], ncols)
        case TY_INT:
            while (impnli(im2,buf2,v2) != EOF && imgnli(im1,buf1,v1) != EOF)
                call amovi (Memi[buf1], Memi[buf2], ncols)
        case TY_LONG:
            while (impnll(im2,buf2,v2) != EOF && imgnll(im1,buf1,v1) != EOF)
                call amovl (Meml[buf1], Meml[buf2], ncols)
        case TY_REAL:
            while (impnlr(im2,buf2,v2) != EOF && imgnlr(im1,buf1,v1) != EOF)
                call amovr (Memr[buf1], Memr[buf2], ncols)
        case TY_DOUBLE:
            while (impnld(im2,buf2,v2) != EOF && imgnld(im1,buf1,v1) != EOF)
                call amovd (Memd[buf1], Memd[buf2], ncols)
        case TY_COMPLEX:
            while (impnlx(im2,buf2,v2) != EOF && imgnlx(im1,buf1,v1) != EOF)
                call amovx (Memx[buf1], Memx[buf2], ncols)
        }

	call imflush (im2)
end


# CHP_GETTYPE -- Get the the image pixel type.

int procedure chp_gettype (intype)

int     intype          # input pixel type

begin
        switch (intype) {
        case CHP_USHORT:
            return (TY_USHORT)
        case CHP_SHORT:
            return (TY_SHORT)
        case CHP_INT:
            return (TY_INT)
        case CHP_LONG:
            return (TY_LONG)
        case CHP_REAL:
            return (TY_REAL)
        case CHP_DOUBLE:
            return (TY_DOUBLE)
        case CHP_COMPLEX:
            return (TY_COMPLEX)
        default:
            return (ERR)
        }
end


# CHP_ENCTYPE -- Encode the pixel type string.

procedure chp_enctype (pixtype, str, maxch)

int     pixtype         # pixel type
char    str[ARB]        # string for encoding pixel type
int     maxch           # maximum characters

begin
        switch (pixtype) {
        case TY_USHORT:
            call strcpy ("ushort", str, maxch)
        case TY_SHORT:
            call strcpy ("short", str, maxch)
        case TY_INT:
            call strcpy ("int", str, maxch)
        case TY_LONG:
            call strcpy ("long", str, maxch)
        case TY_REAL:
            call strcpy ("real", str, maxch)
        case TY_DOUBLE:
            call strcpy ("double", str, maxch)
        case TY_COMPLEX:
            call strcpy ("complex", str, maxch)
        }
end
