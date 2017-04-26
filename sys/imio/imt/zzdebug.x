# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<syserr.h>
include	<imhdr.h>
include	<imset.h>
include <mach.h>
include "imx.h"


task	imt		= t_imt,
	parse		= t_parse,
	fnexpand	= t_fnexpand,
	prelist		= t_prelist,
	preproc		= t_preproc,
	breakout	= t_breakout,
	imexpand	= t_imexpand,
	fexpand		= t_fexpand



# IMT -- Test the image template package.

procedure t_imt ()

char	template[SZ_LINE]
char	image[SZ_FNAME]

pointer	imt, im, imtopen(), immap()
int	i, imtgetim()
bool	num, clgetb()

begin
	call clgstr ("in", template, SZ_LINE)
	num = clgetb ("number")

	imt = imtopen (template)

	for (i=0; imtgetim (imt, image, SZ_FNAME) != EOF; i=i+1) {
 
	    if (num) {
	        im = immap (image, READ_ONLY, 0)
		call printf ("%3d  %s  %d x %d\n") 
		    call pargi (i+1) 
		    call pargstr (image)
		    call pargi (IM_LEN(im,1))
		    call pargi (IM_LEN(im,2))
	        call imunmap (im)
	    } else {
	        if (i > 0)
		    call printf (",")
	        call printf ("%s")
		    call pargstr (image)
	    }
	}
	call printf ("\n")
	call printf ("Nimages = %d\n")
	    call pargi (i)

	call imtclose (imt)
end


# PARSE -- Test the image template package expression parse.

procedure t_parse ()

char	template[SZ_LINE], name[SZ_LINE], index[SZ_LINE], ikparams[SZ_LINE]
char	extname[SZ_LINE], extver[SZ_LINE], expr[SZ_LINE], sec[SZ_LINE]

int	nch, imx_parse()

begin
	call clgstr ("in", template, SZ_LINE)

	nch = imx_parse (template, name, index, extname, extver, 
	    expr, sec, ikparams, SZ_LINE)

	call eprintf ("%s\n")			; call pargstr (template)
	call eprintf ("\tname\t= %s\n")		; call pargstr (name)
	call eprintf ("\tindex\t= %s\n")	; call pargstr (index)
	call eprintf ("\textname\t= %s\n")	; call pargstr (extname)
	call eprintf ("\textver\t= %s\n")	; call pargstr (extver)
	call eprintf ("\texpr\t= %s\n")		; call pargstr (expr)
	call eprintf ("\tikparams\t= %s\n")	; call pargstr (ikparams)
	call eprintf ("\tsec\t= %s\n")		; call pargstr (sec)
end


# FNEXPAND -- Test the image template package pre-processor.

procedure t_fnexpand ()

char	template[SZ_LINE]

pointer	pp, imx_fnexpand()

begin
	call clgstr ("in", template, SZ_LINE)

	pp = imx_fnexpand (template)

	call eprintf ("%s\n")
	    call pargstr (Memc[pp])
	call mfree (pp, TY_CHAR)
end


# PRELIST -- Test the image template package pre-processor.

procedure t_prelist ()

char	template[SZ_LINE]

pointer	pp, imx_preproc_list()

begin
	call clgstr ("in", template, SZ_LINE)

	pp = imx_preproc_list (template)

	call eprintf ("%s\n")
	    call pargstr (Memc[pp])
	call mfree (pp, TY_CHAR)
end


# PREPROC -- Test the image template package pre-processor.

procedure t_preproc ()

char	template[SZ_LINE]

pointer	pp, imx_preproc()

begin
	call clgstr ("in", template, SZ_LINE)

	pp = imx_preproc (template)

	call eprintf ("%s\n")
	    call pargstr (Memc[pp])
	call mfree (pp, TY_CHAR)
end


# BREAKOUT -- Test the image template package expression breakout code.

procedure t_breakout ()

char	template[SZ_LINE]

int	nchars
char	image[SZ_LINE], expr[SZ_LINE], sec[SZ_LINE], ikparams[SZ_LINE]

int	imx_breakout()

begin
	call clgstr ("in", template, SZ_LINE)

	nchars = imx_breakout(template, NO, image, expr, sec, ikparams, SZ_LINE)

	call eprintf ("nchars=%d  image='%s'  expr='%s'  sec='%s'  ik='%s'\n")
	    call pargi (nchars)
	    call pargstr (image)
	    call pargstr (expr)
	    call pargstr (sec)
	    call pargstr (ikparams)
end


# IMEXPAND -- Test the MEF image expansion.

procedure t_imexpand ()

char	template[SZ_LINE]
int	nimages

pointer	imt, imx_imexpand()

begin
	call clgstr ("in", template, SZ_LINE)

	imt = imx_imexpand (template, 
			  "", 			# expr
			  "", 			# index
			  "", 			# extname
			  "", 			# extver
			  "", 			# ikparams
			  "", 			# sections
			  nimages)

	call printf ("nimages = %d\n%s\n"); 
	    call pargi (nimages)
	    call pargstr (Memc[imt])

	call mfree (imt, TY_CHAR)
end


# FEXPAND -- Test the filename expansion.

procedure t_fexpand ()

char	template[SZ_LINE]
int	nimages

pointer	imt, imx_fexpand()

begin
	call clgstr ("in", template, SZ_LINE)

	imt = imx_fexpand (template, 
			  "", 			# expr
			  "", 			# index
			  "", 			# extname
			  "", 			# extver
			  "", 			# ikparams
			  "", 			# sections
			  nimages)

	call printf ("nimages = %d\n%s\n"); 
	    call pargi (nimages)
	    call pargstr (Memc[imt])

	call mfree (imt, TY_CHAR)
end
