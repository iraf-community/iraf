# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>


# IMC_COPY -- Copy the input image to the output image.
# This procedure is called when there is only one image to combine.

procedure imc_copys (in, out)

pointer	in			# Input IMIO pointer
pointer	out			# Output IMIO pointer

int	nc
pointer	sp, v1, v2, indata, outdata, impnls(), imgnls()

begin
	call smark (sp)
	call salloc (v1, IM_MAXDIM, TY_LONG)
	call salloc (v2, IM_MAXDIM, TY_LONG)
	call amovkl (long(1), Meml[v1], IM_MAXDIM)
	call amovkl (long(1), Meml[v2], IM_MAXDIM)

	nc = IM_LEN(in,1)
	while ((impnls (out, outdata, Meml[v1]) != EOF) &&
	    (imgnls (in, indata, Meml[v2]) != EOF))
	    call amovs (Mems[indata], Mems[outdata], nc)

	call sfree (sp)
end

# IMC_COPY -- Copy the input image to the output image.
# This procedure is called when there is only one image to combine.

procedure imc_copyi (in, out)

pointer	in			# Input IMIO pointer
pointer	out			# Output IMIO pointer

int	nc
pointer	sp, v1, v2, indata, outdata, impnli(), imgnli()

begin
	call smark (sp)
	call salloc (v1, IM_MAXDIM, TY_LONG)
	call salloc (v2, IM_MAXDIM, TY_LONG)
	call amovkl (long(1), Meml[v1], IM_MAXDIM)
	call amovkl (long(1), Meml[v2], IM_MAXDIM)

	nc = IM_LEN(in,1)
	while ((impnli (out, outdata, Meml[v1]) != EOF) &&
	    (imgnli (in, indata, Meml[v2]) != EOF))
	    call amovi (Memi[indata], Memi[outdata], nc)

	call sfree (sp)
end

# IMC_COPY -- Copy the input image to the output image.
# This procedure is called when there is only one image to combine.

procedure imc_copyl (in, out)

pointer	in			# Input IMIO pointer
pointer	out			# Output IMIO pointer

int	nc
pointer	sp, v1, v2, indata, outdata, impnll(), imgnll()

begin
	call smark (sp)
	call salloc (v1, IM_MAXDIM, TY_LONG)
	call salloc (v2, IM_MAXDIM, TY_LONG)
	call amovkl (long(1), Meml[v1], IM_MAXDIM)
	call amovkl (long(1), Meml[v2], IM_MAXDIM)

	nc = IM_LEN(in,1)
	while ((impnll (out, outdata, Meml[v1]) != EOF) &&
	    (imgnll (in, indata, Meml[v2]) != EOF))
	    call amovl (Meml[indata], Meml[outdata], nc)

	call sfree (sp)
end

# IMC_COPY -- Copy the input image to the output image.
# This procedure is called when there is only one image to combine.

procedure imc_copyr (in, out)

pointer	in			# Input IMIO pointer
pointer	out			# Output IMIO pointer

int	nc
pointer	sp, v1, v2, indata, outdata, impnlr(), imgnlr()

begin
	call smark (sp)
	call salloc (v1, IM_MAXDIM, TY_LONG)
	call salloc (v2, IM_MAXDIM, TY_LONG)
	call amovkl (long(1), Meml[v1], IM_MAXDIM)
	call amovkl (long(1), Meml[v2], IM_MAXDIM)

	nc = IM_LEN(in,1)
	while ((impnlr (out, outdata, Meml[v1]) != EOF) &&
	    (imgnlr (in, indata, Meml[v2]) != EOF))
	    call amovr (Memr[indata], Memr[outdata], nc)

	call sfree (sp)
end

# IMC_COPY -- Copy the input image to the output image.
# This procedure is called when there is only one image to combine.

procedure imc_copyd (in, out)

pointer	in			# Input IMIO pointer
pointer	out			# Output IMIO pointer

int	nc
pointer	sp, v1, v2, indata, outdata, impnld(), imgnld()

begin
	call smark (sp)
	call salloc (v1, IM_MAXDIM, TY_LONG)
	call salloc (v2, IM_MAXDIM, TY_LONG)
	call amovkl (long(1), Meml[v1], IM_MAXDIM)
	call amovkl (long(1), Meml[v2], IM_MAXDIM)

	nc = IM_LEN(in,1)
	while ((impnld (out, outdata, Meml[v1]) != EOF) &&
	    (imgnld (in, indata, Meml[v2]) != EOF))
	    call amovd (Memd[indata], Memd[outdata], nc)

	call sfree (sp)
end

# IMC_COPY -- Copy the input image to the output image.
# This procedure is called when there is only one image to combine.

procedure imc_copyx (in, out)

pointer	in			# Input IMIO pointer
pointer	out			# Output IMIO pointer

int	nc
pointer	sp, v1, v2, indata, outdata, impnlx(), imgnlx()

begin
	call smark (sp)
	call salloc (v1, IM_MAXDIM, TY_LONG)
	call salloc (v2, IM_MAXDIM, TY_LONG)
	call amovkl (long(1), Meml[v1], IM_MAXDIM)
	call amovkl (long(1), Meml[v2], IM_MAXDIM)

	nc = IM_LEN(in,1)
	while ((impnlx (out, outdata, Meml[v1]) != EOF) &&
	    (imgnlx (in, indata, Meml[v2]) != EOF))
	    call amovx (Memx[indata], Memx[outdata], nc)

	call sfree (sp)
end

