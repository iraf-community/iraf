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
