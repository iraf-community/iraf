include <imhdr.h>
include "iralign.h"

# IR_INIT -- Initialize the ir structure

procedure ir_init (ir)

pointer	ir		# pointer to the ir strucuture

begin
	call malloc (ir, LEN_IRSTRUCT, TY_STRUCT)

	IR_IC1(ir) = NULL
	IR_IC2(ir) = NULL
	IR_IL1(ir) = NULL
	IR_IL2(ir) = NULL
	IR_OC1(ir) = NULL
	IR_OC2(ir) = NULL
	IR_OL1(ir) = NULL
	IR_OL2(ir) = NULL
	IR_DELTAX(ir) = NULL
	IR_DELTAY(ir) = NULL
	IR_DELTAI(ir) = NULL
	IR_XRSHIFTS(ir) = NULL
	IR_YRSHIFTS(ir) = NULL
	IR_NRSHIFTS(ir) = NULL
	IR_XCSHIFTS(ir) = NULL
	IR_YCSHIFTS(ir) = NULL
	IR_NCSHIFTS(ir) = NULL
end


# IR_PARAMS -- Get the ir structure parameters

procedure ir_params (ir, im, outim)

pointer	ir		# pointer to the ir strucuture
pointer	im		# pointer to the input image
pointer	outim		# pointer to the output image

int	nimcols, nimlines
real	rval
int	clgeti()
real	clgetr()

begin
	IR_NXRSUB(ir) = clgeti ("nxrsub")
	if (IS_INDEFI(IR_NXRSUB(ir)) || IR_NXRSUB(ir) < 1 || IR_NXRSUB(ir) >
	    IR_NXSUB(ir))
	    IR_NXRSUB(ir) = (IR_NXSUB(ir) + 1) / 2
	IR_NYRSUB(ir) = clgeti ("nyrsub")
	if (IS_INDEFI(IR_NYRSUB(ir)) || IR_NYRSUB(ir) < 1 || IR_NYRSUB(ir) >
	    IR_NYSUB(ir))
	    IR_NYRSUB(ir) = (IR_NYSUB(ir) + 1) / 2

	IR_XREF(ir) = clgeti ("xref")
	IR_YREF(ir) = clgeti ("yref")

	nimcols = clgeti ("nimcols")
	if (! IS_INDEFI(nimcols) && nimcols > 0 && nimcols >= IM_LEN(im,1))
	    IM_LEN(outim,1) = nimcols
	nimlines = clgeti ("nimlines")
	if (! IS_INDEFI(nimlines) && nimlines > 0 && nimlines >= IM_LEN(im,2))
	    IM_LEN(outim,2) = nimlines

	rval = clgetr ("oval")
	if (! IS_INDEFR(rval))
	    IR_OVAL(ir) = rval
end


# IR_ARRAYS -- Setup the ir structure arrays.

procedure ir_arrays (ir, nimages)

pointer	ir		# pointer to the ir strucuture
int	nimages		# number of images to be mosaiced

begin
	call malloc (IR_IC1(ir), nimages, TY_INT)
	call malloc (IR_IC2(ir), nimages, TY_INT)
	call malloc (IR_IL1(ir), nimages, TY_INT)
	call malloc (IR_IL2(ir), nimages, TY_INT)
	call malloc (IR_OC1(ir), nimages, TY_INT)
	call malloc (IR_OC2(ir), nimages, TY_INT)
	call malloc (IR_OL1(ir), nimages, TY_INT)
	call malloc (IR_OL2(ir), nimages, TY_INT)
	call malloc (IR_DELTAX(ir), nimages, TY_REAL)
	call malloc (IR_DELTAY(ir), nimages, TY_REAL)
	call malloc (IR_DELTAI(ir), nimages, TY_REAL)

	call malloc (IR_XRSHIFTS(ir), nimages, TY_REAL)
	call malloc (IR_YRSHIFTS(ir), nimages, TY_REAL)
	call malloc (IR_NRSHIFTS(ir), nimages, TY_INT)
	call malloc (IR_XCSHIFTS(ir), nimages, TY_REAL)
	call malloc (IR_YCSHIFTS(ir), nimages, TY_REAL)
	call malloc (IR_NCSHIFTS(ir), nimages, TY_INT)
end


# IR_FREE -- Free the ir strucuture.

procedure ir_free (ir)

pointer	ir		# pointer to the ir strucuture

begin
	if (IR_IC1(ir) != NULL)
	    call mfree (IR_IC1(ir), TY_INT)
	if (IR_IC2(ir) != NULL)
	    call mfree (IR_IC2(ir), TY_INT)
	if (IR_IL1(ir) != NULL)
	    call mfree (IR_IL1(ir), TY_INT)
	if (IR_IL2(ir) != NULL)
	    call mfree (IR_IL2(ir), TY_INT)
	if (IR_OC1(ir) != NULL)
	    call mfree (IR_OC1(ir), TY_INT)
	if (IR_OC2(ir) != NULL)
	    call mfree (IR_OC2(ir), TY_INT)
	if (IR_OL1(ir) != NULL)
	    call mfree (IR_OL1(ir), TY_INT)
	if (IR_OL2(ir) != NULL)
	    call mfree (IR_OL2(ir), TY_INT)

	if (IR_DELTAX(ir) != NULL)
	    call mfree (IR_DELTAX(ir), TY_REAL)
	if (IR_DELTAY(ir) != NULL)
	    call mfree (IR_DELTAY(ir), TY_REAL)
	if (IR_DELTAI(ir) != NULL)
	    call mfree (IR_DELTAI(ir), TY_REAL)

	if (IR_XRSHIFTS(ir) != NULL)
	    call mfree (IR_XRSHIFTS(ir), TY_REAL)
	if (IR_YRSHIFTS(ir) != NULL)
	    call mfree (IR_YRSHIFTS(ir), TY_REAL)
	if (IR_NRSHIFTS(ir) != NULL)
	    call mfree (IR_NRSHIFTS(ir), TY_INT)
	if (IR_XCSHIFTS(ir) != NULL)
	    call mfree (IR_XCSHIFTS(ir), TY_REAL)
	if (IR_YCSHIFTS(ir) != NULL)
	    call mfree (IR_YCSHIFTS(ir), TY_REAL)
	if (IR_NCSHIFTS(ir) != NULL)
	    call mfree (IR_NCSHIFTS(ir), TY_INT)

	if (ir != NULL)
	    call mfree (ir, TY_STRUCT)
end
