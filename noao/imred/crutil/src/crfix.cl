# CRFIX -- Replace cosmic rays in an image using a cosmic ray mask.

procedure crfix (input, output, crmask)

file	input 			{prompt="Input image"}
file	output 			{prompt="Output image"}
file	crmask 			{prompt="Cosmic ray mask"}

begin
	file	in, out, crm

	in = input
	out = output
	crm = crmask

	if (in != out)
	    imcopy (in, out, verbose=no)
	fixpix (out, crm, linterp="INDEF", cinterp="INDEF", verbose=no,
	    pixels=no)
end
