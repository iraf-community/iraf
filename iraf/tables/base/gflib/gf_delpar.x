#* HISTORY *
#* B.Simon	30-Sep-98	Rewriten to support all image types

# GF_DELPAR -- Delete a parameter from the group parameter block

procedure gf_delpar (im, pname)

pointer	im		# i: image descriptor
char	pname[ARB]	# i: parameter name
#--
bool	gf_geis()
int	imaccf()

begin
	if (gf_geis (im)) {
	    call gi_delpar (im, pname)

	} else if (imaccf (im, pname) == YES) {
	    call imdelf (im, pname)
	}
end
