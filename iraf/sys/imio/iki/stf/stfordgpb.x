# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	"stf.h"

# STF_ORDERGPB -- Order the GPB, putting the group parameters in the
# new image in the same order as in the old image.
# NOTE:  no longer called by stf_opix; save this code for future use!
# <dlb--11/4/87>

procedure stf_ordergpb (o_stf, n_stf)

pointer	o_stf			# STF descriptor of old image
pointer n_stf			# STF descriptor of new image

pointer	sp, temp_pdes, pp, o_plist, n_plist
int	o_pcount, n_pcount, otop, ntop, op, np, offset, sz_param, pn
bool	streq()

begin
	o_pcount = STF_PCOUNT(o_stf)
	n_pcount = STF_PCOUNT(n_stf)
	if (o_pcount <= 0)
	    return

	call smark (sp)
	call salloc (temp_pdes, LEN_PDES, TY_STRUCT)

	o_plist = STF_PDES(o_stf,1)
	n_plist = STF_PDES(n_stf,1)
	otop = (o_pcount * LEN_PDES)
	ntop = (n_pcount * LEN_PDES)

	# Search the new parameter list for a parameter with the same name
	# as a parameter in the old parameter list.  When a match is found,
	# move the new parameter into the same position as it is in the
	# old parameter list.

	for (op=0; op < otop; op=op+LEN_PDES)
	    for (np=op; np < ntop; np=np+LEN_PDES)
		if (streq (P_PTYPE(o_plist+op), P_PTYPE(n_plist+np))) {
		    if (op != np) {
			# Swap parameters between old and new positions
			call amovi (Memi[n_plist+op], Memi[temp_pdes],
			    LEN_PDES)
			call amovi (Memi[n_plist+np], Memi[n_plist+op],
			    LEN_PDES)
			call amovi (Memi[temp_pdes], Memi[n_plist+np],
			    LEN_PDES)
		    }
		    break
		}

	# Update the field offsets.
	offset = 0
	for (pn=1;  pn <= n_pcount;  pn=pn+1) {
	    pp = STF_PDES(n_stf,pn)
	    P_OFFSET(pp) = offset
	    sz_param = P_PSIZE(pp) / NBITS_BYTE / SZB_CHAR
	    offset = offset + sz_param
	}

	call sfree (sp)
end
