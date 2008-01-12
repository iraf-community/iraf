# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "stf.h"

define	NKW	4		# number of reserved header keywords


# STF_COPYFITS -- Copy the spooled FITS header, separating out the GPB cards
# and returning either or both types of cards on the two output streams.

procedure stf_copyfits (stf, spool, gpb, user)

pointer	stf			#I pointer to STF descriptor
int	spool			#I spooled header to read
int	gpb			#I stream to receive GPB cards, or NULL
int	user			#I stream to receive user cards, or NULL

bool	keyword
int	p_ch[MAX_PCOUNT+NKW]
pointer	p_len[MAX_PCOUNT+NKW]
pointer	p_namep[MAX_PCOUNT+NKW]
int	delim, ch, npars, ngpbpars, i
pointer	sp, lbuf, sbuf, pp, op, kw[NKW]
int	strncmp(), getline(), strlen(), gstrcpy()
errchk	getline, putline

begin
	call smark (sp)
	call salloc (lbuf, SZ_LINE, TY_CHAR)
	call salloc (sbuf, SZ_LINE, TY_CHAR)

	# The following reserved keywords describing the GPB are added to
	# the user area by stf_rdheader, and must be filtered out along with
	# the group parameters.  Since the number of reserved or group
	# parameters is normally small (only a dozen or so typically) a
	# simple 1 character - 2 thread hashing scheme is probably faster,
	# and certainly simpler, than a full hash table keyword lookup.

	op = sbuf
	npars = NKW
	kw[1] = op;  op = op + gstrcpy ("GROUPS", Memc[op], ARB) + 1
	kw[2] = op;  op = op + gstrcpy ("GCOUNT", Memc[op], ARB) + 1
	kw[3] = op;  op = op + gstrcpy ("PCOUNT", Memc[op], ARB) + 1
	kw[4] = op;  op = op + gstrcpy ("PSIZE",  Memc[op], ARB) + 1

	do i = 1, npars {
	    p_namep[i] = kw[i]
	    p_len[i] = strlen(Memc[kw[i]])
	    p_ch[i] = Memc[kw[i]+2]
	}

	# Add the GPB parameters to the list of group related parameters.
	ngpbpars = min (MAX_PCOUNT, STF_PCOUNT(stf))
	do i = 1, ngpbpars {
	    npars = npars + 1
	    pp = STF_PDES(stf,i)
	    p_namep[npars] = P_PTYPEP(pp)
	    p_len[npars] = strlen(P_PTYPE(pp))
	    p_ch[npars] = Memc[p_namep[npars]+2]
	}

	# Determine the type of each card and copy it to the appropriate
	# output stream.

	while (getline (spool, Memc[lbuf]) != EOF) {
	    # Does this user card redefine a reserved keyword?
	    keyword = false
	    ch = Memc[lbuf+2]
	    do i = 1, npars {
		if (ch != p_ch[i])
		    next
		delim = Memc[lbuf+p_len[i]]
		if (delim != ' ' && delim != '=')
		    next
		if (strncmp (Memc[lbuf], Memc[p_namep[i]], p_len[i]) == 0) {
		    keyword = true
		    break
		}
	    }

	    # Copy the card to the appropriate stream.
	    if (keyword) {
		if (gpb != NULL)
		    call putline (gpb, Memc[lbuf])
	    } else {
		if (user != NULL)
		    call putline (user, Memc[lbuf])
	    }
	}

	call sfree (sp)
end
