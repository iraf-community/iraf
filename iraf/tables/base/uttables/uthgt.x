# uthgtb -- get header parameter logical
# Read a parameter of type logical from the table header.
#
# P.E. Hodge, 16-Sep-87  Delete section to write error message.
# P.E. Hodge, 28-Dec-87  Different data types combined into one file.

procedure uthgtb (tp, f77par, value, istat)

pointer tp			# i: Pointer to table descriptor
%      character*(*) f77par
bool	value			# o: Value of parameter
int	istat			# o: Return status
#--
pointer sp
int	plen			# length of string f77par
pointer param			# scratch for SPP string version of f77par
int	errcode()
bool	tbhgtb()

begin
	istat = OK

%      plen = len (f77par)
	call smark (sp)
	call salloc (param, plen+1, TY_CHAR)
	call f77upk (f77par, Memc[param], plen)

	iferr (value = tbhgtb (tp, Memc[param])) {
	    istat = errcode()
	}
	call sfree (sp)
end
# uthgtd -- get header parameter double
# Read a parameter of type double from the table header.
#
# P.E. Hodge, 16-Sep-87  Delete section to write error message.

procedure uthgtd (tp, f77par, value, istat)

pointer tp			# i: Pointer to table descriptor
%      character*(*) f77par
double	value			# o: Value of parameter
int	istat			# o: Return status
#--
pointer sp
pointer param			# scratch for SPP string version of f77par
int	plen			# length of string f77par
int	errcode()
double	tbhgtd()

begin
	istat = OK

%      plen = len (f77par)
	call smark (sp)
	call salloc (param, plen+1, TY_CHAR)
	call f77upk (f77par, Memc[param], plen)

	iferr (value = tbhgtd (tp, Memc[param])) {
	    istat = errcode()
	}
	call sfree (sp)
end
# uthgti -- get header parameter integer
# Read a parameter of type integer from the table header.
#
# P.E. Hodge, 16-Sep-87  Delete section to write error message.

procedure uthgti (tp, f77par, value, istat)

pointer tp			# i: Pointer to table descriptor
%      character*(*) f77par
int	value			# o: Value of parameter
int	istat			# o: Return status
#--
pointer sp
pointer param			# scratch for SPP string version of f77par
int	plen			# length of string f77par
int	errcode()
int	tbhgti()

begin
	istat = OK

%      plen = len (f77par)
	call smark (sp)
	call salloc (param, plen+1, TY_CHAR)
	call f77upk (f77par, Memc[param], plen)

	iferr (value = tbhgti (tp, Memc[param])) {
	    istat = errcode()
	}
	call sfree (sp)
end
# uthgtr -- get header parameter real
# Read a parameter of type real from the table header.
#
# P.E. Hodge, 16-Sep-87  Delete section to write error message.

procedure uthgtr (tp, f77par, value, istat)

pointer tp			# i: Pointer to table descriptor
%      character*(*) f77par
real	value			# o: Value of parameter
int	istat			# o: Return status
#--
pointer sp
pointer param			# scratch for SPP string version of f77par
int	plen			# length of string f77par
int	errcode()
real	tbhgtr()

begin
	istat = OK

%      plen = len (f77par)
	call smark (sp)
	call salloc (param, plen+1, TY_CHAR)
	call f77upk (f77par, Memc[param], plen)

	iferr (value = tbhgtr (tp, Memc[param])) {
	    istat = errcode()
	}
	call sfree (sp)
end
# uthgtt -- get header parameter text
# Read a character string parameter from the table header.
# f77par and f77txt are the parameter name (input) and the text string
# (output) respectively.
#
# P.E. Hodge, 16-Sep-87  Delete section to write error message.

procedure uthgtt (tp, f77par, f77txt, istat)

pointer tp			# i: Pointer to table descriptor
%      character*(*) f77par
%      character*(*) f77txt
int	istat			# o: Return status

pointer sp
pointer param			# scratch for SPP string version of f77par
pointer text			# scratch for SPP string version of f77txt
int	plen			# length of string f77par
int	txtlen			# length of string f77txt
int	errcode()

begin
	istat = OK

%      plen = len (f77par)
%      txtlen = len (f77txt)
	call smark (sp)
	call salloc (param, plen+1, TY_CHAR)
	call f77upk (f77par, Memc[param], plen)
	call salloc (text, txtlen+1, TY_CHAR)

	iferr (call tbhgtt (tp, Memc[param], Memc[text], txtlen)) {
	    istat = errcode()
	    call sfree (sp)
	    return
	}
	call f77pak (Memc[text], f77txt, txtlen)
	call sfree (sp)
end
