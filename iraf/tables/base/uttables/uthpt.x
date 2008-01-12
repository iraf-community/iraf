# uthptb -- put header parameter logical
# Write a parameter of type logical to the table header.
#
# P.E. Hodge, 16-Sep-87  Delete section to write error message.
# P.E. Hodge, 28-Dec-87  Different data types combined into one file.

procedure uthptb (tp, f77par, value, istat)

pointer tp			# i: Pointer to table descriptor
%      character*(*) f77par
bool	value			# i: Value of parameter
int	istat			# o: Return status
#--
pointer sp			# stack pointer
pointer param			# scratch for SPP string version of f77par
int	plen			# length of string f77par
int	errcode()

begin
	istat = OK

%      plen = len (f77par)
	call smark (sp)
	call salloc (param, plen+1, TY_CHAR)
	call f77upk (f77par, Memc[param], plen)

	iferr (call tbhptb (tp, Memc[param], value)) {
	    istat = errcode()
	}
	call sfree (sp)
end
# uthptd -- put header parameter double
# Write a parameter of type double precision to the table header.
#
# P.E. Hodge, 16-Sep-87  Delete section to write error message.

procedure uthptd (tp, f77par, value, istat)

pointer tp			# i: Pointer to table descriptor
%      character*(*) f77par
double	value			# i: Value of parameter
int	istat			# o: Return status
#--
pointer sp			# stack pointer
pointer param			# scratch for SPP string version of f77par
int	plen			# length of string f77par
int	errcode()

begin
	istat = OK

%      plen = len (f77par)
	call smark (sp)
	call salloc (param, plen+1, TY_CHAR)
	call f77upk (f77par, Memc[param], plen)

	iferr (call tbhptd (tp, Memc[param], value)) {
	    istat = errcode()
	}
	call sfree (sp)
end
# uthpti -- put header parameter integer
# Write a parameter of type integer to the table header.
#
# P.E. Hodge, 16-Sep-87  Delete section to write error message.

procedure uthpti (tp, f77par, value, istat)

pointer tp			# i: Pointer to table descriptor
%      character*(*) f77par
int	value			# i: Value of parameter
int	istat			# o: Return status
#--
pointer sp			# stack pointer
pointer param			# scratch for SPP string version of f77par
int	plen			# length of string f77par
int	errcode()

begin
	istat = OK

%      plen = len (f77par)
	call smark (sp)
	call salloc (param, plen+1, TY_CHAR)
	call f77upk (f77par, Memc[param], plen)

	iferr (call tbhpti (tp, Memc[param], value)) {
	    istat = errcode()
	}
	call sfree (sp)
end
# uthptr -- put header parameter real
# Write a parameter of type real to the table header.
#
# P.E. Hodge, 16-Sep-87  Delete section to write error message.

procedure uthptr (tp, f77par, value, istat)

pointer tp			# i: Pointer to table descriptor
%      character*(*) f77par
real	value			# i: Value of parameter
int	istat			# o: Return status
#--
pointer sp			# stack pointer
pointer param			# scratch for SPP string version of f77par
int	plen			# length of string f77par
int	errcode()

begin
	istat = OK

%      plen = len (f77par)
	call smark (sp)
	call salloc (param, plen+1, TY_CHAR)
	call f77upk (f77par, Memc[param], plen)

	iferr (call tbhptr (tp, Memc[param], value)) {
	    istat = errcode()
	}
	call sfree (sp)
end
# uthptt -- put header parameter text
# Write a character string parameter to the table header.
#
# P.E. Hodge, 16-Sep-87  Delete section to write error message.

procedure uthptt (tp, f77par, f77txt, istat)

pointer tp			# i: Pointer to table descriptor
%      character*(*) f77par
%      character*(*) f77txt
int	istat			# o: Return status
#--
pointer sp			# stack pointer
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
	call f77upk (f77txt, Memc[text], txtlen)

	iferr (call tbhptt (tp, Memc[param], Memc[text])) {
	    istat = errcode()
	}
	call sfree (sp)
end
