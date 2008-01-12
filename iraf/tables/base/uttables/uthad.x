include <tbset.h>

# uthadb -- add header parameter logical
# Write a parameter of type logical to the table header.  If the parameter already
# exists the value will be replaced; otherwise, a new parameter will be written.
#
# P.E. Hodge, 7-Aug-87  Subroutine created
# P.E. Hodge, 16-Sep-87  Delete section to write error message.
# P.E. Hodge, 28-Dec-87  Different data types combined into one file.

procedure uthadb (tp, f77par, value, istat)

pointer tp			# i: Pointer to table descriptor
				# i: Name of parameter to be added
%      character*(*) f77par
bool	value			# i: Value of parameter
int	istat			# o: Return status
#--
pointer sp			# stack pointer
pointer param			# scratch for SPP string version of f77par
int	errcode()

begin
	istat = OK

	call smark (sp)
	call salloc (param, SZ_KEYWORD, TY_CHAR)
	call f77upk (f77par, Memc[param], SZ_KEYWORD)

	iferr (call tbhadb (tp, Memc[param], value)) {
	    istat = errcode()
	}
	call sfree (sp)
end


# uthadd -- add header parameter double
# Write a parameter of type double precision to the table header.  If the
# parameter already exists the value will be replaced; otherwise, a new
# parameter will be written.
#
# P.E. Hodge, 7-Aug-87  Subroutine created
# P.E. Hodge, 16-Sep-87  Delete section to write error message.

procedure uthadd (tp, f77par, value, istat)

pointer tp			# i: Pointer to table descriptor
				# i: Name of parameter to be added
%      character*(*) f77par
double	value			# i: Value of parameter
int	istat			# o: Return status
#--
pointer sp			# stack pointer
pointer param			# scratch for SPP string version of f77par
int	errcode()

begin
	istat = OK

	call smark (sp)
	call salloc (param, SZ_KEYWORD, TY_CHAR)
	call f77upk (f77par, Memc[param], SZ_KEYWORD)

	iferr (call tbhadd (tp, Memc[param], value)) {
	    istat = errcode()
	}
	call sfree (sp)
end


# uthadi -- add header parameter integer
# Write a parameter of type integer to the table header.  If the parameter
# already exists the value will be replaced; otherwise, a new parameter
# will be written.
#
# P.E. Hodge, 7-Aug-87  Subroutine created
# P.E. Hodge, 16-Sep-87  Delete section to write error message.

procedure uthadi (tp, f77par, value, istat)

pointer tp			# i: Pointer to table descriptor
				# i: Name of parameter to be added
%      character*(*) f77par
int	value			# i: Value of parameter
int	istat			# o: Return status
#--
pointer sp			# stack pointer
pointer param			# scratch for SPP string version of f77par
int	errcode()

begin
	istat = OK

	call smark (sp)
	call salloc (param, SZ_KEYWORD, TY_CHAR)
	call f77upk (f77par, Memc[param], SZ_KEYWORD)

	iferr (call tbhadi (tp, Memc[param], value)) {
	    istat = errcode()
	}
	call sfree (sp)
end


# uthadr -- add header parameter real
# Write a parameter of type real to the table header.  If the parameter already
# exists the value will be replaced; otherwise, a new parameter will be written.
#
# P.E. Hodge, 7-Aug-87  Subroutine created
# P.E. Hodge, 16-Sep-87  Delete section to write error message.

procedure uthadr (tp, f77par, value, istat)

pointer tp			# i: Pointer to table descriptor
				# i: Name of parameter to be added
%      character*(*) f77par
real	value			# i: Value of parameter
int	istat			# o: Return status
#--
pointer sp			# stack pointer
pointer param			# scratch for SPP string version of f77par
int	errcode()

begin
	istat = OK

	call smark (sp)
	call salloc (param, SZ_KEYWORD, TY_CHAR)
	call f77upk (f77par, Memc[param], SZ_KEYWORD)

	iferr (call tbhadr (tp, Memc[param], value)) {
	    istat = errcode()
	}
	call sfree (sp)
end


# uthadt -- add header parameter text
# Write a character-string parameter to the table header.  If the parameter
# already exists the value will be replaced; otherwise, a new parameter
# will be written.
#
# P.E. Hodge, 7-Aug-87  Subroutine created
# P.E. Hodge, 8-Sep-87  Delete declaration of txtlen.
# P.E. Hodge, 16-Sep-87  Delete section to write error message.

procedure uthadt (tp, f77par, f77txt, istat)

pointer tp			# i: Pointer to table descriptor
				# i: Name of parameter to be added
%      character*(*) f77par
				# i: Text string which is the value to be added
%      character*(*) f77txt
int	istat			# o: Return status
#--
pointer sp			# stack pointer
pointer param			# scratch for SPP string version of f77par
pointer text			# scratch for SPP string version of f77txt
int	errcode()

begin
	istat = OK

	call smark (sp)
	call salloc (param, SZ_KEYWORD, TY_CHAR)
	call f77upk (f77par, Memc[param], SZ_KEYWORD)
	call salloc (text, SZ_PARREC, TY_CHAR)
	call f77upk (f77txt, Memc[text], SZ_PARREC)

	iferr (call tbhadt (tp, Memc[param], Memc[text])) {
	    istat = errcode()
	}
	call sfree (sp)
end
