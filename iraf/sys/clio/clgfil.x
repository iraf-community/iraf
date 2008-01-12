# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<fset.h>

.help clpopn[isu], clplen, clgfil, clpcls
.nf ___________________________________________________________________________
Expand a filename template given as the string value of a CL parameter.

	clpopni - open a sorted input list or open list "STDIN"
	clpopns - open a sorted list
	clpopnu - open an unsorted list
	 clpcls - close a list
	 clplen - get number of filenames in list
	 clgfil - get next filename from list
	 clprew - rewind the list

The CLPOPNI procedure creates a dummy list containing the single filename
"STDIN" if the standard input is redirected.
.endhelp ______________________________________________________________________


# CLPOPNI -- Open an input list (sorted list of input files).  If the standard
# input has been redirected, create a dummy list containing the single file
# name "STDIN", and do not try to access the template parameter.

int procedure clpopni (param)

char	param[ARB]			# CL filename template parameter
int	sort
pointer	sp, template, list
int	fntopnb(), fstati()

begin
	call smark (sp)
	call salloc (template, SZ_COMMAND, TY_CHAR)

	sort = YES

	if (fstati (STDIN, F_REDIR) == YES)
	    list = fntopnb ("STDIN", sort)
	else {
	    call clgstr (param, Memc[template], SZ_COMMAND)
	    list = fntopnb (Memc[template], sort)
	}

	call sfree (sp)
	return (list)
end


# CLPOPNS -- Open a sorted list (sorted list of files, not associated with any
# particular byte stream).

int procedure clpopns (param)

char	param[ARB]			# CL filename template parameter
int	sort
pointer	sp, template, list
int	fntopnb()

begin
	call smark (sp)
	call salloc (template, SZ_COMMAND, TY_CHAR)

	sort = YES

	call clgstr (param, Memc[template], SZ_COMMAND)
	list = fntopnb (Memc[template], sort)

	call sfree (sp)
	return (list)
end


# CLPOPNU -- Open an unsorted list (unsorted list of files, not associated
# with any particular stream).

int procedure clpopnu (param)

char	param[ARB]			# CL filename template parameter
int	sort
pointer	sp, template, list
int	fntopnb()

begin
	call smark (sp)
	call salloc (template, SZ_COMMAND, TY_CHAR)

	sort = NO

	call clgstr (param, Memc[template], SZ_COMMAND)
	list = fntopnb (Memc[template], sort)

	call sfree (sp)
	return (list)
end


# CLPLEN -- Return the number of file names in the list.

int procedure clplen (list)

pointer	list
int	fntlenb()

begin
	return (fntlenb (list))
end


# CLGFIL -- Return the next filename from the list.

int procedure clgfil (list, fname, maxch)

int	list			# list descriptor
char	fname[ARB]		# output string
int	maxch
int	fntgfnb()

begin
	return (fntgfnb (list, fname, maxch))
end


# CLPCLS -- Close a filename list and return all storage.

procedure clpcls (list)

int	list			# list descriptor

begin
	call fntclsb (list)
end


# GLPREW -- Rewind the filename list.

procedure clprew (list)

int	list			# list descriptor

begin
	call fntrewb (list)
end
