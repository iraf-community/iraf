#* HISTORY *
#* B.Simon	09-Nov-99	Original code

# These functions wrap the header keyword put and add functions and 
# additionally check the keyword to see if it is in the primary header.
# If so, it sets an internal flag indicating the primary header should 
# be updated

procedure gf_ipstr (im, key, value)

pointer	im		# i: image descriptor
char	key[ARB]	# i: keyword name
char	value[ARB]	# i: keyword value
#--

begin
	call gf_chk_key (im, key)
	call impstr (im, key, value)
end

procedure gf_iputb (im, key, value)

pointer	im		# i: image descriptor
char	key[ARB]	# i: keyword name
bool	value		# i: keyword value
#--

begin
	call gf_chk_key (im, key)
	call imputb (im, key, value)
end

procedure gf_iputd (im, key, value)

pointer	im		# i: image descriptor
char	key[ARB]	# i: keyword name
double	value		# i: keyword value
#--

begin
	call gf_chk_key (im, key)
	call imputd (im, key, value)
end

procedure gf_iputh (im, key, value)

pointer	im		# i: image descriptor
char	key[ARB]	# i: keyword name
char	value[ARB]	# i: keyword value
#--

begin
	call gf_chk_key (im, key)
	call imputh (im, key, value)
end

procedure gf_iputi (im, key, value)

pointer	im		# i: image descriptor
char	key[ARB]	# i: keyword name
int	value		# i: keyword value
#--

begin
	call gf_chk_key (im, key)
	call imputi (im, key, value)
end

procedure gf_iputl (im, key, value)

pointer	im		# i: image descriptor
char	key[ARB]	# i: keyword name
long	value		# i: keyword value
#--

begin
	call gf_chk_key (im, key)
	call imputl (im, key, value)
end

procedure gf_iputr (im, key, value)

pointer	im		# i: image descriptor
char	key[ARB]	# i: keyword name
real	value		# i: keyword value
#--

begin
	call gf_chk_key (im, key)
	call imputr (im, key, value)
end

procedure gf_iputs (im, key, value)

pointer	im		# i: image descriptor
char	key[ARB]	# i: keyword name
short	value		# i: keyword value
#--

begin
	call gf_chk_key (im, key)
	call imputs (im, key, value)
end

procedure gf_iaddb (im, key, value)

pointer	im		# i: image descriptor
char	key[ARB]	# i: keyword name
bool	value		# i: keyword value
#--

begin
	call gf_chk_key (im, key)
	call imaddb (im, key, value)
end

procedure gf_iaddd (im, key, value)

pointer	im		# i: image descriptor
char	key[ARB]	# i: keyword name
double	value		# i: keyword value
#--

begin
	call gf_chk_key (im, key)
	call imaddd (im, key, value)
end

procedure gf_iaddi (im, key, value)

pointer	im		# i: image descriptor
char	key[ARB]	# i: keyword name
int	value		# i: keyword value
#--

begin
	call gf_chk_key (im, key)
	call imaddi (im, key, value)
end

procedure gf_iaddl (im, key, value)

pointer	im		# i: image descriptor
char	key[ARB]	# i: keyword name
long	value		# i: keyword value
#--

begin
	call gf_chk_key (im, key)
	call imaddl (im, key, value)
end

procedure gf_iaddr (im, key, value)

pointer	im		# i: image descriptor
char	key[ARB]	# i: keyword name
real	value		# i: keyword value
#--

begin
	call gf_chk_key (im, key)
	call imaddr (im, key, value)
end

procedure gf_iadds (im, key, value)

pointer	im		# i: image descriptor
char	key[ARB]	# i: keyword name
short	value		# i: keyword value
#--

begin
	call gf_chk_key (im, key)
	call imadds (im, key, value)
end

procedure gf_iastr (im, key, value)

pointer	im		# i: image descriptor
char	key[ARB]	# i: keyword name
char	value[ARB]	# i: keyword value
#--

begin
	call gf_chk_key (im, key)
	call imastr (im, key, value)
end
