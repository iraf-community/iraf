procedure tbcfmt (tp, cdef, str)

pointer	tp
pointer	cdef
char	str[ARB]

begin
end

procedure tbcnam (tp, cdef, str)

pointer	tp
pointer	cdef
char	str[ARB]

begin
end

procedure tbcnit (tp, cdef, str)

pointer	tp
pointer	cdef
char	str[ARB]

begin
end

procedure tbegtd (tp, cdef, row, val)

pointer	tp
pointer	cdef
int	row
double	val

begin
end

procedure tbegtr (tp, cdef, row, val)

pointer	tp
pointer	cdef
int	row
real	val

begin
end

procedure tbegti (tp, cdef, row, val)

pointer	tp
pointer	cdef
int	row
int	val

begin
end

procedure tbegtt (tp, cdef, row, val, len)

pointer	tp
pointer	cdef
int	row
char	val[ARB]
int	len

begin
end

procedure tbeptd (tp, cdef, row, val)

pointer	tp
pointer	cdef
int	row
double	val

begin
end

procedure tbeptr (tp, cdef, row, val)

pointer	tp
pointer	cdef
int	row
real	val

begin
end

procedure tbepti (tp, cdef, row, val)

pointer	tp
pointer	cdef
int	row
int	val

begin
end

procedure tbeptt (tp, cdef, row, val)

pointer	tp
pointer	cdef
int	row
char	val[ARB]

begin
end

procedure tbhgtr (tp, key, val)

pointer	tp
char	key[ARB]
char	val[ARB]

begin
end

procedure tbhgtt (tp, key, val, maxchar)

pointer	tp
char	key[ARB]
char	val[ARB]
int	maxchar

begin
end

procedure tbhadr (tp, key, val)

pointer	tp
char	key[ARB]
char	val[ARB]

begin
end

procedure tbhadt (tp, key, val)

pointer	tp
char	key[ARB]
char	val[ARB]

begin
end

procedure tbpsta (tp, par)

pointer	tp
int	par

begin
end

procedure tbtclo (tp)

pointer	tp

begin
end

procedure tbtcre (tp)

pointer	tp

begin
end

pointer procedure tbtopn (fname, mode, arg)

char	fname[ARB]
int	mode
pointer	arg

begin
end

procedure tbcdef1 (tp, cdef, label, units, format, type, n)

pointer	tp
pointer	cdef
char	label[ARB]
char	units[ARB]
char	format[ARB]
int	type
int	n

begin
end

procedure tbcfnd1 (tp, label, cdef)

pointer	tp
char	label[ARB]
pointer	cdef

begin
end
