# DP_RECTIFY -- Shuffle a real vector based upon an input index vector using
# dynamic memory.

procedure dp_rectify (x, index, nitem)

real	x[ARB]
int	index[ARB]
int	nitem

pointer	sp, hold

begin
	call smark (sp)
	call salloc (hold, nitem, TY_REAL)
	call dp_dorect (x, Memr[hold], index, nitem)
	call sfree (sp)
end


# DP_IRECTIFY -- Shuffle an integer vector based upon an input index vector
# using dynamic memory.

procedure dp_irectify (x, index, nitem)

int	x[ARB]
int	index[ARB]
int	nitem

pointer	sp, hold

begin
	call smark (sp)
	call salloc (hold, nitem, TY_INT)
	call dp_idorect (x, Memi[hold], index, nitem)
	call sfree (sp)
end


# DP_DORECT -- Shuffle a vector based upon an input index vector using
# a preallocated dummy array.

procedure dp_dorect (x, hold, index, nitem)

real	x[ARB]
real	hold[ARB]
int	index[ARB]
int	nitem

int	i

begin
	call amovr (x, hold, nitem)
	do i = 1, nitem
	    x[i] = hold[index[i]]
end


# DP_IDORECT -- Shuffle an integer  vector based upon an input index vector
# using a preallocated dummy array.

procedure dp_idorect (x, hold, index, nitem)

int	x[ARB]
int	hold[ARB]
int	index[ARB]
int	nitem

int	i

begin
	call amovi (x, hold, nitem)
	do i = 1, nitem
	    x[i] = hold[index[i]]
end
