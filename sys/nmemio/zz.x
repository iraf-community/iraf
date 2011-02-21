task hello = t_hello

procedure t_hello()
pointer	t1, t2
begin
	call malloc (t1, SZ_LINE, TY_CHAR)
	call mfree (t1, TY_CHAR)

	call malloc (t2, SZ_LINE, TY_INT)
	call mfree (t2, TY_INT)
end
