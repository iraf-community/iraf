task fpe 	= t_fpe,
     segvio 	= t_segvio,
     spperr	= t_spperr

procedure t_fpe ()
real	x, y, z
begin
	x = 1.0
	y = 0.0
	z = x / y
end


procedure t_segvio ()
pointer	ip
begin
	ip = 0
	Memc[ip] = 'x'
end


procedure t_spperr ()
begin
	call error (123, "test spp error()")
end
