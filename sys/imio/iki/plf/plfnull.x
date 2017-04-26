# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# PLF_NULL -- Null driver entry point.

procedure plf_null()

begin
	call error (1, "PLF image kernel abort - null driver entry point")
end
