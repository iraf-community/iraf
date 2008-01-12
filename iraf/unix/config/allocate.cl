# ALLOCATE -- Allocate a device.  The real work is done by the hidden CL
# _allocate task, but we provide a script interface as well to provide
# scope for machine dependent additions.

procedure allocate (device)

string	device { prompt = "device to be allocated" }

begin
	_allocate (device)
end
