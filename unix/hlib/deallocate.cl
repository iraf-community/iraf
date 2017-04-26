# DEALLOCATE -- Deallocate a device.  The real work is done by the hidden CL
# _deallocate task, but we provide a script interface as well to provide
# scope for machine dependent additions.

procedure deallocate (device)

string	device { prompt = "device to be deallocated" }
bool	rewind = yes

begin
	_deallocate (device, rewind)
end
