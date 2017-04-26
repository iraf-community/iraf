# AP_DELETE -- Delete the specified aperture and return a new current aperture.

procedure ap_delete (current, aps, naps)

int	current			# Return current aperture index
pointer	aps[ARB]		# Aperture data
int	naps			# Number of apertures

int	i

begin
	if (current < 1)
	    return

	call ap_free (aps[current])
	for (i = current; i < naps; i = i + 1)
	    aps[i] = aps[i+1]

	aps[naps] = NULL

	naps = naps - 1
	current = min (naps, current)
end
