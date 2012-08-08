# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"symtab.h"

# STINFO -- Print interesting information on the inner workings and contents
# of the symbol table.

procedure stinfo (stp, fd, verbose)

pointer	stp			# symtab descriptor
int	fd			# output file
int	verbose			# if YES, trace each thread

pointer	index, stab, sbuf, ep
int	keylen, min_keylen, max_keylen, nsymbols, el, i, head
int	nthreads, max_threadlen, nonnull_threads, nsym
real	sum, mean_hash_index, avg_keylen, avg_threadlen
int	strlen()

begin
	index = ST_INDEX(stp)
	stab  = ST_STABP(stp)
	sbuf  = ST_SBUFP(stp)

	# Print the symbol table name.
	ep = sbuf + ST_NAME(stp)
	call fprintf (fd, "name: %s\n")
	    if (Memc[ep] == EOS)
		call pargstr ("(none given)")
	    else
		call pargstr (Memc[ep])

	# Print information on memory usage.
	call fprintf (fd,
	    "index=(%x,%d), stab=(%x,%d,%d%%), sbuf=(%x,%d,%d%%)\n")
	    call pargi (index)
	    call pargi (ST_INDEXLEN(stp))
	    call pargi (stab)
	    call pargi (ST_STABLEN(stp))
	    call pargr (ST_STABOP(stp) * 100.0 / ST_STABLEN(stp))
	    call pargi (sbuf)
	    call pargi (ST_SBUFLEN(stp))
	    call pargr (ST_SBUFOP(stp) * 100.0 / ST_SBUFLEN(stp))

	call fprintf (fd,
	    "sbuf reallocated %d times, stab reallocated %d times\n")
	    call pargi (ST_SBUFNGROW(stp))
	    call pargi (ST_STABNGROW(stp))

	# Scan the symbols and compute the min, max, and mean key lengths.
	# Count the number of symbols.

	min_keylen = MAX_SZKEY
	max_keylen = 0
	avg_keylen = 0
	nsymbols   = 0
	sum	   = 0

	for (el = ST_LASTSYMBOL(stp);  el != NULL;  el = E_NEXTGLOB(ep)) {
	    nsymbols = nsymbols + 1
	    ep = stab + el

	    keylen = strlen (Memc[sbuf+E_KEY(ep)])
	    min_keylen = min (min_keylen, keylen)
	    max_keylen = max (max_keylen, keylen)
	    sum = sum + keylen
	}

	if (nsymbols > 0)
	    avg_keylen = sum / nsymbols
	else
	    min_keylen = 0

	call fprintf (fd,
	    "nsymbols=%d, minkeylen=%d, maxkeylen=%d, avgkeylen=%.1f\n")
	    call pargi (nsymbols)
	    call pargi (min_keylen)
	    call pargi (max_keylen)
	    call pargr (avg_keylen)

	# Scan the index and compute the number of nonnull threads, the
	# mean and max thread lengths, and the mean hash index, which should
	# be near the center of the index.

	nthreads = ST_INDEXLEN(stp)
	mean_hash_index = 0
	nonnull_threads = 0
	max_threadlen = 0
	avg_threadlen = 0
	sum = 0

	if (verbose == YES)
	    call fprintf (fd, "----------- threads ----------\n")

	do i = 1, nthreads {
	    if (verbose == YES) {
		call fprintf (fd, "[%4d] ")
		    call pargi (i)
	    }

	    head = Memi[index+i-1]
	    if (head != NULL) {
		nonnull_threads = nonnull_threads + 1

		# Count the number of symbols on the thread.
		nsym = 0
		for (el=head;  el != NULL;  el=E_NEXTHASH(ep)) {
		    nsym = nsym + 1
		    ep = stab + el

		    if (verbose == YES) {
			call fprintf (fd, "%s ")
			    call pargstr (Memc[sbuf+E_KEY(ep)])
		    }
		}


		max_threadlen = max (max_threadlen, nsym)
		sum = sum + (nsym * i)
	    }

	    if (verbose == YES)
		call fprintf (fd, "\n")
	}

	if (nonnull_threads > 0) {
	    avg_threadlen = real(nsymbols) / nonnull_threads
	    mean_hash_index = sum / nsymbols
	}

	if (verbose == YES)
	    call fprintf (fd, "---------------------\n")

	call fprintf (fd,
	    "nthreads=%d, maxlen=%d, avglen=%.1f, meanindex=%.1f\n")
	    call pargi (nonnull_threads)
	    call pargi (max_threadlen)
	    call pargr (avg_threadlen)
	    call pargr (mean_hash_index)
	
	call flush (fd)
end
