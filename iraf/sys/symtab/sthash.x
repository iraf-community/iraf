# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

define	MAX_HASHCHARS	18

# STHASH -- Compute the hash index of a key, i.e., the index of a thread in
# the symbol table index.  Multiple keys may hash to the same thread.  The
# ideal hash function will uniformly map keys into index space, both when the
# keys are selected randomly and when the keys form patterns, e.g., when keys
# share a common prefix.  The SYMTAB package uses a simple hash function which
# is computed inline.  The STHASH function is NOT used at present, but is
# included in the library anyway for use in other packages and because this
# is a slightly better (more uniform) hashing function than the simple inline
# version used in SYMTAB.

int procedure sthash (key, modulus)

char	key[ARB]		# character string serving as a key
int	modulus			# number of possible output values

int	i
long	sum
int	primes[MAX_HASHCHARS]
data	(primes(i),i=1,9)	/101,103,107,109,113,127,131,137,139/
data	(primes(i),i=10,18)	/149,151,157,163,167,173,179,181,191/

begin
	sum = 0

	# Hash up to len(primes)=18 characters from the key.
	do i = 1, MAX_HASHCHARS {
	    if (key[i] == EOS)
		break
	    sum = sum + (key[i] * primes[i])
	}

	return (mod (sum, modulus) + 1)
end
