
include defs

# MAPID -- Map a long identifier.  The new identifier is formed by
# concatenating the first MAXIDLENGTH-1 characters and the last character.


subroutine mapid (name)

character name(MAXTOK)
integer	i

	for (i=1;  name(i) != EOS;  i=i+1)
	    ;
	if (i-1 > MAXIDLENGTH) {
	    name(MAXIDLENGTH) = name(i-1)
	    name(MAXIDLENGTH+1) = EOS
	}
end
