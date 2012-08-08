define	SKIP	($1==' '||$1=='\t'||$1=='"'||$1=='\'')

# ID_LABEL -- Set label

procedure id_label (str, label)

char	str[ARB]		# String to be set
pointer	label			# Label pointer to be set

int	i, j, strlen()
pointer	cp

begin
	call mfree (label, TY_CHAR)

	for (i=1; str[i]!=EOS && SKIP(str[i]); i=i+1)
	    ;
	for (j=strlen(str); j>=i && SKIP(str[j]); j=j-1) 
	    ;

	if (i <= j) {
	    call malloc (label, j-i+1, TY_CHAR)
	    cp = label
	    for (; i<=j; i=i+1) {
		Memc[cp] = str[i]
		cp = cp + 1
	    }
	    Memc[cp] = EOS
	}
end
