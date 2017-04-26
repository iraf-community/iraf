procedure nttools()
string mode="al"

begin
	package nttools
	task gtedit,
	     imtab,
	     keypar,
	     keytab,
	     parkey,
	     partab,
	     tabim,
	     tabkey,
	     tabpar,
	     taextract,
	     tainsert,
	     tcalc,
	     tchcol,
	     tcheck,
	     tchsize,
	     tcopy,
	     tcreate,
	     tdelete,
	     tdiffer,
	     tdump,
	     tedit,
	     texpand,
	     thedit,
	     thistogram,
	     thselect,
	     tinfo,
	     tintegrate,
	     tjoin,
	     tlcol,
             tlinear,
	     tmatch,
	     tmerge,
	     tprint,
	     tproduct,
	     tproject,
	     tquery,
	     tread,
	     trebin,
	     tselect,
	     tsort,
	     tstat,
	     ttranspose,
	     tunits,
	     tupar = "nttools$x_nttools.e"

	task gtpar = "nttools$gtpar.par"

	task txtable,
             tximage,
             titable,
             tiimage,
             tscopy = "nttools$threed/x_nttools.e"

	clbye()
end
