procedure ttools()
string mode="al"

begin
	set  threed = "ttools$threed/"

	package ttools
	task gtedit,
	     imtab,
	     keypar,
	     keyselect,
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
	     tupar = "ttools$x_ttools.e"

	task gtpar = "ttools$gtpar.par"

	task txtable,
             tximage,
             titable,
             tiimage,
             tscopy = "threed$x_threed.e"

	clbye()
end
