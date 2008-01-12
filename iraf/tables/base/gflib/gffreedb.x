#* HISTORY *
#* B.Simon	09-Nov-99	Original code

# GF_FREE_DB -- Free a database of extension keyword names

procedure gf_free_db (im)

pointer	im		# i: iamge descriptor
#--
pointer	db

begin
	call gf_remove_db (im, db)
	call gf_freehash (db)
end
