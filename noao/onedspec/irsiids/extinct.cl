#{ EXTINCT -- Use the BSWITCH task to perform the correction for
#            atmospheric extinction.

{
# Root name
rt = root

# Records
rec = records

# Output root
out = output

# Output starting record
strt = start_rec

# Do operation
# Inhibit weighting and statisitic file generation

bswitch  (input=rt, records=rec, output=out, start_rec=strt, subset=nr_aps,
         weighting=no, ids_mode=no, stats="")
}
