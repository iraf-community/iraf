C
C  F77POS -- Fortran example task to extract position columns from a votable.
C
C  M.Fitzpatrick, NOAO, Aug 2009

        program f77pos

        character  ra*16, dec*16, ucd*30
        integer    vot, res, tab, data, tdata, field
        integer    i, ncols, nrows
        integer    ra_col, dec_col


C       Declare the libvotable functions we'll be using.
C
        integer  vf_openvotable, vf_getresource, vf_gettable
        integer  vf_getdata, vf_gettabledata, vf_getnext
        integer  vf_getfield, vf_getnrows, vf_getncols


C       Parse the table.
C
        vot   = vf_openvotable ("sample.xml")
        res   = vf_getresource (vot)
        tab   = vf_gettable (res)
        data  = vf_getdata (tab)
        tdata = vf_gettabledata (data)

C       Get some useful values.
C
        nrows = vf_getnrows (tdata)
        ncols = vf_getncols (tdata)
            print *, nrows, "  ", ncols

C       Get the RA and DEC columns by matching the UCD.
C
        field = vf_getfield (tab)
        do 90 i = 1, ncols
C           Get the UCD for this FIELD
            call vf_getattr (field, "ucd", ucd, 30)
            if (ucd .eq. "POS_EQ_RA_MAIN")  ra_col = i
            if (ucd .eq. "POS_EQ_DEC_MAIN")  dec_col = i

C           Get the next FIELD (i.e. column) in the table.
            field = vf_getnext (field)
90      continue


C       Loop through the data table and print the selected columns.
C
        do 91 i = 1, nrows
C           Get the value of the cell
            call vf_gettablecell (tdata, i, ra_col,  ra,  16)
            call vf_gettablecell (tdata, i, dec_col, dec, 16)

            print *, ra, "  ", dec
91      continue

        stop
        end

