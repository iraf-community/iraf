C----------------------------------------------------------------------
        subroutine ftphtb(ounit,ncols,nrows,nfield,ttype,tbcol,
     &  tform,tunit,extnam,status)

C       write required standard header keywords for an ASCII table extension 
C
C       ounit   i  fortran output unit number
C       ncols   i  number of columns in the table
C       nrows   i  number of rows in the table
C       nfield  i  number of fields in the table
C       ttype   c  name of each field (array) (optional)
C       tbcol   i  beginning column of each field (array)
C       tform   c  Fortran-77 format of each field (array)
C       tunit   c  units of each field (array) (optional)
C       extnam  c  name of table extension (optional)
C       OUTPUT PARAMETERS:
C       status  i  output error status (0=OK)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,ncols,nrows,nfield,tbcol(*),status,i
        character*(*) ttype(*),tform(*),tunit(*),extnam
        character comm*48,tfm*20

        comm='ASCII table extension'
        call ftpkys(ounit,'XTENSION','TABLE',comm,status)

        comm='8-bit ASCII characters'
        call ftpkyj(ounit,'BITPIX',8,comm,status)

        comm='2-dimensional ASCII table'
        call ftpkyj(ounit,'NAXIS',2,comm,status)

        if (status .gt. 0)return

        if (ncols .ge. 0)then
                comm='width of table in characters'
                call ftpkyj(ounit,'NAXIS1',ncols,comm,status)
        else
C               illegal table width
                status=217
        call ftpmsg('ASCII table has negative width (NAXIS1) in'//
     &  ' call to FTPHTB')
                return
        end if

        if (status .gt. 0)return

        if (nrows .ge. 0)then
                comm='number of rows in table'
                call ftpkyj(ounit,'NAXIS2',nrows,comm,status)
        else
C               illegal number of rows in table
                status=218
        call ftpmsg('ASCII table has negative number of rows in'//
     &  ' call to FTPHTB')
        end if

        if (status .gt. 0)return

        comm='no group parameters (required keyword)'
        call ftpkyj(ounit,'PCOUNT',0,comm,status)

        comm='one data group (required)'
        call ftpkyj(ounit,'GCOUNT',1,comm,status)

        if (status .gt. 0)return

        if (nfield .ge. 0)then
                comm='number of fields in each row'
                call ftpkyj(ounit,'TFIELDS',nfield,comm,status)
        else
C               illegal number of fields
                status=216
        call ftpmsg('ASCII table has negative number of fields in'//
     &  ' call to FTPHTB')
        end if

        if (status .gt. 0)return

        do 10 i=1,nfield
            if (ttype(i) .ne. ' ' .and. ichar(ttype(i)(1:1)).ne.0)then
                comm='label for field '
                write(comm(17:19),1000)i
1000            format(i3)      
                call ftpkns(ounit,'TTYPE',i,1,ttype(i),comm,status)
            end if

            comm='beginning column of field '
            write(comm(27:29),1000)i
            call ftpknj(ounit,'TBCOL',i,1,tbcol(i),comm,status)

            comm='Fortran-77 format of field'
C           make sure format characters are in upper case:
            tfm=tform(i)
            call ftupch(tfm)
            call ftpkns(ounit,'TFORM',i,1,tfm,comm,status)

            if (tunit(i) .ne. ' ' .and. ichar(tunit(i)(1:1)).ne.0)then
                comm='physical unit of field'
                call ftpkns(ounit,'TUNIT',i,1,tunit(i),comm,status)
            end if
        if (status .gt. 0)return
10      continue        

        if (extnam .ne. ' ' .and. ichar(extnam(1:1)) .ne. 0)then
                comm='name of this ASCII table extension'
                call ftpkys(ounit,'EXTNAME',extnam,comm,status)
        end if
        end
