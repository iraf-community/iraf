C----------------------------------------------------------------------
        subroutine ftphbn(ounit,nrows,nfield,ttype,tform,tunit,
     &                    extnam,pcount,status)

C       write required standard header keywords for a binary table extension 
C
C       ounit   i  fortran output unit number
C       nrows   i  number of rows in the table
C       nfield  i  number of fields in the table
C       ttype   c  name of each field (array) (optional)
C       tform   c  format of each field (array)
C       tunit   c  units of each field (array) (optional)
C       extnam  c  name of table extension (optional)
C       pcount  i  size of special data area following the table (usually = 0)
C       OUTPUT PARAMETERS:
C       status  i  output error status (0=OK)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,nrows,nfield,pcount,status
        integer i,lenrow,dtype,rcount,xbcol,length,width
        character*(*) ttype(*),tform(*),tunit(*),extnam
        character comm*48,tfm*40

        comm='binary table extension'
        call ftpkys(ounit,'XTENSION','BINTABLE',comm,status)

        comm='8-bit bytes'
        call ftpkyj(ounit,'BITPIX',8,comm,status)

        comm='2-dimensional binary table'
        call ftpkyj(ounit,'NAXIS',2,comm,status)

        if (status .gt. 0)return

C       calculate the total width of each row, in bytes
        lenrow=0
        do 10 i=1,nfield
C               get the numerical datatype and repeat count of the field
                call ftbnfm(tform(i),dtype,rcount,width,status)
                if (dtype .eq. 1)then
C                       treat Bit datatype as if it were a Byte datatype
                        dtype=11
                        rcount=(rcount+7)/8
                end if
C               get the width of the field
                call ftgtbc(1,dtype,rcount,xbcol,length,status)
                lenrow=lenrow+length
10      continue

        comm='width of table in bytes'
        call ftpkyj(ounit,'NAXIS1',lenrow,comm,status)

        if (status .gt. 0)return

        if (nrows .ge. 0)then
                comm='number of rows in table'
                call ftpkyj(ounit,'NAXIS2',nrows,comm,status)
        else
                status=218
        end if

        if (status .gt. 0)return

        if (pcount .ge. 0)then
                comm='size of special data area'
                call ftpkyj(ounit,'PCOUNT',pcount,comm,status)
        else
                status=214
        end if

        comm='one data group (required keyword)'
        call ftpkyj(ounit,'GCOUNT',1,comm,status)

        comm='number of fields in each row'
        call ftpkyj(ounit,'TFIELDS',nfield,comm,status)

        if (status .gt. 0)return

        do 20 i=1,nfield
            if (ttype(i) .ne. ' ' .and. ichar(ttype(i)(1:1)).ne.0)then
                comm='label for field '
                write(comm(17:19),1000)i
1000            format(i3)      
                call ftpkns(ounit,'TTYPE',i,1,ttype(i),comm,status)
            end if

            comm='data format of the field'
C           make sure format characters are in upper case:
            tfm=tform(i)
            call ftupch(tfm)

C           Add datatype to the comment string:
            call ftbnfm(tfm,dtype,rcount,width,status)
            if (dtype .eq. 21)then
                comm(25:)=': 2-byte INTEGER'
            else if(dtype .eq. 41)then
                comm(25:)=': 4-byte INTEGER'
            else if(dtype .eq. 42)then
                comm(25:)=': 4-byte REAL'
            else if(dtype .eq. 82)then
                comm(25:)=': 8-byte DOUBLE'
            else if(dtype .eq. 16)then
                comm(25:)=': ASCII Character'
            else if(dtype .eq. 14)then
                comm(25:)=': 1-byte LOGICAL'
            else if(dtype .eq. 11)then
                comm(25:)=': BYTE'
            else if(dtype .eq. 1)then
                comm(25:)=': BIT'
            else if(dtype .eq. 83)then
                comm(25:)=': COMPLEX'
            else if(dtype .eq. 163)then
                comm(25:)=': DOUBLE COMPLEX'
            end if

            call ftpkns(ounit,'TFORM',i,1,tfm,comm,status)

            if (tunit(i) .ne. ' ' .and. ichar(tunit(i)(1:1)).ne.0)then
                comm='physical unit of field'
                call ftpkns(ounit,'TUNIT',i,1,tunit(i),comm,status)
            end if
            if (status .gt. 0)return
20      continue        

        if (extnam .ne. ' ' .and. ichar(extnam(1:1)) .ne. 0)then
                comm='name of this binary table extension'
                call ftpkys(ounit,'EXTNAME',extnam,comm,status)
        end if
        end
