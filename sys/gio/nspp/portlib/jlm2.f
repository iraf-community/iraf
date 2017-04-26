      function jlm2 (ichar)
      dimension       ichar(1)
      call ncgchr (ichar,2,1,ichar1)
      call ncgchr (ichar,2,2,ichar2)
      jlm2 = ior(ishift(ichar1,8),ichar2)
      return
      end
