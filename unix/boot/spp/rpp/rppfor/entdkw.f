      subroutine entdkw
      integer deft(2), prag(2)
      integer defnam(7)
      integer prgnam(7)
      data defnam(1)/100/,defnam(2)/101/,defnam(3)/102/,defnam(4)/105/,d
     *efnam(5)/110/,defnam(6)/101/,defnam(7)/-2/
      data prgnam(1)/112/,prgnam(2)/114/,prgnam(3)/97/,prgnam(4)/103/,pr
     *gnam(5)/109/,prgnam(6)/97/,prgnam(7)/-2/
      data deft (1), deft (2) /-4, -2/
      data prag (1), prag (2) /-17, -2/
      call ulstal (defnam, deft)
      call ulstal (prgnam, prag)
      return
      end
