      subroutine option (icas,int,ital,ior)
      call optn (4hcase,icas)
      if (int .eq. 0) call optn (4hintn,3hlow)
      if (int .eq. 1) call optn (4hintn,4hhigh)
      call optn (4hfont,ital)
      call optn (4horen,ior)
      return
      end
