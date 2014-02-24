
c              Copyright 1993 Colorado State University
c                       All Rights Reserved


      subroutine woodec (dtm)

      real      dtm

c...Wood decomposition 			written by vek 04/91

c     defac = decomposition factor based on water and temperature
c             (computed in prelim and in cycle)
c     pligst = fixed parameter that represents the effect of
c              of lignin-to-structural-ratio on structural
c              decomposition
c     add decp_factor as a temprate factor for each wood pool

      include 'comput.inc'
      include 'const.inc'
      include 'param.inc'
      include 'parfs.inc'
      include 'parfx.inc'
      include 'plot1.inc'
      include 'plot2.inc'
      include 'plot3.inc'

c...Local variables
      real      tcflow
      real      decp_factor



c...FINE BRANCHES
c     wood1c = C in dead fine branch component of forest system (g/m2)
c     decw1 = intrinsic rate of decomposition of dead fine branches
c     wdlig(FBRCH) = lignin fraction for fine branches

      if (wood1c .gt. 1.e-07) then

c...Compute total C flow out of fine branches
          decp_factor = defac * decw1 * exp(-pligst(SRFC) * 
     $             wdlig(FBRCH)) * dtm
     
          tcflow = wood1c * decp_factor
          decp_wood1c = decp_wood1c + decp_factor
c...Decompose fine branches into som1 and som2 with CO2 loss.


          call declig(aminrl,wdlig(FBRCH),SRFC,nelem,1,ps1co2,rneww1,
     $        rsplig,tcflow,wood1c,csrsnk,wd1cis,wood1e,
     $        gromin,minerl,w1mnr,resp,som1ci,som1e,som2ci,som2e)
      endif


c...LARGE WOOD
c     wood2c = C in dead large wood component of forest system (g/m2)
c     decw2 = intrinsic rate of decomposition of dead large wood
c     wdlig(LWOOD) = lignin fraction for large wood

      if (wood2c .gt. 1.e-07) then

c...Compute total C flow out of large wood
          decp_factor = defac * decw2 * exp(-pligst(SRFC) * 
     $             wdlig(LWOOD)) * dtm
          tcflow = wood2c * decp_factor
        decp_wood2c = decp_wood2c + decp_factor
c...Decompose large wood into som1 and som2 with CO2 loss.


          call declig(aminrl,wdlig(LWOOD),SRFC,nelem,1,ps1co2,rneww2,
     $        rsplig,tcflow,wood2c,csrsnk,wd2cis,wood2e,
     $        gromin,minerl,w2mnr,resp,som1ci,som1e,som2ci,som2e)
      endif


c...COARSE ROOTS
c     wood3c = C in dead coarse root component of forest system (g/m2)
c     decw3 = intrinsic rate of decomposition of dead coarse roots
c     wdlig(CROOT) = lignin fraction for coarse roots

      if (wood3c .gt. 1.e-07) then

c...Compute total C flow out of coarse roots.
          decp_factor = defac * decw3 * exp(-pligst(SOIL) *
     $             wdlig(CROOT)) *  anerb * dtm  
          tcflow = wood3c * decp_factor
          decp_wood3c = decp_wood3c + decp_factor

c...Decompose coarse roots into som1 and som2 with CO2 loss.

          call declig(aminrl,wdlig(CROOT),SOIL,nelem,1,ps1co2,rneww3,
     $        rsplig,tcflow,wood3c,csrsnk,wd3cis,wood3e,
     $        gromin,minerl,w3mnr,resp,som1ci,som1e,som2ci,som2e)

      endif
      decp_times = decp_times + 1
      return
      end
