
c               Copyright 1993 Colorado State University
c                       All Rights Reserved


      subroutine droot(pltlig)

      real      pltlig(2)

c...Simulate death of roots for the month.
c
      include 'const.inc'
      include 'param.inc'
      include 'parcp.inc'
      include 'parfx.inc'
      include 'plot1.inc'

c...Local variables
      integer   iel
      real      fr14, recres(MAXIEL), rdeath, rtdh


c...Death of roots

c...Mod. added (stemp .lt. 2.0) conditional since roots don't really
c     die in winter because it's too cold for physiological activity. 
c...Also added rtdh term for drought conditions.
c     -rm  9-12-90
      if ((bglivc .le. 0.) .or. (stemp .lt. rtdtmp)) then
        return
      endif

c...This is representing the death of fine surface roots.  They depend
c     on moisture in the top soil layers, so use avh2o(1).
      rtdh = 1.0 - avh2o(1)/(deck5+avh2o(1))
      rdeath = rdr * rtdh
      if (rdeath .gt. 0.95) then
        rdeath = 0.95
      endif
      rdeath = rdeath * bglivc
      do 10 iel = 1, nelem
        recres(iel) = bglive(iel)/bglivc
10    continue
      fr14 = bglcis(LABELD)/bglivc
      call partit(rdeath,recres,2,bglcis,bglive,pltlig(BELOW),fr14)

      return
      end
