
c               Copyright 1993 Colorado State University
c                       All Rights Reserved

c...KILLRT
      subroutine killrt(accum)

c...Death of roots due to cutting or fire in a forest.

c...Called from:	frem

      include 'const.inc'
      include 'forrem.inc'
      include 'param.inc'
      include 'parfs.inc'
      include 'plot2.inc'
      include 'plot3.inc'
      include 'zztim.inc'

c...Argument Declarations
      real	accum(ISOS)

c...Local Variables
      integer	iel
      real	crd, dethe, frc14, frd, recres(MAXIEL)

c...Death of FINE ROOTS

      frd = frootc * fd(1)
      do 10 iel = 1, nelem
        recres(iel) = froote(iel) / (0.001+frootc)
 10   continue

      frc14 = frtcis(LABELD) / (0.0001+frootc)
      call partit(frd,recres,2,frtcis,froote,wdlig(FROOT),frc14)

c...Death of COARSE ROOTS

      crd = crootc * fd(2)
      do 20 iel = 1, nelem
        dethe = crd * (croote(iel)/(0.0001+crootc))
        call flow(croote(iel),wood3e(iel),time,dethe)
 20   continue

      call csched(crd,crtcis(LABELD),crootc,
     $            crtcis(UNLABL),wd3cis(UNLABL),
     $             crtcis(LABELD),wd3cis(LABELD),
     $             1.0,accum)

      return
      end
