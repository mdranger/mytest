
c               Copyright 1993 Colorado State University
c                       All Rights Reserved

c...LIVREM
      subroutine livrem(accum)

c...Removal of live biomass due to cutting or fire in a forest.

c...Called from:	frem

      include 'const.inc'
      include 'forrem.inc'
      include 'param.inc'
      include 'plot2.inc'
      include 'plot3.inc'
      include 'zztim.inc'

c...Argument Declarations
      real      accum(ISOS)

c...Local variables
      integer	iel
      real	closs, eloss(MAXIEL)

c...Remove live LEAVES

      if (remf(1) > 0.95) then 
          prdx(3) = 0.01
      endif

      closs = remf(1) * rleavc
      tcrem = tcrem + closs
      
c       print *
c      print *, "closs", closs, rleavc,tcrem
      
           call csched(closs,rlvcis(LABELD),rleavc,
     $             rlvcis(UNLABL),csrsnk(UNLABL),
     $             rlvcis(LABELD),csrsnk(LABELD),
     $             1.0,accum)

      do 10 iel = 1, nelem
        eloss(iel) = closs * (rleave(iel) / (0.0001+rleavc))
	terem(iel) = terem(iel) + eloss(iel)
	call flow(rleave(iel),esrsnk(iel),time,eloss(iel))
 10   continue


c...Remove live FINE BRANCHES

      closs = remf(2) * fbrchc
      tcrem = tcrem + closs
      call csched(closs,fbrcis(LABELD),fbrchc,
     $               fbrcis(UNLABL),csrsnk(UNLABL),
     $               fbrcis(LABELD),csrsnk(LABELD),
     $               1.0,accum)

      do 20 iel = 1, nelem
	 eloss(iel) = closs * (fbrche(iel) / (0.0001+fbrchc))
	 terem(iel) = terem(iel) + eloss(iel)
	 call flow(fbrche(iel),esrsnk(iel),time,eloss(iel))
 20   continue

c...Remove live LARGE WOOD

      closs = remf(3) * rlwodc
      tcrem = tcrem + closs
      call csched(closs,rlwcis(LABELD),rlwodc,
     $               rlwcis(UNLABL),csrsnk(UNLABL),
     $               rlwcis(LABELD),csrsnk(LABELD),
     $               1.0,accum)

      do 30 iel = 1, nelem
	 eloss(iel) = closs * (rlwode(iel) / (0.0001+rlwodc))
	 terem(iel) = terem(iel) + eloss(iel)
	 call flow(rlwode(iel),esrsnk(iel),time,eloss(iel))
 30   continue

c...Remove from STORAGE pool based on fraction of large wood
c   removed.

      do 40 iel = 1, nelem
	eloss(iel) = MAX(remf(3) * forstg(iel), 0.0)
	call flow(forstg(iel),esrsnk(iel),time,eloss(iel))
 40   continue

      return
      end
