
c               Copyright 1993 Colorado State University
c                       All Rights Reserved

c...CUTRTN
      subroutine cutrtn(accum)

c...Elemental return from a cutting event.

c...Called from:	frem

      include 'const.inc'
      include 'forrem.inc'
      include 'param.inc'
      include 'parfs.inc'
      include 'plot1.inc'
      include 'plot2.inc'
      include 'plot3.inc'
      include 'zztim.inc'

c...Argument Declarations
      real      accum(ISOS)

c...Local Variables
      integer	iel
      real	cgain, egain(MAXIEL), frc14, recres(MAXIEL)

c...LEAVES are returned to LITTER

      cgain = remf(1) * retf(1,1) * rleavc
c....the following line was added on Oct 12, 2004 at EDC to account for the fact
c....that some of the removed materials never left the site
      tcrem = tcrem - cgain
      if (cgain .gt. 0.0) then
	do 10 iel = 1, nelem
	  egain(iel) = remf(1) * retf(1,iel+1) * rleave(iel)
	  recres(iel) = egain(iel) / (0.0001+ cgain)
 10     continue
        frc14 = rlvcis(LABELD) / (0.0001+rleavc)
	call partit(cgain,recres,1,csrsnk,esrsnk,wdlig(LEAF),frc14)
      endif

c...FINE BRANCHES go to DEAD FINE BRANCHES

      cgain = remf(2) * retf(2,1) * fbrchc
c....the following line was added on Oct 12, 2004 at EDC to account for the fact
c....that some of the removed materials never left the site
      tcrem = tcrem - cgain
      call csched(cgain,fbrcis(LABELD),fbrchc,
     $            csrsnk(UNLABL),wd1cis(UNLABL),
     $            csrsnk(LABELD),wd1cis(LABELD),
     $            1.0,accum)

      do 20 iel = 1, nelem
	egain(iel) = remf(2) * retf(2,iel+1) * fbrche(iel)
	call flow(esrsnk(iel),wood1e(iel),time,egain(iel))
 20   continue

c...LARGE WOOD goes to DEAD LARGE WOOD

      cgain = remf(3) * retf(3,1) * rlwodc
c....the following line was added on Oct 12, 2004 at EDC to account for the fact
c....that some of the removed materials never left the site
      tcrem = tcrem - cgain
      call csched(cgain,rlwcis(LABELD),rlwodc,
     $            csrsnk(UNLABL),wd2cis(UNLABL),
     $            csrsnk(LABELD),wd2cis(LABELD),
     $            1.0,accum)
      do 30 iel = 1, nelem
        egain(iel) = remf(3) * retf(3,iel+1) * rlwode(iel)
        call flow(esrsnk(iel),wood2e(iel),time,egain(iel))
 30   continue

c...Add STORAGE back

      do 40 iel = 1, nelem
        egain(iel) = remf(3) * retf(3,iel+1) * forstg(iel)
        call flow(esrsnk(iel),metabe(SRFC,iel),time,egain(iel))
 40   continue

      return
      end
