

c               Copyright 1993 Colorado State University
c                       All Rights Reserved

C...RESTRP.F
c...Restrict the actual production based on C/E ratios.  Calculate
c     minimum, and maximum whole plant nutrient concentrations.
c..find some increase after this function
c..need to find out why
      subroutine restrp(nelem, nparts, avefrc, cerat, cfrac, potenc,
     $                  rimpct, storage, snfxmx, cprodl, eprodl,
     $                  eup, uptake, elimit, nfix, snfxac, ptagc)

      include 'const.inc'
      include 'fertil.inc'
      include 'parfx.inc'
      include 'seq.inc'
      include 'zztim.inc'
      
      

      integer	nelem, nparts, elimit
      real	avefrc(MAXIEL), cerat(2,nparts,MAXIEL), cfrac(nparts),
     $		cprodl, eprodl(MAXIEL), eup(nparts,MAXIEL),
     $          nfix, potenc, rimpct, snfxac, snfxmx,
     $		storage(MAXIEL), uptake(4,MAXIEL), ptagc

c...Local variables
c   NOTE:  Local variables cannot have adjustable array size.  MINECI
c          and MAXECI are set to the largest array size which may occur.
      integer	iel, ipart
      real	afert(MAXIEL), eavail(MAXIEL), minec(MAXIEL), 
     $		maxec(MAXIEL), mineci(5,MAXIEL),
     $          maxeci(5,MAXIEL), ustorg(MAXIEL)

c....keep potenc of forest in PTAGC (potential production for crops/pasture)
       ptagc = potenc 
      
c...Reset variables to zero
      cprodl = 0.0
      do 30 iel = 1, nelem
	eprodl(iel) = 0.0
	afert(iel) = 0.0
	do 20 ipart = 1, nparts
	  eup(ipart,iel) = 0.0
 20     continue
 30   continue

c...There is no production if one of the mineral elements in not
c     available.
      do 40 iel = 1, nelem
        if ((avefrc(iel) .le. 0.0) .and. (snfxmx .eq. 0.0)) then
          return
        endif
 40   continue

c...Initialize cprodl
      cprodl = potenc
      
c...Calculate soil available nutrients, adding storage.
      do 60 iel = 1, nelem
	eavail(iel) = favail(iel) * rimpct * avefrc(iel)
     $                  + storage(iel)
 60   continue


c...Calculate average E/C of whole plant (crop, grass, or tree)
      do 100 iel = 1, nelem
	minec(iel) = 0.0
	maxec(iel) = 0.0
	do 90 ipart = 1, nparts
	  mineci(ipart,iel) = 1 / (0.0001+cerat(IMAX,ipart,iel))
	  maxeci(ipart,iel) = 1 / (0.0001+cerat(IMIN,ipart,iel))
	  minec(iel) = minec(iel) + cfrac(ipart) * mineci(ipart,iel)
	  maxec(iel) = maxec(iel) + cfrac(ipart) * maxeci(ipart,iel)
 90     continue
 100  continue
       

c...Compute the limitation
      
      call nutrlm(nelem, nparts, cprodl, eprodl, maxec,
     $            maxeci, mineci, cfrac, eavail, nfix,
     $            snfxmx, snfxac, elimit, eup)
       
c...Calculate uptakes from all sources: storage, soil, nfix
      do 200 iel = 1, nelem
	ustorg(iel) = min(storage(iel), eprodl(iel))

c...If storage pool contains all needed for uptake
      if (eprodl(iel) .le. ustorg(iel)) then
	uptake(ESTOR,iel) = eprodl(iel)
	uptake(ESOIL,iel) = 0.0

c...Otherwise, extra necessary from the soil pool
        elseif (eprodl(iel) .gt. ustorg(iel)) then
	  uptake(ESTOR,iel) = storage(iel)
	  uptake(ESOIL,iel) = eprodl(iel) - storage(iel)

        endif

 200  continue

c...N fixation uptake was computed in the limitation routines
      uptake(ENFIX,N) = nfix

      return
      end


