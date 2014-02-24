
c               Copyright 1993 Colorado State University
c                       All Rights Reserved

      subroutine annacc

c...Reset annual accumulators.

      include 'const.inc'
      include 'plot1.inc'
      include 'plot2.inc'
      include 'plot3.inc'
      include 'param.inc'
      include 'parfs.inc'
      include 'timvar.inc'
      include 'zztim.inc'
      include 'parcp.inc'
      include 'wetland.inc'

c...Local variables
      integer iel
      

c...Initialize annual removal accumulators
!...Add by zp, add crmvst in the list,
      tcrem = 0.0
      prcann = 0.0
      petann = 0.0
      nfixac = 0.0
      cgrain = 0.0
      crmvst = 0.0
      snfxac(CRPSYS) = 0.0
      snfxac(FORSYS) = 0.0
      accrst = 0.0
      shrema = 0.0
      shrmai(UNLABL) = 0.0
      shrmai(LABELD) = 0.0
      sdrema = 0.0
      sdrmai(UNLABL) = 0.0
      sdrmai(LABELD) = 0.0
      creta = 0.0
      resp(UNLABL) = 0.0
      resp(LABELD) = 0.0
      do 10 iel = 1, nelem
	ereta(iel) = 0.0
	shrmae(iel) = 0.0
	sdrmae(iel) = 0.0

c...Initialize mineralization accumulators
	tnetmn(iel) = 0.0
	sumnrs(iel) = 0.0
        soilnm(iel) = 0.0
10    continue

c...Initialize annual C production
      cproda = 0.0
      cprodc = 0.0
      cprodf = 0.0
      fcacc = 0.0
!...Initial annual GHG emission accumulators
      n2oa = 0.0
      ch4a = 0.0
      ch4vege = 0.0
      ch4soil = 0.0
!      print *, "reset", ch4a
c....When input set the flag to inversion then do it
c....Otherwise not inversion 
!..       0 - use prdx in tree.100 no inversion
!..       1 - use commandline input -g and do inversion
!..       2 - use the prdxin.100 
!..       3 - use commandline -g but not do inversion
!..       4 - use the previous value read in from status_start.bin
!...inversion for first 15 years, then using 
!...inverted prdx      
!      print *, 'flag ', flaginv, int(time)-strtyr
      if ( npp_input .eq. 1 ) then
      if  ((int(time)-strtyr ).lt.10)then
c...MODIS NPP is annual, need to transform to monthly NPP      
c...by dividing the growing season
c...but then it need to multiply the gowing season length
        if ( flaginv > 0 ) then
            if (pforcfac .eq. 0) then
            treeprdxt = 0.0
            prdxtree = gpp_eco
            else
c        else if ((int(time)-strtyr ).le.5 )then
            prdxtree = gpp_eco / pforcfac
            endif
            if (prdxtree > 1000) then 
            prdxtree = 1000.0
            endif
            

            if ((int(time)-strtyr ).gt.3 )then
                treeprdxt = prdxtree /6 + treeprdxt
            endif
c...Finish tree prdx        
c        if ( (int(time)-strtyr ).gt.1 ) then
c              if (pcropcfac .eq. 0) then
c              prdxcrop = gpp_eco
c              cropprdxt = 0.0
c              else
c              prdxcrop = gpp_eco /pcropcfac / (1 + rtsh) * 2.5
c        print *, '...', int(time), pcropcfac, prdxcrop, 1/(1+rtsh)
c              else
c              cropprdxt = gpp_eco /growthlen / pcropcfac / (1 + rtsh)
c              endif
        
c        if (cropprdxt > prdxcrop .and. (int(time)-strtyr ).gt.5) then
c            if ((int(time)-strtyr ).gt.3 )then
c                cropprdxt = prdxcrop /6 + cropprdxt
c            endif
                        
c            if (prdxcrop > 1000) then 
c            prdxcrop = 1000.0
c            endif
        else
c... flag inv equals to 0, save the current prdx to treeprdx
           treeprdxt = prdxtree
c           cropprdxt = prdxcrop
        endif
      else
c        stop
c         if treeprdxt .
         prdxtree = treeprdxt
c         prdxcrop = cropprdxt
      endif
      endif
c...end inversion process
c      print *, int(time)-strtyr , prdxtree, treeprdxt

c      print *, 'growing', growthlen, fgrowthlen
c      print *, treeprdxt, cropprdxt
      pforcfac = 0.0
      pcropcfac = 0.0
      growthlen = 0.0
      fgrowthlen = 0.0
c...Initialize cinputs
      cinput = 0.0

c...Reset minimum total non-living C, an annual value
      totc = 1000000
      
c...Initialize co2 accumulators (10/92)
      ast1c2 = 0.0
      ast2c2 = 0.0
      amt1c2 = 0.0
      amt2c2 = 0.0
      as11c2 = 0.0
      as21c2 = 0.0
      as2c2 = 0.0
      as3c2 = 0.0

      return
      end
