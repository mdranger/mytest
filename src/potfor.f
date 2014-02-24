
c               Copyright 1993 Colorado State University
c                       All Rights Reserved
c   modified by zli & sliu  03/21/2008
c   Add two extra factors, pforcfac, pcropcfac
c   to calculate the environmental factors
!   Add ECLUE model calculation for GPP 
!   ECLUE calculated the GPP and respiration
      subroutine potfor(month)
      integer month

!...Compute monthly potential production for forest

      include 'comput.inc'
      include 'const.inc'
      include 'param.inc'
      include 'parfs.inc'
      include 'parfx.inc'
      include 'plot1.inc'
      include 'plot3.inc'
      include 'potent.inc'
      include 'wth.inc'
      include 'zztim.inc'

!...RESPPT is an array of respiration values for the forest system
c     production components.

!...Added savanna model, pcropc now pforc
c                        tgprod now tfprod (BO)

!...Function declarations
      real     gpdf, frespr, laprod, pevap, pprdwc
      external gpdf, frespr, laprod, pevap, pprdwc

!...Local variables
      integer  i
      real     fcmax, frlive, lai, potprd, pptprd, resppt(5), 
     $         tfprod, tmoist, agefact,prdx3,tempaa
     $         ,cum_tmp_imp, pfac
!...Parameters to determin the age factor
!    defined in the param.inc
!      real     agefact_k, agefact_a, agefact_b, agefact_c,
!     $         agefact_d
!.........file for forest monthly output
       if (debug_flag .eq. 2 ) then
          open(unit=94, file="fmonthly.out", status="unknown")
       endif
!...age factor.  Liu Sept 2001
!    agefactor is used to describe the decrease of NPP as forest aging
       if (forage .eq. 0) then
          forage = 1
       endif

!       if (forage > 300.0) then
!       tempaa = 300.0/30.0
       if (steady .eq. 1) then
          tempaa = steadyage/30.0
       else
        tempaa = float (forage) / 30.0
       endif

        tempaa = float (forage) / 30.0
!...from Meldahl et al (1998)
c        agefact = (tempaa)**(-0.4498)

!... from FIA Maryland Data
!        agefact = (tempaa)**(-0.3665)
!....7.07 is the NPP at age 30
!....curve generated from Maryland FIA data and Meldahl et al, 1998
!        agefact = (6.19*exp(-0.0238*forage)+4.04)/7.07
!  04.2005, by zhengpeng li
!  use Shuguang Liu's formula
!  y=k*exp(a(x+b))*(x+b)^c+d
        agefact = agefact_k * exp( -1 * agefact_a * ( forage +
     $  agefact_b )) * (forage + agefact_b) ** agefact_c + agefact_d
     
        
        if (agefact > 2.) then
          agefact = 2.
        endif

!        agefact = exp(-0.0089*(forage-20.0))
        prdx3 = prdx(3) 
!....remove agefact because NPP goes down with time.  S.Liu 10/29/03
      if (debug_flag .eq. 2 ) then
       if (ftitle_flag .ne. 1) then
         write(94, *) 'Year month pforc GPP AutoResp
     $  pptprd potprd'
         ftitle_flag = 1
	endif
	endif
        

!...Estimate potential production based on temp & h2o
      if (stemp .gt. 0.0) then
      fgrowthlen = fgrowthlen + 1
!      if (cum_temp(month) .gt. 100.0) then
!....change back to use soil temp as growing season indicator
!....due to low temp in north east                                                                                           
!......gradual impact of cumulative temperature
         cum_tmp_imp = (cum_temp(month)-100.)/300.0
         if (cum_tmp_imp .gt. 1.0) then
              cum_tmp_imp = 1.0
         endif
         
               cum_tmp_imp = 1.0
                                                                                         
!...Calculate temperature effect on growth.
        potprd = gpdf(stemp, ppdf(1,2), ppdf(2,2), ppdf(3,2), 
     $                ppdf(4,2))

!...Added to match version 3.0 -lh 4/93
!....add cumulative temperature impact;
!         potprd = potprd * cum_tmp_imp


!...Calculate moisture effect on growth -
!     Value for potential plant production is now calculated from the
!     equation of a line whose intercept changes depending on water
!     content based on soil type.  The function PPRDWC contains the
!     equation for the line.  -rm 9/91
        tmoist = prcurr(month) + irract
 
        pet = pevap(month)
        if (pet .ge. .01) then
          pptprd = (avh2o(2) + tmoist) / (0.0001+pet)
        else
          pptprd = 0.01
        endif
                 
        pptprd = pprdwc(wc,pptprd,pprpts)
 
!...For large wood, calculate the percentage which is live (sapwood)
        frlive = sapk / (sapk + rlwodc)
 
!...Calculate LAI and then use in function for LAI reducer on
!     production.  -rm 5/91
        call lacalc(lai, rleavc, rlwodc, btolai, maxlai, klai)
        
!...Calculate monthly maintenance respiration
        resppt(LEAF) = frespr(tave,rleave(N))
        resppt(FROOT) = frespr(stemp,froote(N))
        resppt(FBRCH) = frespr(tave,fbrche(N))
        resppt(LWOOD) = frespr(tave,frlive*rlwode(N))
        resppt(CROOT) = frespr(stemp,frlive*croote(N))

        sumrsp = 0.0
        do 10 i = 1, FPARTS
          sumrsp = sumrsp + resppt(i)
10      continue
       
!...Use 2.0 to convert from biomass to carbon in forest system
!     - Mike Ryan & Dennis Ojime
!...Added calculation of fcmax  -rm  11/91
        tempaa = laprod(lai,laitop)

        fcmax = agefact*prdx3 * potprd * pptprd * tempaa
!...maximum net forest production depends on ecoregions. modified in Nov, 2002 by Liu
c        if (prdx3 .lt. 5) then
c          gpp_eco = 0.1
c        endif
!        fcmax = agefact*gpp_eco * potprd * pptprd * tempaa
!        fcmax = agefact*prdx(3) * potprd * pptprd * laprod(lai,laitop)
        agefact = 1.0
! tempaa - leaf related factor
! potprd - temperature effect on growth in gpdf 
! pptprd - precipation effect on growth in pprdwc
! laprod - lai effect

        pfac = agefact*potprd*pptprd*
     $      laprod(lai,laitop)*co2cpr(FORSYS)
     
        tfprod = pfac*prdx(2)
        pforcfac = pforcfac + pfac
!..       0 - use prdx in tree.100 no inversion
!..       1 - use commandline input -g and do inversion
!..       2 - use the prdxin.100 
!..       3 - use commandline -g but not do inversion
!..       4 - use the previous value read in from status_start.bin
        if ((npp_input .eq. 1).or.(npp_input .eq. 4)) then
            fcmax = pfac * prdxtree
        else if (npp_input .eq. 2) then
!....Added a prdx scalar read in and multiply here
!....this value is read each year
            pforc = fcmax * prdx_scalar
        else
!..npp_input = 0        
            fcmax = pfac*prdx(3)
        endif
        
!        if (prdx(3) < 5) then
!            fcmax = pfac*prdx(3)
!            flaginv = 0
!        else
!           flaginv = 1+flaginv
!...cause some high prdx values after clear cutting            
        endif
         
!....the following was commented by Liu Sept 2001
!....there was no NPP for some months even if there is LAI even for growing
!....forests
        if (pforc .lt. 0.0) then
          pforc = 0
        endif
!        if (pforc .gt. fcmax) then
          
          pforc = fcmax
       
       if (debug_flag .eq. 2 ) then
        write (94, 46) time, month, pforc
       endif
46     format (f8.3, i4, 7f8.2)
!       else 
!      set pforc to 0 if not in growing season       
!       pforc = 0.0    
!        endif

!      if (month .eq. 7) then
!            print *, month, forage,fcmax,
!     $    potprd,pptprd,tempaa,lai,rleavc
!      endif

!      else
!        tfprod = 0.
!        pforc = 0.
!        write(94, 46) int(time), month, -1.0, -1.0, -1.0,-1.0,-1.0,-1.0 
!      endif
      
      return
      end
