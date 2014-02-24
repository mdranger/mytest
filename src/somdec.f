
c               Copyright 1993 Colorado State University
c                       All Rights Reserved


!...SOMDEC.F

      subroutine somdec(dtm)

      real      dtm

!...Soil Organic Matter Decomposition                written by vek, 04/91
!     Decompose SOM1 (surface and soil), SOM2, and SOM3.
!         defac = decomposition factor based on water and
!                 temperature computed in prelim and in cycle
!         dtm = time step (dt/ntspm)
!    anerb - anerobic factor on decomposition, used to calculate the soil

      include 'comput.inc'
      include 'const.inc'
      include 'param.inc'
      include 'parcp.inc'
      include 'parfx.inc'
      include 'plot1.inc'
      include 'plot2.inc'
      include 'zztim.inc'

!...Function declarations
      real      bgdrat

!...Local variables
      integer   iel
      real      accum(ISOS),cf3co2, cfs1s2, cfs1s3, cfs2s1, cfs2s3, 
     $          cfs3s1, cfsfs2, cleach, co2los, linten, mnrflo, orgflow, 
     $          radds1, rceof1, rceto1(MAXIEL), rceto2(MAXIEL), 
     $          rceto3(MAXIEL), rcetob, tcflow
      logical   candec

c*******************************************************************


      accum(LABELD) = 0.0
      accum(UNLABL) = 0.0


!...Surface SOM1 decomposes to SOM2 with CO2 loss
      if (som1c(SRFC) .gt. 1.e-07) then

!...Determine C/E ratios for flows to som2
          do 10 iel=1,nelem
            radds1 = rad1p(1,iel) + rad1p(2,iel) *
     $                 ((som1c(1)/som1e(1,iel))-pcemic(2,iel))
            rceto2(iel) = som1c(SRFC)/som1e(SRFC,iel) + radds1
            rceto2(iel) = max(rceto2(iel), rad1p(3,iel))
10        continue

!...Compute total C flow out of surface microbes.
          tcflow = som1c(SRFC) * defac * dec3(SRFC) * dtm

!...where
!     som1c(SRFC) =  unlabeled and labeled C in surface microbes
!                      (som1ci(SRFC,1)+som1ci(SRFC,2))
!     dec3(SRFC)  =  intrinsic decomposition rate of
!                      surface microbes

!...If decomposition can occur, schedule flows associated with respiration 
!...     and decomposition 
          if (candec(nelem,aminrl,som1c(SRFC),som1e,2,SRFC,rceto2)) then

!...CO2 loss - Compute and schedule respiration flows.
              co2los = tcflow * p1co2(SRFC)
!...where 
!     p1co2(SRFC) is set to p1co2a(SRFC) in prelim.
!     p1co2a(SRFC) is a fixed parameter;
!     intercept parameter which controls flow from soil organic matter 
!       with fast turnover to CO2 (fraction of carbon lost to CO2 when 
!       there is no sand in the soil)
!     Changed csrsnk to s11c2 (10/92)

              call respir(co2los,2,SRFC,som1c,som1ci,s11c2,resp,
     $                      som1e,minerl,gromin,s1mnr)


!...Decompose Surface SOM1 to SOM2

!...cfsfs2 is C Flow from SurFace som1 to Som2
              cfsfs2 = tcflow - co2los

!...Partition and schedule C flows by isotope
              call csched(cfsfs2,som1ci(SRFC,LABELD),som1c(SRFC),
     $                    som1ci(SRFC,UNLABL),som2ci(UNLABL),
     $                    som1ci(SRFC,LABELD),som2ci(LABELD),
     $                    1.0,accum)


!...Compute and schedule N, P, and S flows.

!...Update mineralization accumulators.
              do 20 iel=1,nelem
                  call esched(cfsfs2,som1c(SRFC),rceto2(iel),
     $                        som1e(SRFC,iel),som2e(iel),
     $                        minerl(SRFC,iel),mnrflo)
                  call mnracc(mnrflo,gromin(iel),s1mnr(SRFC,iel))
20            continue
          endif
      endif

!...End of SOM1 (surface layer) Decomposition

!*******************************************************************

!...Soil SOM1 decomposes to SOM2 and SOM3 with CO2 loss and possible leaching 
c     of organics.
      if (som1c(SOIL) .gt. 1.e-07) then

!...Determine C/E ratios for flows to som2
          do 30 iel=1,nelem
            rceto2(iel) = bgdrat(aminrl,varat2,iel)
30        continue


!...Compute total C flow out of soil microbes.

!...Added impact of soil anaerobic conditions -rm 12/91
          tcflow = som1c(SOIL) * defac *dec3(SOIL) *cltfac(1) * eftext *
     $              anerb * dtm
!...where
c     som1c(SOIL) = unlabeled and labeled C in soil microbes
c                     (som1ci(SOIL,1)+som1ci(SOIL,2))
c     dec3(SOIL)  = intrinsic decomposition rate of
c                     soil microbes
c     cltfac(1)   = cultivation factor for som1
c                     (set in cycle)
c     eftext      = effect of soil texture on the soil microbe
c                     decomposition rate (computed in prelim)


!...If soil som1 can decompose to som2, it will also go to som3.
c     If it can't go to som2, it can't decompose at all.

!...If decomposition can occur,
          if (candec(nelem,aminrl,som1c(SOIL),som1e,2,SOIL,rceto2)) then

!...CO2 Loss - Compute and schedule respiration flows
              co2los = tcflow * p1co2(SOIL)

!...where 
!     p1co2(SOIL) is computed in prelim as a function of fixed parameters 
!       p1co2a(SOIL) and p1co2b(SOIL) and soil texture.
!...Changed csrsnk to s21c2 (10/92)
              call respir(co2los,2,SOIL,som1c,som1ci,s21c2,resp,
     $                          som1e,minerl,gromin,s1mnr)

!...Decompose Soil SOM1 to SOM3
c     The fraction of tcflow that goes to SOM3 is a function of 
c     clay content (fps1s3 is computed in prelim).
              cfs1s3 = tcflow * fps1s3 * (1.0 + animpt * 
     $                      (1.0 -anerb))

!...Partition and schedule C flows by isotope
              call csched(cfs1s3,som1ci(SOIL,LABELD),som1c(SOIL),
     $                    som1ci(SOIL,UNLABL),som3ci(UNLABL),
     $                          som1ci(SOIL,LABELD),som3ci(LABELD),
     $                    1.0,accum)

!...Compute and schedule N, P, and S flows and update mineralization 
c     accumulators.
              do 40 iel=1,nelem
                  rceto3(iel) = bgdrat(aminrl,varat3,iel)
                  call esched(cfs1s3,som1c(SOIL),rceto3(iel),
     $                        som1e(SOIL,iel),som3e(iel),
     $                        minerl(SRFC,iel),mnrflo)
                  call mnracc(mnrflo,gromin(iel),s1mnr(SOIL,iel))
40              continue

!...Leaching of Organics
c     This only occurs when the water flow out of water layer 2
c     exceeds a critical value.  Use the same C/N, C/P, and C/S
c     ratios as for the flow to SOM3.

!...Removed organic leaching sink and replaced it with the
!     stream flows. -rm 2/92
              if(amov(2) .gt. 0.0) then
                  linten = min(1.0-(omlech(3)-amov(2))/
     $                  (0.001+omlech(3)), 1.0)
                  cleach = tcflow * orglch * linten
!...Partition and schedule C flows by isotope
                  call csched(cleach,som1ci(SOIL,LABELD),som1c(SOIL),
     $                        som1ci(SOIL,UNLABL),strm5u,
     $                        som1ci(SOIL,LABELD),strm5l,
     $                        1.0,accum)

!...Compute and schedule N, P, and S flows and update mineralization 
!     accumulators.
                  do 50 iel=1,nelem

!...Need to use the ratio for som1 for organic leaching     -rm 3/92
                      rceof1 = som1c(SOIL) / (0.0001+som1e(SOIL,iel))
                      orgflow = cleach / (0.0001+rceof1)
                      call flow(som1e(SOIL,iel),stream(iel+5),time,
     $                          orgflow)
50                continue

!...No leaching this time step
              else
                  cleach = 0.
              endif

!...Decompose Soil SOM1 to SOM2.
!     SOM2 gets what's left of tcflow.
              cfs1s2 = tcflow - co2los - cfs1s3 - cleach

!...Partition and schedule C flows by isotope
              call csched(cfs1s2,som1ci(SOIL,LABELD),som1c(SOIL),
     $                     som1ci(SOIL,UNLABL),som2ci(UNLABL),
     $                           som1ci(SOIL,LABELD),som2ci(LABELD),
     $                     1.0,accum)

!...Compute and schedule N, P, and S flows and update mineralization 
!     accumulators.
              do 60 iel=1,nelem
                  call esched(cfs1s2,som1c(SOIL),rceto2(iel),
     $                        som1e(SOIL,iel),som2e(iel),
     $                        minerl(SRFC,iel),mnrflo)
                  call mnracc(mnrflo,gromin(iel),s1mnr(SOIL,iel))
60              continue
          endif
      endif

!...End of Soil SOM1 decomposition

c*****************************************************************

!...SOM2 decomposes to soil SOM1 and SOM3 with CO2 loss
      if (som2c .gt. 1.e-07) then

!...Determine C/E ratios for flows to SOM1
          do 70 iel=1,nelem
            rceto1(iel) = bgdrat(aminrl,varat1,iel)
70        continue


!...Compute total C flow out of SOM2C

!...Added impact of soil anaerobic conditions -rm 12/91
          tcflow = som2c * defac * dec5 * cltfac(2) * anerb * dtm
!...where
!     dec5      = intrinsic decomposition rate of som2
!     cltfac(2) = cultivation factor for som2 (set in cycle)

!...If som2 can decompose to som1, it will also go to som3.
!     If it can't go to som1, it can't decompose at all.

!...If decomposition can occur,
          if (candec(nelem,aminrl,som2c,som2e,1,1,rceto1)) then

!...CO2 loss - Compute and schedule respiration flows
              co2los = tcflow * p2co2

!...Changed csrsnk to s2c2 (10/92)
              call respir(co2los,1,SRFC,som2c,som2ci,s2c2,resp,
     $                      som2e,minerl,gromin,s2mnr)

!...Rearranged the order of calculation.  There is a calculated
!     impact of clay on the decomposition of som2 to som3.  -rm 12/91

!...Decompose SOM2 to SOM3, SOM3 gets what's left of tcflow.
!     Added impact of soil anaerobic conditions -rm 12/91
              cfs2s3 = tcflow * fps2s3 * (1.0 + animpt * (1.0 - anerb))

!...Partition and schedule C flows by isotope
              call csched(cfs2s3,som2ci(LABELD),som2c,
     $                   som2ci(UNLABL),som3ci(UNLABL),
     $                   som2ci(LABELD),som3ci(LABELD),
     $                   1.0,accum)

!...Compute and schedule N, P, and S flows and update mineralization 
!     accumulators.
              do 80 iel=1,nelem
                   rcetob = bgdrat(aminrl,varat3,iel)
                   call esched(cfs2s3,som2c,rcetob,som2e(iel),
     $                         som3e(iel),minerl(SRFC,iel),mnrflo)
                  call mnracc(mnrflo,gromin(iel),s2mnr(iel))
80                 continue

!...Decompose SOM2 to SOM1

!...Added impact of soil anaerobic conditions -rm 12/91
              cfs2s1 = tcflow  - co2los - cfs2s3

!...Partition and schedule C flows by isotope
              call csched(cfs2s1,som2ci(LABELD),som2c,
     $                    som2ci(UNLABL),som1ci(SOIL,UNLABL),
     $                          som2ci(LABELD),som1ci(SOIL,LABELD),
     $                    1.0,accum)

!...Compute and schedule N, P, and S flows and update mineralization 
!     accumulators.
              do 90 iel=1,nelem
                  call esched(cfs2s1,som2c,rceto1(iel),som2e(iel),
     $                        som1e(SOIL,iel),minerl(SRFC,iel),mnrflo)
                  call mnracc(mnrflo,gromin(iel),s2mnr(iel))
90              continue

          endif
      endif

!...End of SOM2 Decompositon

!**********************************************************************

!...SOM3 decomposes to soil SOM1 with CO2 loss.
      if (som3c .gt. 1.e-07) then

!...Determine C/E ratios for flows to SOM1.
          do 100 iel=1,nelem
            rceto1(iel) = bgdrat(aminrl,varat1,iel)
100       continue

!...Compute total C flow out of SOM3C
          tcflow = som3c * defac * dec4 * cltfac(3) * anerb * dtm

!...where
!     dec4      = intrinsic decomposition rate of som2
!     cltfac(3) = cultivation factor for som3 (set in cycle)

!...If decomposition can occur,

          if (candec(nelem,aminrl,som3c,som3e,1,1,rceto1)) then

!...CO2 loss - Compute and schedule respiration flows.
              cf3co2 = tcflow * p3co2 * anerb

!...Changed csrsnk to s3c2 (10/92)
              call respir(cf3co2,1,SRFC,som3c,som3ci,s3c2,resp,
     $                          som3e,minerl,gromin,s3mnr)

!...Decompose SOM3 to soil SOM1
              cfs3s1 = tcflow - cf3co2

!...Partition and schedule C flows by isotope
              call csched(cfs3s1,som3ci(LABELD),som3c,
     $                    som3ci(UNLABL),som1ci(SOIL,UNLABL),
     $                          som3ci(LABELD),som1ci(SOIL,LABELD),
     $                    1.0,accum)

!...Compute and schedule N, P, and S flows and update mineralization 
c     accumulators.
              do 110 iel=1,nelem
                  call esched(cfs3s1,som3c,rceto1(iel),som3e(iel),
     $                        som1e(SOIL,iel),minerl(SRFC,iel),mnrflo)
                  call mnracc(mnrflo,gromin(iel),s3mnr(iel))
110                  continue
          endif
      endif

!...End of SOM3 Decomposition

      return
      end
