!     
! File:   ghg.f90
! Author: zhengpengli
!
! Created on March 17, 2010, 2:12 PM
! to hold the new variables for wetland GHG emission
!
!EDCM provided the outputs of the gaseous losses of organic N associated with mineralization/nitrification (VOLGMA), denitrification (VOLEXA), and volatilization from maturing crops or senescing grassland (VOLPLA).
!Using the empirical formula from Davison, data
!Log[NO/N2O] = -3.79 WFPS + 2.73 (from the TRAGNET database across the world)
!Log[no/n2o] = -2.64 wfps + 1.66 (from Davidson, 2000).
!Davidson, E. A. and L. V. Verchot (2000).
!"Testing the hole-in-the-pipe model of nitric and nitrous oxide emissions from soils using the TRAGNET database." Global biogeochemical cycles 14(4): 1035-1043.
!Variables:
!n2o - monthly derived N2O
!n2oa - accumulation of N2O 
!NO - accumulation of NO for each month
!CH4 - n2o,no,nh3,n2,ch4a,real n2o,no,nh3,n2,ch4, ch4a
!ch4a - CH4 accumulator
       common/ghg/ n2oa, ch4a
       real n2oa, ch4a

       save /ghg/