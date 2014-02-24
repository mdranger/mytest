!   Methane Production algorithm in wetlands
!...12/02/2009  Add CH4 production under anerobic conditions
!...It calculate the soil EH based on DNDC approach (Zhang, 2002)
!...Eh change = rate of change * (al - i) when this soil layer is lower than water table
!...          = rate of change * (al + i - wfpsl) when soil layer is higher than water table
!... al = FRD * Pa * RLD(i)
!... FRD is the area of the cross section of a typical fine root (cm2), constant 0.0013 cm-2,
!... (Barver& Silberbush, 1984)
!... Pa is the salar for the degree of gas diffusion from root to the atmosphere(0 - 1)
!... RLD(i) is the root length density (cm root cm-3) soil.
!...Methane production
!...In DNDC, the calcualtion condsidered following effect factors:
!...1. C substrate, either from soil or root biomass
!     CH4_P1 = 15.0 * Coef1 * C_root / 6.43  * 86400.0 / 10000.0 * 1000.0 / 12.0 * F_ph * F_time * CH4_balloon * F_temp * F_texture * MicrobioIndex / F_leak; //mol CH4/ha/d/layer,
!     CH4_P2 = 30.0 * Coef1 * C_soil / 5.378 * 86400.0 / 10000.0 * 1000.0 / 12.0 * F_ph * F_time * CH4_balloon * F_temp * F_texture * MicrobioIndex / F_leak; //
!... the production of wetland assume the wetland have surface water existing
!...temp - monthly temperature
!...
!...ch4prod - monthly CH4 production
!...We do the first step, just assume that the production of methane is related with
!...soil carbon and temperature effect
!...Add surface water depth relationship

!       real function ch4_prod(aneref, drain, rprpet, pet, tave, micosm,
!     $ somtc, cropdrain)

      real function ch4prod(stemp, soilc)

      real      stemp, soilc

      include 'dovars.inc'
      include 'wetland.inc'
!F_temp = (float)exp(0.0693 * T) / 7.996 * 1e-11;
!      floodtime is range from 0 to 1.0
!      surfwater is in cm units, can be 0 to 100cm (1 meter surface water)
      if (doflood ) then
          ch4prod = exp(stemp * 0.0693) * soilc * 0.001 * floodtime
      else
          ch4prod = 0.0
      endif
!      print *, "inflood", ch4prod, stemp, soilc
      return
      end


