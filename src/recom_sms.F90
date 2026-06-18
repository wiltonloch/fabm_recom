#include "fabm_driver.h"

module awi_recom_sms

    use fabm_types

    implicit none

    private

    type, extends(type_base_model), public :: type_awi_recom_sms

        ! Dependencies
        type (type_horizontal_dependency_id) :: id_surface_air_pressure
        type (type_horizontal_dependency_id) :: id_swr0
        type (type_horizontal_dependency_id) :: id_latitude

        type (type_dependency_id) :: id_temperature
        type (type_dependency_id) :: id_practical_salinity
        type (type_dependency_id) :: id_par
        type (type_dependency_id) :: id_depth

        type (type_global_dependency_id) :: id_days_since_start_of_year

        ! Base state variables
        type (type_state_variable_id) :: id_dissolved_inorganic_nitrogen
        type (type_state_variable_id) :: id_dissolved_inorganic_carbon
        type (type_state_variable_id) :: id_alkalinity
        type (type_state_variable_id) :: id_phytoplankton_nitrogen
        type (type_state_variable_id) :: id_phytoplankton_carbon
        type (type_state_variable_id) :: id_phytoplankton_chlorophyll
        type (type_state_variable_id) :: id_detrital_nitrogen
        type (type_state_variable_id) :: id_detrital_carbon
        type (type_state_variable_id) :: id_heterotroph_nitrogen
        type (type_state_variable_id) :: id_heterotroph_carbon
        type (type_state_variable_id) :: id_dissolved_organic_nitrogen
        type (type_state_variable_id) :: id_dissolved_organic_carbon
        type (type_state_variable_id) :: id_diatom_nitrogen
        type (type_state_variable_id) :: id_diatom_carbon
        type (type_state_variable_id) :: id_diatom_chlorophyll
        type (type_state_variable_id) :: id_diatom_silica
        type (type_state_variable_id) :: id_detrital_silica
        type (type_state_variable_id) :: id_silica
        type (type_state_variable_id) :: id_iron
        type (type_state_variable_id) :: id_phytoplankton_calcite
        type (type_state_variable_id) :: id_detrital_calcite
        type (type_state_variable_id) :: id_oxygen

        ! Extra state variables
        type (type_state_variable_id) :: id_macrozooplankton_nitrogen
        type (type_state_variable_id) :: id_macrozooplankton_carbon
        type (type_state_variable_id) :: id_macrozooplankton_detrital_nitrogen
        type (type_state_variable_id) :: id_macrozooplankton_detrital_carbon
        type (type_state_variable_id) :: id_macrozooplankton_detrital_silica
        type (type_state_variable_id) :: id_macrozooplankton_detrital_calcite
        type (type_state_variable_id) :: id_microzooplankton_nitrogen
        type (type_state_variable_id) :: id_microzooplankton_carbon
        type (type_state_variable_id) :: id_coccolithophore_nitrogen
        type (type_state_variable_id) :: id_coccolithophore_carbon
        type (type_state_variable_id) :: id_coccolithophore_chlorophyll
        type (type_state_variable_id) :: id_phaeocystis_nitrogen
        type (type_state_variable_id) :: id_phaeocystis_carbon
        type (type_state_variable_id) :: id_phaeocystis_chlorophyll

    contains
        procedure :: initialize
        procedure :: do
    end type type_awi_recom_sms

contains

    subroutine initialize(self, configunit)
        use fabm_types, only: standard_variables
        use recom_init_interface, only: initialize_tracer_ids, get_tracer_init_value
        use recom_declarations, only: tracer_ids
        use recom_diags_management, only: allocate_and_init_diags

        implicit none

        class(type_awi_recom_sms), intent(inout), target :: self
        integer, intent(in) :: configunit

        call read_recom_namelist()
        call initialize_tracer_ids()
        call allocate_and_init_diags(2)

        call self%register_dependency(self%id_surface_air_pressure, standard_variables%surface_air_pressure)
        call self%register_dependency(self%id_temperature, standard_variables%temperature)
        call self%register_dependency(self%id_practical_salinity, standard_variables%practical_salinity)
        call self%register_dependency(self%id_par, standard_variables%downwelling_photosynthetic_radiative_flux)
        call self%register_dependency(self%id_depth, standard_variables%depth)
        call self%register_dependency(self%id_swr0, standard_variables%surface_downwelling_shortwave_flux)
        call self%register_dependency(self%id_latitude, standard_variables%latitude)
        call self%register_dependency(self%id_days_since_start_of_year, standard_variables%number_of_days_since_start_of_the_year)

        call self%register_state_variable(self%id_dissolved_inorganic_nitrogen, 'dissolved_inorganic_nitrogen', 'mmol m-3', 'Dissolved Inorganic Nitrogen', standard_variable=standard_variables%mole_concentration_of_nitrate)
        call self%register_state_variable(self%id_dissolved_inorganic_carbon, 'dissolved_inorganic_carbon', 'mmol m-3', 'Dissolved Inorganic Carbon', standard_variable=standard_variables%mole_concentration_of_dissolved_inorganic_carbon)
        call self%register_state_variable(self%id_alkalinity, 'alkalinity', 'mmol m-3', 'Alkalinity', standard_variable=standard_variables%alkalinity_expressed_as_mole_equivalent)

        call self%register_state_variable(self%id_phytoplankton_nitrogen, 'phytoplankton_nitrogen', 'mmol m-3', 'Phytoplankton Nitrogen', get_tracer_init_value(tracer_ids%phytoplankton_nitrogen))
        call self%register_state_variable(self%id_phytoplankton_carbon, 'phytoplankton_carbon', 'mmol m-3', 'Phytoplankton Carbon', get_tracer_init_value(tracer_ids%phytoplankton_carbon))
        call self%register_state_variable(self%id_phytoplankton_chlorophyll, 'phytoplankton_chlorophyll', 'mg m-3', 'Phytoplankton Chlorophyll', get_tracer_init_value(tracer_ids%phytoplankton_chlorophyll))
        call self%register_state_variable(self%id_detrital_nitrogen, 'detrital_nitrogen', 'mmol m-3', 'Detrital Nitrogen', get_tracer_init_value(tracer_ids%detrital_nitrogen))
        call self%register_state_variable(self%id_detrital_carbon, 'detrital_carbon', 'mmol m-3', 'Detrital Carbon', get_tracer_init_value(tracer_ids%detrital_carbon))
        call self%register_state_variable(self%id_heterotroph_nitrogen, 'heterotroph_nitrogen', 'mmol m-3', 'Heterotroph Nitrogen', get_tracer_init_value(tracer_ids%heterotroph_nitrogen))
        call self%register_state_variable(self%id_heterotroph_carbon, 'heterotroph_carbon', 'mmol m-3', 'Heterotroph Carbon', get_tracer_init_value(tracer_ids%heterotroph_carbon))
        call self%register_state_variable(self%id_dissolved_organic_nitrogen, 'dissolved_organic_nitrogen', 'mmol m-3', 'Dissolved Organic Nitrogen', get_tracer_init_value(tracer_ids%dissolved_organic_nitrogen))
        call self%register_state_variable(self%id_dissolved_organic_carbon, 'extracellular_organic_carbon', 'mmol m-3', 'Extracellular Organic Carbon', get_tracer_init_value(tracer_ids%dissolved_organic_carbon))
        call self%register_state_variable(self%id_diatom_nitrogen, 'diatom_nitrogen', 'mmol m-3', 'Diatom Nitrogen', get_tracer_init_value(tracer_ids%diatom_nitrogen))
        call self%register_state_variable(self%id_diatom_carbon, 'diatom_carbon', 'mmol m-3', 'Diatom Carbon', get_tracer_init_value(tracer_ids%diatom_carbon))
        call self%register_state_variable(self%id_diatom_chlorophyll, 'diatom_chlorophyll', 'mg m-3', 'Diatom Chlorophyll', get_tracer_init_value(tracer_ids%diatom_chlorophyll))
        call self%register_state_variable(self%id_diatom_silica, 'diatom_silicate', 'mmol m-3', 'Diatom Silicate', get_tracer_init_value(tracer_ids%diatom_silica))
        call self%register_state_variable(self%id_detrital_silica, 'detrital_silicate', 'mmol m-3', 'Detrital Silicate', get_tracer_init_value(tracer_ids%detrital_silica))
        call self%register_state_variable(self%id_silica, 'dissolved_silicate', 'mmol m-3', 'Dissolved Silicate', get_tracer_init_value(tracer_ids%silica))
        call self%register_state_variable(self%id_iron, 'dissolved_iron', 'mmol m-3', 'Dissolved Iron', get_tracer_init_value(tracer_ids%iron))
        call self%register_state_variable(self%id_phytoplankton_calcite, 'phytoplankton_calcite', 'mmol m-3', 'Phytoplankton Calcite', get_tracer_init_value(tracer_ids%phytoplankton_calcite))
        call self%register_state_variable(self%id_detrital_calcite, 'detrital_calcite', 'mmol m-3', 'Detrital Calcite', get_tracer_init_value(tracer_ids%detrital_calcite))
        call self%register_state_variable(self%id_oxygen, 'dissolved_oxygen', 'mmol m-3', 'Dissolved Oxygen', get_tracer_init_value(tracer_ids%oxygen))

        call self%register_state_variable(self%id_macrozooplankton_nitrogen, 'macrozooplankton_nitrogen', 'mmol m-3', 'Macrozooplankton Nitrogen', get_tracer_init_value(tracer_ids%macrozooplankton_nitrogen))
        call self%register_state_variable(self%id_macrozooplankton_carbon, 'macrozooplankton_carbon', 'mmol m-3', 'Macrozooplankton Carbon', get_tracer_init_value(tracer_ids%macrozooplankton_carbon))
        call self%register_state_variable(self%id_macrozooplankton_detrital_nitrogen, 'macrozooplankton_detrital_nitrogen', 'mmol m-3', 'Macrozooplankton Detrital Nitrogen', get_tracer_init_value(tracer_ids%macrozooplankton_detrital_nitrogen))
        call self%register_state_variable(self%id_macrozooplankton_detrital_carbon, 'macrozooplankton_detrital_carbon', 'mmol m-3', 'Macrozooplankton Detrital Carbon', get_tracer_init_value(tracer_ids%macrozooplankton_detrital_carbon))
        call self%register_state_variable(self%id_macrozooplankton_detrital_silica, 'macrozooplankton_detrital_silica', 'mmol m-3', 'Macrozooplankton Detrital Silica', get_tracer_init_value(tracer_ids%macrozooplankton_detrital_silica))
        call self%register_state_variable(self%id_macrozooplankton_detrital_calcite, 'macrozooplankton_detrital_calcite', 'mmol m-3', 'Macrozooplankton Detrital Calcite', get_tracer_init_value(tracer_ids%macrozooplankton_detrital_calcite))
        call self%register_state_variable(self%id_microzooplankton_nitrogen, 'microzooplankton_nitrogen', 'mmol m-3', 'Microzooplankton Nitrogen', get_tracer_init_value(tracer_ids%microzooplankton_nitrogen))
        call self%register_state_variable(self%id_microzooplankton_carbon, 'microzooplankton_carbon', 'mmol m-3', 'Microzooplankton Carbon', get_tracer_init_value(tracer_ids%microzooplankton_carbon))
        call self%register_state_variable(self%id_coccolithophore_nitrogen, 'coccolithophore_nitrogen', 'mmol m-3', 'Coccolithophore Nitrogen', get_tracer_init_value(tracer_ids%coccolithophore_nitrogen))
        call self%register_state_variable(self%id_coccolithophore_carbon, 'coccolithophore_carbon', 'mmol m-3', 'Coccolithophore Carbon', get_tracer_init_value(tracer_ids%coccolithophore_carbon))
        call self%register_state_variable(self%id_coccolithophore_chlorophyll, 'coccolithophore_chlorophyll', 'mg m-3', 'Coccolithophore Chlorophyll', get_tracer_init_value(tracer_ids%coccolithophore_chlorophyll))
        call self%register_state_variable(self%id_phaeocystis_nitrogen, 'phaeocystis_nitrogen', 'mmol m-3', 'Phaeocystis Nitrogen', get_tracer_init_value(tracer_ids%phaeocystis_nitrogen))
        call self%register_state_variable(self%id_phaeocystis_carbon, 'phaeocystis_carbon', 'mmol m-3', 'Phaeocystis Carbon', get_tracer_init_value(tracer_ids%phaeocystis_carbon))
        call self%register_state_variable(self%id_phaeocystis_chlorophyll, 'phaeocystis_chlorophyll', 'mg m-3', 'Phaeocystis Chlorophyll', get_tracer_init_value(tracer_ids%phaeocystis_chlorophyll))

    end subroutine initialize

    subroutine do(self, _ARGUMENTS_DO_)

       use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
       use recom_config

       use recom_declarations, only: wp, decaybenthos, vertrespp, vertrespc, vertrespd, &
               vertrespn, vertcalcif, vertdocexp, vertdocexc, vertdocexd, vertdocexn, vertaggp, &
               vertaggc, vertaggd, vertaggn, vertcalcdiss, vertrespmicro, vertrespmacro, &
               vertrespmeso, vertgrazmicro_p, vertgrazmicro_c, vertgrazmicro_d, vertgrazmicro_n, &
               vertgrazmicro_tot, vertgrazmacro_det, vertgrazmacro_mic, vertgrazmacro_det2, &
               vertgrazmacro_mes, vertgrazmacro_p, vertgrazmacro_c, vertgrazmacro_d, &
               vertgrazmacro_n, vertgrazmeso_det, vertgrazmeso_mic, vertgrazmeso_det2, &
               vertgrazmeso_p, vertgrazmeso_c, vertgrazmeso_d, vertgrazmeso_n, vertgrazmeso_tot, &
               vertgrazmacro_tot, aggregationrate, arrfunc, arrfunczoo2, ca, calc_diss, &
               calc_diss2, calc_diss_ben, calc_loss_agg, calc_loss_gra, calc_loss_gra2, &
               calc_loss_gra3, calcification, chl2c, chl2c_cocco, chl2c_dia, chl2c_phaeo, &
               chl2c_plast, chl2c_plast_cocco, chl2c_plast_dia, chl2c_plast_phaeo, chl2n, &
               chl2n_cocco, chl2n_dia, chl2n_phaeo, chl_lower, chl_upper, chlave, chlsynth, &
               chlsynth_cocco, chlsynth_dia, chlsynth_phaeo, co3_sat, coccoco2, &
               cphot, cphot_cocco, cphot_dia, cphot_phaeo, diaco2, fcoccon, fcoccon2, fcoccon3, &
               fdetn, fdetn2, fdetz2n, fdetz2n2, fdian, fdian2, fdian3, felimitfac, fhetn, &
               fmiczoon, fmiczoon2, food, food2, food3, foodsq, foodsq2, foodsq3, fphaeon, &
               fphaeon2, fphaeon3, fphyn, fphyn2, fphyn3, grazeff, grazingflux, grazingflux2, &
               grazingflux_cocco, grazingflux_cocco2, grazingflux_cocco3, grazingflux_det, &
               grazingflux_det2, grazingflux_detz2, grazingflux_detz22, grazingflux_dia, &
               grazingflux_dia2, grazingflux_dia3, grazingflux_het2, grazingflux_miczoo, &
               grazingflux_miczoo2, grazingflux_phaeo, grazingflux_phaeo2, grazingflux_phaeo3, &
               grazingflux_phy, grazingflux_phy2, grazingflux_phy3, hetlossflux, hetrespflux, &
               is_3zoo2det, is_coccos, kappastar, kdzlower, kdzupper, kochl, kochl_cocco, &
               kochl_phaeo, limitfacn, limitfacn_cocco, limitfacn_dia, limitfacn_phaeo, &
               lowerlight, mesfecalloss_c, mesfecalloss_n, miczoolossflux, miczoorespflux, &
               n_assim_cocco, n_assim_dia, n_assim_phaeo, o2func, parave, phaeoco2, phyco2, &
               phyresprate, phyresprate_cocco, phyresprate_dia, phyresprate_phaeo, pmax, &
               pmax_dia, pmax_phaeo, q10_mes, q10_mes_res, q10_mic, q10_mic_res, qlimitfac, &
               qlimitfactmp, qsic, qsin, quota, quota_cocco, quota_dia, quota_phaeo, &
               kochl_dia, grazingflux3, limitfacsi, n_assim, pmax_cocco, recip_res_zoo22, &
               recipbiostep, recipdet, recipdet2, recipquota, recipquota_cocco, recipquota_dia, &
               recipquota_phaeo, recipqzoo, recipqzoo2, recipqzoo3, reminsit, rtloc, rtref, &
               temp_cocco, temp_diatoms, temp_phaeo, temp_phyto, tiny_c, tiny_c_c, tiny_c_d, &
               tiny_c_p, tiny_n, tiny_n_c, tiny_n_d, tiny_n_p, tiny_si, v_cm, varpzcocco, &
               varpzcocco2, varpzcocco3, varpzdet, varpzdet2, varpzdetz2, varpzdetz22, varpzdia, &
               varpzdia2, varpzdia3, varpzhet, varpzmiczoo, varpzmiczoo2, varpzphaeo, &
               varpzphaeo3, varpzphy, varpzphy2, varpzphy3, zoo2fecalloss_c, zoo2fecalloss_n, &
               zoo2lossflux, zoo2respflux, vttemp_phyto, vttemp_diatoms, vttemp_cocco, &
               h_depth, vtphyco2, vtdiaco2, vtcoccoco2, vtphaeoco2, vtcphotliglim_phyto, &
               vtcphot_phyto, vtcphotliglim_diatoms, vtcphot_diatoms, vtcphotliglim_cocco, &
               vtcphot_cocco, vtcphotliglim_phaeo, vtcphot_phaeo, vtsi_assimdia, &
               vertnppn, vertnppd, vertnppc, vertnppp, vertgppn, vertgppd, vertgppc, vertgppc, &
               vertgppp, vertgppp, vertnnan, vertnnan, vertnnad, vertnnad, vertnnac, vertnnac, &
               vertnnap, vertnnap, vertchldegn, vertchldegn, vertchldegd, vertchldegd, &
               si_assim, varpzphaeo2, vttemp_phaeo, vertchldegc, vertchldegc, vertchldegp, &
               vertchldegp

       use recom_ciso
       use recom_sms_module, only: recom_limiter, iron_chemistry

       use recom_locvar, only: betad_depth, co2_depth, co3_depth, dpos, fco2_depth, &
                grazingfluxcarbon_mes, grazingfluxcarbonzoo2, hco3_depth, kspc_depth, locbenthos, &
                locriverdoc, logfile_outfreq_30, logfile_outfreq_7, omegaa_depth, omegac_depth, &
                pco2_depth, p_depth, ph_depth, picpocco2, picpocn, picpoctemp, res_zoo2_a, &
                res_zoo2_f, rhosw_depth, tempis_depth

        use mvars, only: vars_sprac
        use recom_extra, only: krill_resp

       implicit none

       class (type_awi_recom_sms), intent(in) :: self
       _DECLARE_ARGUMENTS_DO_

        real(kind=wp) :: dt_d !< Size of time steps [day]
        real(kind=wp) :: dt_b !< Size of time steps [day]

        real(kind=wp) :: recip_hetN_plus !< MB's addition to heterotrophic respiration

        real(kind=wp) :: recip_res_het
        real(kind=wp) :: Sink_Vel
        real(kind=wp) :: aux
        integer :: step, mstep, daynew

        real(kind=wp) :: Patm_depth(1)

        real(kind=wp) :: REcoM_T_depth(1)
        real(kind=wp) :: REcoM_S_depth(1)
        real(kind=wp) :: REcoM_DIC_depth(1)
        real(kind=wp) :: REcoM_Alk_depth(1)
        real(kind=wp) :: REcoM_Si_depth(1)
        real(kind=wp) :: REcoM_Phos_depth(1)
        real(kind=wp) :: mocsy_step_per_day

        real(kind=wp) :: Loc_slp
        real(kind=wp) :: dt
        real(kind=wp) :: Temp
        real(kind=wp) :: Sali_depth
        real(kind=wp) :: depth
        real(kind=wp) :: SurfSR
        real(kind=wp) :: Latd(1) ! latitude in degree

        real(kind=wp), dimension(1) :: CO2_watercolumn !< [mol/m3]
        real(kind=wp), dimension(1) :: pH_watercolumn !< on total scale
        real(kind=wp), dimension(1) :: pCO2_watercolumn !< [uatm]
        real(kind=wp), dimension(1) :: HCO3_watercolumn !< [mol/m3]
        real(kind=wp), dimension(1) :: CO3_watercolumn !< [mol/m3]
        real(kind=wp), dimension(1) :: OmegaC_watercolumn
        real(kind=wp), dimension(1) :: kspc_watercolumn
        real(kind=wp), dimension(1) :: rhoSW_watercolumn

        real(kind=wp) :: &
                DIN, & ! [mmol/m3] Dissolved inorganic nitrogen
                DIC, & ! [mmol/m3] Dissolved inorganic carbon
                Alk, & ! [mmol/m3] Total alkalinity
                PhyN, & ! [mmol/m3] Phytoplankton nitrogen (small)
                PhyC, & ! [mmol/m3] Phytoplankton carbon (small)
                PhyChl, & ! [mg/m3] Phytoplankton chlorophyll
                DetN, & ! [mmol/m3] Detrital nitrogen
                DetC, & ! [mmol/m3] Detrital carbon
                HetN, & ! [mmol/m3] Heterotroph nitrogen
                HetC, & ! [mmol/m3] Heterotroph carbon
                DON, & ! [mmol/m3] Dissolved organic nitrogen
                EOC, & ! [mmol/m3] Extracellular organic carbon
                DiaN, & ! [mmol/m3] Diatom nitrogen
                DiaC, & ! [mmol/m3] Diatom carbon
                DiaChl, & ! [mg/m3] Diatom chlorophyll
                DiaSi, & ! [mmol/m3] Diatom silicate
                DetSi, & ! [mmol/m3] Detrital silicate
                Si, & ! [mmol/m3] Dissolved silicate
                Fe, & ! [mmol/m3] Dissolved iron
                PhyCalc, & ! [mmol/m3] Phytoplankton calcite
                DetCalc, & ! [mmol/m3] Detrital calcite
                FreeFe, & ! [mmol/m3] Free iron
                O2 ! [mmol/m3] Dissolved oxygen

        real(kind=wp) :: &
                CoccoN, & ! [mmol/m3] Coccolithophore nitrogen
                CoccoC, & ! [mmol/m3] Coccolithophore carbon
                CoccoChl, & ! [mg/m3] Coccolithophore chlorophyll
                PhaeoN, & ! [mmol/m3] Phaeocystis nitrogen
                PhaeoC, & ! [mmol/m3] Phaeocystis carbon
                PhaeoChl ! [mg/m3] Phaeocystis chlorophyll

        real(kind=wp) :: &
                Zoo2N, & ! [mmol/m3] Zooplankton type 2 nitrogen
                Zoo2C, & ! [mmol/m3] Zooplankton type 2 carbon
                DetZ2N, & ! [mmol/m3] Zooplankton detritus nitrogen
                DetZ2C, & ! [mmol/m3] Zooplankton detritus carbon
                DetZ2Si, & ! [mmol/m3] Zooplankton detritus silicate
                DetZ2Calc, & ! [mmol/m3] Zooplankton detritus calcite
                MicZooN, & ! [mmol/m3] Microzooplankton nitrogen
                MicZooC ! [mmol/m3] Microzooplankton carbon

        real(kind=wp), dimension(bgc_num) :: sms, state !< Source-Minus-Sinks term

        mstep = 1

       _LOOP_BEGIN_

        _GET_HORIZONTAL_(self%id_surface_air_pressure, Loc_slp)

        tiny_N = tiny_chl / chl2N_max
        tiny_C = tiny_N / NCmax
        tiny_N_d = tiny_chl / chl2N_max_d
        tiny_C_d = tiny_N_d / NCmax_d
        tiny_Si = tiny_C_d / SiCmax

        if (enable_coccos) then
            tiny_N_c = tiny_chl / chl2N_max_c
            tiny_C_c = tiny_N_c / NCmax_c
            tiny_N_p = tiny_chl / chl2N_max_p
            tiny_C_p = tiny_N_p / NCmax_p
        end if

        recip_res_het = 1.d0 / res_het
        Patm_depth = Loc_slp / Pa2atm
        dt_d = dt / SecondsPerDay
        dt_b = dt_d / real(biostep)
        rTref = real(one) / recom_Tref

        ! Collect variable states
        _GET_(self%id_dissolved_inorganic_nitrogen, state(idin))
        _GET_(self%id_dissolved_inorganic_carbon, state(idic))
        _GET_(self%id_alkalinity, state(ialk))
        _GET_(self%id_phytoplankton_nitrogen, state(iphyn))
        _GET_(self%id_phytoplankton_carbon, state(iphyc))
        _GET_(self%id_phytoplankton_chlorophyll, state(ipchl))
        _GET_(self%id_detrital_nitrogen, state(idetn))
        _GET_(self%id_detrital_carbon, state(idetc))
        _GET_(self%id_heterotroph_nitrogen, state(ihetn))
        _GET_(self%id_heterotroph_carbon, state(ihetc))
        _GET_(self%id_dissolved_organic_nitrogen, state(idon))
        _GET_(self%id_dissolved_organic_carbon, state(idoc))
        _GET_(self%id_diatom_nitrogen, state(idian))
        _GET_(self%id_diatom_carbon, state(idiac))
        _GET_(self%id_diatom_chlorophyll, state(idchl))
        _GET_(self%id_diatom_silica, state(idiasi))
        _GET_(self%id_detrital_silica, state(idetsi))
        _GET_(self%id_silica, state(isi))
        _GET_(self%id_iron, state(ife))
        _GET_(self%id_phytoplankton_calcite, state(iphycal))
        _GET_(self%id_detrital_calcite, state(idetcal))
        _GET_(self%id_oxygen, state(ioxy))

        kdzUpper = 0.d0
        if (any(abs(sms(:)) <= tiny)) sms(:) = zero
        DIN = max(tiny, state(idin) + sms(idin))
        DIC = max(tiny, state(idic) + sms(idic))
        ALK = max(tiny, state(ialk) + sms(ialk))
        PhyN = max(tiny_N, state(iphyn) + sms(iphyn))
        PhyC = max(tiny_C, state(iphyc) + sms(iphyc))
        PhyChl = max(tiny_chl, state(ipchl) + sms(ipchl))
        DetN = max(tiny, state(idetn) + sms(idetn))
        DetC = max(tiny, state(idetc) + sms(idetc))
        HetN = max(tiny, state(ihetn) + sms(ihetn))
        HetC = max(tiny, state(ihetc) + sms(ihetc))
        DON = max(tiny, state(idon) + sms(idon))
        EOC = max(tiny, state(idoc) + sms(idoc))
        DiaN = max(tiny_N_d, state(idian) + sms(idian))
        DiaC = max(tiny_C_d, state(idiac) + sms(idiac))
        DiaChl = max(tiny_chl, state(idchl) + sms(idchl))
        DiaSi = max(tiny_si, state(idiasi) + sms(idiasi))
        Si = max(tiny, state(isi) + sms(isi))
        DetSi = max(tiny, state(idetsi) + sms(idetsi))
        Fe = max(tiny, state(ife) + sms(ife))
        PhyCalc = max(tiny, state(iphycal) + sms(iphycal))
        DetCalc = max(tiny, state(idetcal) + sms(idetcal))
        O2 = max(tiny, state(ioxy) + sms(ioxy))

        FreeFe = zero

        if (enable_coccos) then
            CoccoN = max(tiny_N_c, state(icocn) + sms(icocn))
            CoccoC = max(tiny_C_c, state(icocc) + sms(icocc))
            CoccoChl = max(tiny_chl, state(icchl) + sms(icchl))
            PhaeoN = max(tiny_N_p, state(iphan) + sms(iphan))
            PhaeoC = max(tiny_C_p, state(iphac) + sms(iphac))
            PhaeoChl = max(tiny_chl, state(iphachl) + sms(iphachl))
        end if

        if (enable_3zoo2det) then
            Zoo2N = max(tiny, state(izoo2n) + sms(izoo2n))
            Zoo2C = max(tiny, state(izoo2c) + sms(izoo2c))
            MicZooN = max(tiny, state(imiczoon) + sms(imiczoon))
            MicZooC = max(tiny, state(imiczooc) + sms(imiczooc))

            DetZ2N = max(tiny, state(idetz2n) + sms(idetz2n))
            DetZ2C = max(tiny, state(idetz2c) + sms(idetz2c))
            DetZ2Si = max(tiny, state(idetz2si) + sms(idetz2si))
            DetZ2Calc = max(tiny, state(idetz2calc) + sms(idetz2calc))
        end if

        _GET_(self%id_temperature, Temp)
        _GET_(self%id_practical_salinity, Sali_depth)

        REcoM_T_depth = max(2.d0, Temp) ! Apply minimum
        REcoM_T_depth = min(REcoM_T_depth, 40.d0) ! Apply maximum
        REcoM_S_depth = max(21.d0, Sali_depth) ! Apply minimum
        REcoM_S_depth = min(REcoM_S_depth, 43.d0) ! Apply maximum
        REcoM_DIC_depth = max(tiny * 1e-3, state(idic) * 1e-3 + sms(idic) * 1e-3)
        REcoM_Alk_depth = max(tiny * 1e-3, state(ialk) * 1e-3 + sms(ialk) * 1e-3)
        REcoM_Si_depth = max(tiny * 1e-3, state(isi) * 1e-3 + sms(isi) * 1e-3)
        REcoM_Phos_depth = max(tiny * 1e-3, state(idin) * 1e-3 + sms(idin) * 1e-3) &
                / 16.d0

        quota = PhyN / PhyC
        recipquota = real(one) / quota
        Chl2C = PhyChl / PhyC
        Chl2N = PhyChl / PhyN
        CHL2C_plast = Chl2C * (quota / (quota - NCmin))
        quota_dia = DiaN / DiaC
        recipQuota_dia = real(one) / quota_dia
        Chl2C_dia = DiaChl / DiaC
        Chl2N_dia = DiaChl / DiaN
        CHL2C_plast_dia = Chl2C_dia * (quota_dia / (quota_dia - NCmin_d))
        qSiC = DiaSi / DiaC
        qSiN = DiaSi / DiaN

        if (enable_coccos) then
            quota_cocco = CoccoN / CoccoC
            recipQuota_cocco = real(one) / quota_cocco
            Chl2C_cocco = CoccoChl / CoccoC
            Chl2N_cocco = CoccoChl / CoccoN
            CHL2C_plast_cocco = Chl2C_cocco * (quota_cocco / (quota_cocco - NCmin_c))
            quota_phaeo = PhaeoN / PhaeoC
            recipQuota_phaeo = real(one) / quota_phaeo
            Chl2C_phaeo = PhaeoChl / PhaeoC
            Chl2N_phaeo = PhaeoChl / PhaeoN
            CHL2C_plast_phaeo = Chl2C_phaeo * (quota_phaeo / (quota_phaeo - NCmin_p))
        end if

        recipQZoo = HetC / HetN
        recip_hetN_plus = 1.d0 / (HetN + tiny_het)

        if (Grazing_detritus) then
            recipDet = DetC / DetN
        end if

        if (enable_3zoo2det) then
            recipQZoo2 = Zoo2C / Zoo2N
            recipQZoo3 = MicZooC / MicZooN
            if (Grazing_detritus) then
                recipDet2 = DetZ2C / DetZ2N
            end if
        end if

        if (ciso) then
            DIC_13 = max(tiny, state(idic_13) + sms(idic_13))
            PhyC_13 = max(tiny_C, state(iphyc_13) + sms(iphyc_13))
            DetC_13 = max(tiny, state(idetc_13) + sms(idetc_13))
            HetC_13 = max(tiny, state(ihetc_13) + sms(ihetc_13))
            EOC_13 = max(tiny, state(idoc_13) + sms(idoc_13))
            DiaC_13 = max(tiny_C, state(idiac_13) + sms(idiac_13))
            PhyCalc_13 = max(tiny, state(iphycal_13) + sms(iphycal_13))
            DetCalc_13 = max(tiny, state(idetcal_13) + sms(idetcal_13))

            calc_diss_13 = alpha_dcal_13 * calc_diss

            quota_13 = PhyN / PhyC_13
            recipQuota_13 = real(one) / quota_13
            quota_dia_13 = DiaN / DiaC_13
            recipQuota_dia_13 = real(one) / quota_dia_13
            recipQZoo_13 = HetC_13 / HetN

            if (ciso_14) then
                DIC_14 = max(tiny, state(idic_14) + sms(idic_14))

                if (ciso_organic_14) then
                    PhyC_14 = max(tiny_C, state(iphyc_14) + sms(iphyc_14))
                    DetC_14 = max(tiny, state(idetc_14) + sms(idetc_14))
                    HetC_14 = max(tiny, state(ihetc_14) + sms(ihetc_14))
                    EOC_14 = max(tiny, state(idoc_14) + sms(idoc_14))
                    DiaC_14 = max(tiny_C, state(idiac_14) + sms(idiac_14))
                    PhyCalc_14 = max(tiny, state(iphycal_14) + sms(iphycal_14))
                    DetCalc_14 = max(tiny, state(idetcal_14) + sms(idetcal_14))

                    calc_diss_14 = alpha_dcal_14 * calc_diss

                    quota_14 = PhyN / PhyC_14
                    recipQuota_14 = real(one) / quota_14
                    quota_dia_14 = DiaN / DiaC_14
                    recipQuota_dia_14 = real(one) / quota_dia_14
                    recipQZoo_14 = HetC_14 / HetN
                end if ! ciso_organic_14
            end if ! ciso_14
        end if ! ciso

        rTloc = real(one) / (Temp + C2K)
        arrFunc = exp(-Ae * (rTloc - rTref))

        if (enable_coccos) then
            Temp_phyto = exp(ord_phy + expon_phy * Temp)
            VTTemp_phyto(1) = Temp_phyto ! Store for diagnostics
            Temp_diatoms = exp(ord_d + expon_d * Temp)
            VTTemp_diatoms(1) = Temp_diatoms ! Store for diagnostics

            if (Temp < 5.0) then
                Temp_cocco = tiny
            else
                Temp_cocco = exp(ord_cocco + expon_cocco * Temp)
                Temp_cocco = max(Temp_cocco, tiny) ! Ensure positive values
            end if
            VTTemp_cocco(1) = Temp_cocco ! Store for diagnostics
            Temp_phaeo = uopt_phaeo &
                    * ((Tmax_phaeo - Temp) / (Tmax_phaeo - Topt_phaeo)) ** beta_phaeo &
                    * exp(-beta_phaeo * (Topt_phaeo - Temp) / (Tmax_phaeo - Topt_phaeo))
            Temp_phaeo = max(Temp_phaeo, tiny) ! Ensure positive values
            VTTemp_phaeo(1) = Temp_phaeo ! Store for diagnostics
        end if


        if (enable_3zoo2det) then
            arrFuncZoo2 = exp(t1_zoo2 / t2_zoo2 - t1_zoo2 * rTloc) / &
                    (1.0 + exp(t3_zoo2 / t4_zoo2 - t3_zoo2 * rTloc))

            q10_mes = 1.0242 ** (Temp) ! Mesozooplankton metabolism
            q10_mic = 1.04 ** (Temp) ! Microzooplankton metabolism
            q10_mes_res = 1.0887 ** (Temp) ! Mesozooplankton respiration
            q10_mic_res = 1.0897 ** (Temp) ! Microzooplankton respiration
        end if

        reminSiT = max(0.023d0 * 2.6d0 ** ((Temp - 10.0) / 10.0), reminSi)
        O2Func = 1.d0

        if (O2dep_remin) then
            O2Func = O2 / (k_o2_remin + O2)
        end if

        ! TODO: we should implement REcoM's light attenuation subroutine
        _GET_HORIZONTAL_(self%id_swr0, SurfSR)
        _GET_HORIZONTAL_(self%id_latitude, Latd(1))
        _GET_(self%id_par, PARave)
        _GET_(self%id_depth, depth)

        ! Convert model depth coordinate to positive depth for MOCSY
        ! Model convention: zF(k) is negative (e.g., -100 m)
        ! MOCSY convention: depth is positive (e.g., 100 m)
        dpos(1) = -depth

        ! Calculate update frequencies based on model time step
        mocsy_step_per_day = 1.0 / dt_b
        logfile_outfreq_7 = int(mocsy_step_per_day * 7.0) ! Steps in 7 days
        logfile_outfreq_30 = int(mocsy_step_per_day * 30.0) ! Steps in 30 days

        if (mstep == 1 .or. &
            PARave > 0.01 * SurfSR .and. mod(mstep, logfile_outfreq_7) == 0 .or. &
            PARave < 0.01 * SurfSR .and. mod(mstep, logfile_outfreq_30) == 0) then

            ! Monthly updates in deep waters (low biological activity)
            call vars_sprac(ph_depth, pco2_depth, fco2_depth, co2_depth, hco3_depth, &
                    co3_depth, &
                    OmegaA_depth, OmegaC_depth, kspc_depth, BetaD_depth, &
                    rhoSW_depth, p_depth, tempis_depth, &
                    REcoM_T_depth, REcoM_S_depth, REcoM_Alk_depth, REcoM_DIC_depth, &
                    REcoM_Si_depth, REcoM_Phos_depth, Patm_depth, dpos, Latd, Nmocsy, &
                    optCON='mol/m3', optT='Tpot   ', optP='m ', optB='u74', &
                    optK1K2='l  ', optKf='dg', optGAS='Pinsitu', optS='Sprc')

            ! Update water column arrays with new carbonate chemistry
            CO2_watercolumn(1) = co2_depth(1)
            pH_watercolumn(1) = ph_depth(1)
            pCO2_watercolumn(1) = pco2_depth(1)
            HCO3_watercolumn(1) = hco3_depth(1)
            CO3_watercolumn(1) = co3_depth(1)
            OmegaC_watercolumn(1) = OmegaC_depth(1)
            kspc_watercolumn(1) = kspc_depth(1)
            rhoSW_watercolumn(1) = rhoSW_depth(1)
        end if

        h_depth(1) = 10.d0 ** (-ph_depth(1))
        PhyCO2 = a_co2_phy * HCO3_watercolumn(1) * Cunits &
                / (b_co2_phy + HCO3_watercolumn(1) * Cunits) &
                - exp(-c_co2_phy * CO2_watercolumn(1) * Cunits) &
                - d_co2_phy * 10.d0 ** (-pH_watercolumn(1))
        PhyCO2 = min(PhyCO2, 3.d0) ! Upper limit: maximum 3x enhancement
        PhyCO2 = max(0.d0, PhyCO2) ! Lower limit: prevent negative growth response
        VTPhyCO2(1) = PhyCO2
        DiaCO2 = a_co2_dia * HCO3_watercolumn(1) * Cunits &
                / (b_co2_dia + HCO3_watercolumn(1) * Cunits) &
                - exp(-c_co2_dia * CO2_watercolumn(1) * Cunits) &
                - d_co2_dia * 10.d0 ** (-pH_watercolumn(1))
        DiaCO2 = min(DiaCO2, 3.d0) ! Upper limit: 3x enhancement
        DiaCO2 = max(0.d0, DiaCO2) ! Lower limit: no negative effect
        VTDiaCO2(1) = DiaCO2

        if (enable_coccos) then
            CoccoCO2 = a_co2_cocco * HCO3_watercolumn(1) * Cunits &
                    / (b_co2_cocco + HCO3_watercolumn(1) * Cunits) &
                    - exp(-c_co2_cocco * CO2_watercolumn(1) * Cunits) &
                    - d_co2_cocco * 10.d0 ** (-pH_watercolumn(1))
            CoccoCO2 = min(CoccoCO2, 3.d0) ! Upper limit: 3x enhancement
            CoccoCO2 = max(0.d0, CoccoCO2) ! Lower limit: no negative effect
            VTCoccoCO2(1) = CoccoCO2
            PhaeoCO2 = a_co2_phaeo * HCO3_watercolumn(1) * Cunits &
                    / (b_co2_phaeo + HCO3_watercolumn(1) * Cunits) &
                    - exp(-c_co2_phaeo * CO2_watercolumn(1) * Cunits) &
                    - d_co2_phaeo * 10.d0 ** (-pH_watercolumn(1))
            PhaeoCO2 = min(PhaeoCO2, 3.d0) ! Upper limit: 3× enhancement
            PhaeoCO2 = max(0.d0, PhaeoCO2) ! Lower limit: no negative effect
            VTPhaeoCO2(1) = PhaeoCO2
        end if

        Sink_Vel = Vdet_a * abs(depth) + Vdet
        if (OmegaC_diss) then
            Ca = (0.02128d0 / 40.078d0) * Sali_depth / 1.80655d0
            CO3_sat = (kspc_watercolumn(1) / Ca) * rhoSW_watercolumn(1)
            calc_diss = calc_diss_omegac &
                    * max(zero, (1.0 - (CO3_watercolumn(1) / CO3_sat))) ** (calc_diss_exp)
            if (enable_3zoo2det) then
                calc_diss2 = calc_diss ! Fast-sinking detritus
            end if
            calc_diss_ben = calc_diss ! Benthic detritus
        else
            calc_diss = calc_diss_rate * Sink_Vel / 20.d0
            if (enable_3zoo2det) then
                calc_diss2 = calc_diss_rate2 * Sink_Vel / 20.d0
            end if
            calc_diss_ben = calc_diss_rate * Sink_Vel / 20.d0
        end if

        qlimitFac = recom_limiter(NMinSlope, NCmin, quota)
        feLimitFac = Fe / (k_Fe + Fe)
        qlimitFac = min(qlimitFac, feLimitFac)
        if (enable_coccos) then
            pMax = qlimitFac * Temp_phyto
        else
            pMax = P_cm * qlimitFac * arrFunc
        end if
        qlimitFac = recom_limiter(NMinSlope, NCmin_d, quota_dia)
        qlimitFacTmp = recom_limiter(SiMinSlope, SiCmin, qSiC)
        qlimitFac = min(qlimitFac, qlimitFacTmp)
        feLimitFac = Fe / (k_Fe_d + Fe)
        qlimitFac = min(qlimitFac, feLimitFac)
        if (enable_coccos) then
            pMax_dia = qlimitFac * Temp_diatoms
        else
            pMax_dia = P_cm_d * qlimitFac * arrFunc
        end if


        if (enable_coccos) then
            qlimitFac = recom_limiter(NMinSlope, NCmin_c, quota_cocco)
            feLimitFac = Fe / (k_Fe_c + Fe)
            qlimitFac = min(qlimitFac, feLimitFac)
            pMax_cocco = qlimitFac * Temp_cocco
            qlimitFac = recom_limiter(NMinSlope, NCmin_p, quota_phaeo)
            feLimitFac = Fe / (k_Fe_p + Fe)
            qlimitFac = min(qlimitFac, feLimitFac)
            pMax_phaeo = qlimitFac * Temp_phaeo
        end if

        if (pMax < tiny .or. ieee_is_nan(PARave) .or. ieee_is_nan(CHL2C)) then
            Cphot = zero
        else
            Cphot = pMax * (1.0d0 - exp(-alfa * Chl2C * PARave / pMax))
            VTCphotLigLim_phyto(1) = Cphot / pMax
            if (CO2lim) Cphot = Cphot * PhyCO2
        end if
        if (Cphot < tiny) Cphot = zero
        VTCphot_phyto(1) = Cphot

        if (pMax_dia < tiny .or. ieee_is_nan(PARave) .or. ieee_is_nan(CHL2C_dia)) then
            Cphot_dia = zero
        else
            Cphot_dia = pMax_dia * (1.0 - exp(-alfa_d * Chl2C_dia * PARave / pMax_dia))
            VTCphotLigLim_diatoms(1) = Cphot_dia / pMax_dia
            if (CO2lim) Cphot_dia = Cphot_dia * DiaCO2
        end if
        if (Cphot_dia < tiny) Cphot_dia = zero
        VTCphot_diatoms(1) = Cphot_dia

        if (enable_coccos) then
            if (pMax_cocco < tiny .or. ieee_is_nan(PARave) .or. ieee_is_nan(CHL2C_cocco)) &
                    then
                Cphot_cocco = zero
            else
                Cphot_cocco = pMax_cocco &
                        * (1.0 - exp(-alfa_c * Chl2C_cocco * PARave / pMax_cocco))
                VTCphotLigLim_cocco(1) = Cphot_cocco / pMax_cocco
                if (CO2lim) Cphot_cocco = Cphot_cocco * CoccoCO2
            end if
            if (Cphot_cocco < tiny) Cphot_cocco = zero
            VTCphot_cocco(1) = Cphot_cocco

            if (pMax_phaeo < tiny .or. ieee_is_nan(PARave) .or. ieee_is_nan(CHL2C_phaeo)) &
                    then
                Cphot_phaeo = zero
            else
                Cphot_phaeo = pMax_phaeo &
                        * (1.0 - exp(-alfa_p * Chl2C_phaeo * PARave / pMax_phaeo))
                VTCphotLigLim_phaeo(1) = Cphot_phaeo / pMax_phaeo
                if (CO2lim) Cphot_phaeo = Cphot_phaeo * PhaeoCO2
            end if
            if (Cphot_phaeo < tiny) Cphot_phaeo = zero
            VTCphot_phaeo(1) = Cphot_phaeo
        end if

        KOchl = deg_Chl ! Small phytoplankton
        KOchl_dia = deg_Chl_d ! Diatoms

        if (enable_coccos) then
            KOchl_cocco = deg_Chl_c ! Coccolithophores
            KOchl_phaeo = deg_Chl_p ! Phaeocystis
        end if

        if (use_photodamage) then
            if (pMax < tiny .or. ieee_is_nan(PARave) .or. ieee_is_nan(CHL2C_plast)) then
                KOchl = deg_Chl * 0.1d0
            else
                KOchl = deg_Chl * (real(one) - exp(-alfa * CHL2C_plast * PARave / pMax))
                KOchl = max((deg_Chl * 0.1d0), KOchl)
            end if

            if (pMax_dia < tiny .or. ieee_is_nan(PARave) .or. &
                    ieee_is_nan(CHL2C_plast_dia)) then
                KOchl_dia = deg_Chl_d * 0.1d0
            else
                KOchl_dia = deg_Chl_d &
                        * (real(one) - exp(-alfa_d * CHL2C_plast_dia * PARave / pMax_dia))
                KOchl_dia = max((deg_Chl_d * 0.1d0), KOchl_dia)
            end if

            if (enable_coccos) then
                if (pMax_cocco < tiny .or. ieee_is_nan(PARave) .or. &
                        ieee_is_nan(CHL2C_plast_cocco)) then
                    KOchl_cocco = deg_Chl_c * 0.1d0
                else
                    KOchl_cocco = deg_Chl_c &
                            * (real(one) &
                            - exp(-alfa_c * CHL2C_plast_cocco * PARave / pMax_cocco))
                    KOchl_cocco = max((deg_Chl_c * 0.1d0), KOchl_cocco)
                end if

                if (pMax_phaeo < tiny .or. ieee_is_nan(PARave) .or. &
                        ieee_is_nan(CHL2C_plast_phaeo)) then
                    KOchl_phaeo = deg_Chl_p * 0.1d0
                else
                    KOchl_phaeo = deg_Chl_p &
                            * (real(one) &
                            - exp(-alfa_p * CHL2C_plast_phaeo * PARave / pMax_phaeo))
                    KOchl_phaeo = max((deg_Chl_p * 0.1d0), KOchl_phaeo)
                end if
            end if ! enable_coccos
        end if ! use_photodamage

        if (ieee_is_nan(KOchl)) then
            print*, 'ERROR: KOchl is NaN'
            print*, '  deg_Chl =', deg_Chl
            print*, '  alfa =', alfa
            print*, '  CHL2C_plast =', CHL2C_plast
            print*, '  PARave =', PARave
            print*, '  pMax =', pMax
            stop
        end if

        if (ieee_is_nan(KOchl_dia)) then
            print*, 'ERROR: KOchl_dia is NaN'
            print*, '  deg_Chl_d =', deg_Chl_d
            print*, '  alfa_d =', alfa_d
            print*, '  CHL2C_plast_dia =', CHL2C_plast_dia
            print*, '  PARave =', PARave
            print*, '  pMax_dia =', pMax_dia
            stop
        end if

        if (enable_coccos) then
            if (ieee_is_nan(KOchl_cocco)) then
                print*, 'ERROR: KOchl_cocco is NaN'
                print*, '  deg_Chl_c =', deg_Chl_c
                print*, '  alfa_c =', alfa_c
                print*, '  CHL2C_plast_cocco =', CHL2C_plast_cocco
                print*, '  PARave =', PARave
                print*, '  pMax_cocco =', pMax_cocco
                stop
            end if

            if (ieee_is_nan(KOchl_phaeo)) then
                print*, 'ERROR: KOchl_phaeo is NaN'
                print*, '  deg_Chl_p =', deg_Chl_p
                print*, '  alfa_p =', alfa_p
                print*, '  CHL2C_plast_phaeo =', CHL2C_plast_phaeo
                print*, '  PARave =', PARave
                print*, '  pMax_phaeo =', pMax_phaeo
                stop
            end if
        end if

        V_cm = V_cm_fact
        limitFacN = recom_limiter(NMaxSlope, quota, NCmax)
        N_assim = V_cm * pMax * NCuptakeRatio * limitFacN * (DIN / (DIN + k_din))

        V_cm = V_cm_fact_d
        limitFacN_dia = recom_limiter(NMaxSlope, quota_dia, NCmax_d)
        N_assim_dia = V_cm * pMax_dia * NCUptakeRatio_d * limitFacN_dia * DIN &
                / (DIN + k_din_d)

        if (enable_coccos) then
            V_cm = V_cm_fact_c
            limitFacN_cocco = recom_limiter(NMaxSlope, quota_cocco, NCmax_c)
            N_assim_cocco = V_cm * pMax_cocco * NCUptakeRatio_c * limitFacN_cocco * &
                    DIN / (DIN + k_din_c)

            V_cm = V_cm_fact_p
            limitFacN_phaeo = recom_limiter(NMaxSlope, quota_phaeo, NCmax_p)
            N_assim_phaeo = V_cm * pMax_phaeo * NCUptakeRatio_p * limitFacN_phaeo * &
                    DIN / (DIN + k_din_p)
        end if

        limitFacSi = recom_limiter(SiMaxSlope, qSiC, SiCmax) * limitFacN_dia
        if (.not.enable_coccos) then
            Si_assim = V_cm_fact_d * P_cm_d * arrFunc * SiCUptakeRatio * limitFacSi * &
                    Si / (Si + k_si)
        else
            Si_assim = V_cm_fact_d * Temp_diatoms * SiCUptakeRatio * limitFacSi * &
                    Si / (Si + k_si)
            VTSi_assimDia(1) = Si_assim
        end if

        freeFe = iron_chemistry(Fe, totalligand, ligandStabConst)

        chlSynth = zero
        if (PARave >= tiny .and. .not.ieee_is_nan(PARave)) then
            chlSynth = N_assim * Chl2N_max * &
                    min(real(one), Cphot / (alfa * Chl2C * PARave))
        end if

        ChlSynth_dia = zero
        if (PARave >= tiny .and. .not.ieee_is_nan(PARave)) then
            ChlSynth_dia = N_assim_dia * Chl2N_max_d * &
                    min(real(one), Cphot_dia / (alfa_d * Chl2C_dia * PARave))
        end if

        if (enable_coccos) then
            ChlSynth_cocco = zero
            if (PARave >= tiny .and. .not.ieee_is_nan(PARave)) then
                ChlSynth_cocco = N_assim_cocco * Chl2N_max_c * &
                        min(real(one), Cphot_cocco / (alfa_c * Chl2C_cocco * PARave))
            end if

            ChlSynth_phaeo = zero
            if (PARave >= tiny .and. .not.ieee_is_nan(PARave)) then
                ChlSynth_phaeo = N_assim_phaeo * Chl2N_max_p * &
                        min(real(one), Cphot_phaeo / (alfa_p * Chl2C_phaeo * PARave))
            end if
        end if

        phyRespRate = res_phy * limitFacN + biosynth * N_assim
        phyRespRate_dia = res_phy_d * limitFacN_dia + biosynth * N_assim_dia + &
                biosynthSi * Si_assim
        if (enable_coccos) then
            phyRespRate_cocco = res_phy_c * limitFacN_cocco + biosynth * N_assim_cocco
            phyRespRate_phaeo = res_phy_p * limitFacN_phaeo + biosynth * N_assim_phaeo
        end if

        if (REcoM_Grazing_Variable_Preference) then
            aux = pzPhy * PhyN + pzDia * DiaN

            if (Grazing_detritus) then
                aux = aux + pzDet * DetN
            end if

            if (enable_3zoo2det) then
                if (Grazing_detritus) aux = aux + pzDetZ2 * DetZ2N ! Fast-sinking detritus
                aux = aux + pzMicZoo * MicZooN ! Microzooplankton
            end if

            if (enable_coccos) then
                aux = aux + pzCocco * CoccoN + pzPhaeo * PhaeoN
            end if

            varpzPhy = (pzPhy * PhyN) / aux
            varpzDia = (pzDia * DiaN) / aux

            if (Grazing_detritus) then
                varpzDet = (pzDet * DetN) / aux
            end if

            if (enable_3zoo2det) then
                if (Grazing_detritus) varpzDetZ2 = (pzDetZ2 * DetZ2N) / aux
                varpzMicZoo = (pzMicZoo * MicZooN) / aux
            end if

            if (enable_coccos) then
                varpzCocco = (pzCocco * CoccoN) / aux
                varpzPhaeo = (pzPhaeo * PhaeoN) / aux
            end if

            fPhyN = varpzPhy * PhyN
            fDiaN = varpzDia * DiaN

            if (Grazing_detritus) then
                fDetN = varpzDet * DetN
            end if

            if (enable_3zoo2det) then
                if (Grazing_detritus) fDetZ2N = varpzDetZ2 * DetZ2N
                fMicZooN = varpzMicZoo * MicZooN
            end if

            if (enable_coccos) then
                fCoccoN = varpzCocco * CoccoN
                fPhaeoN = varpzPhaeo * PhaeoN
            end if

        else

            fPhyN = pzPhy * PhyN
            fDiaN = pzDia * DiaN

            if (Grazing_detritus) then
                fDetN = pzDet * DetN
            end if

            if (enable_3zoo2det) then
                if (Grazing_detritus) fDetZ2N = pzDetZ2 * DetZ2N
                fMicZooN = pzMicZoo * MicZooN
            end if

            if (enable_coccos) then
                fCoccoN = pzCocco * CoccoN
                fPhaeoN = pzPhaeo * PhaeoN
            end if

        end if ! REcoM_Grazing_Variable_Preference

        food = fPhyN + fDiaN

        if (Grazing_detritus) then
            food = food + fDetN
        end if

        if (enable_3zoo2det) then
            if (Grazing_detritus) food = food + fDetZ2N
            food = food + fMicZooN
        end if

        if (enable_coccos) then
            food = food + fCoccoN + fPhaeoN
        end if

        foodsq = food ** 2

        if (enable_3zoo2det) then
            grazingFlux = (Graz_max * foodsq) / (epsilonr + foodsq) * HetN * q10_mes
        else
            grazingFlux = (Graz_max * foodsq) / (epsilonr + foodsq) * HetN * arrFunc
        end if

        grazingFlux_phy = grazingFlux * fPhyN / food
        grazingFlux_Dia = grazingFlux * fDiaN / food

        if (Grazing_detritus) then
            grazingFlux_Det = grazingFlux * fDetN / food
        end if

        if (enable_3zoo2det) then
            if (Grazing_detritus) grazingFlux_DetZ2 = grazingFlux * fDetZ2N / food
            grazingFlux_miczoo = grazingFlux * fMicZooN / food
        end if

        if (enable_coccos) then
            grazingFlux_Cocco = grazingFlux * fCoccoN / food
            grazingFlux_Phaeo = grazingFlux * fPhaeoN / food
        end if


        grazEff = gfin + 1.0 / (0.2 * food + 2.0)

        grazingFluxcarbon_mes = (grazingFlux_phy * recipQuota * grazEff) + &
                (grazingFlux_Dia * recipQuota_Dia * grazEff)

        if (Grazing_detritus) then
            grazingFluxcarbon_mes = grazingFluxcarbon_mes + &
                    (grazingFlux_Det * recipDet * grazEff)
        end if

        if (enable_3zoo2det) then
            if (Grazing_detritus) then
                grazingFluxcarbon_mes = grazingFluxcarbon_mes + &
                        (grazingFlux_DetZ2 * recipDet2 * grazEff)
            end if
            grazingFluxcarbon_mes = grazingFluxcarbon_mes + &
                    (grazingFlux_miczoo * recipQZoo3 * grazEff)
        end if

        if (enable_coccos) then
            grazingFluxcarbon_mes = grazingFluxcarbon_mes + &
                    (grazingFlux_Cocco * recipQuota_Cocco * grazEff) + &
                    (grazingFlux_Phaeo * recipQuota_phaeo * grazEff)
        end if

        if (enable_3zoo2det) then
            if (REcoM_Grazing_Variable_Preference) then

                aux = pzPhy2 * PhyN + pzDia2 * DiaN + pzHet * HetN + pzMicZoo2 * MicZooN

                if (Grazing_detritus) then
                    aux = aux + pzDet2 * DetN + pzDetZ22 * DetZ2N
                end if

                if (enable_coccos) then
                    aux = aux + pzCocco2 * CoccoN + pzPhaeo2 * PhaeoN
                end if

                varpzPhy2 = (pzPhy2 * PhyN) / aux
                varpzDia2 = (pzDia2 * DiaN) / aux
                varpzMicZoo2 = (pzMicZoo2 * MicZooN) / aux
                varpzHet = (pzHet * HetN) / aux

                if (enable_coccos) then
                    varpzCocco2 = (pzCocco2 * CoccoN) / aux
                    varpzPhaeo2 = (pzPhaeo2 * PhaeoN) / aux
                end if

                if (Grazing_detritus) then
                    varpzDet2 = (pzDet2 * DetN) / aux
                    varpzDetZ22 = (pzDetZ22 * DetZ2N) / aux
                end if

                fPhyN2 = varpzPhy2 * PhyN
                fDiaN2 = varpzDia2 * DiaN
                fMicZooN2 = varpzMicZoo2 * MicZooN
                fHetN = varpzHet * HetN

                if (enable_coccos) then
                    fCoccoN2 = varpzCocco2 * CoccoN
                    fPhaeoN2 = varpzPhaeo2 * PhaeoN
                end if

                if (Grazing_detritus) then
                    fDetN2 = varpzDet2 * DetN
                    fDetZ2N2 = varpzDetZ22 * DetZ2N
                end if

            else

                fPhyN2 = pzPhy2 * PhyN
                fDiaN2 = pzDia2 * DiaN
                fMicZooN2 = pzMicZoo2 * MicZooN
                fHetN = pzHet * HetN

                if (enable_coccos) then
                    fCoccoN2 = pzCocco2 * CoccoN
                    fPhaeoN2 = pzPhaeo2 * PhaeoN
                end if

                if (Grazing_detritus) then
                    fDetN2 = pzDet2 * DetN
                    fDetZ2N2 = pzDetZ22 * DetZ2N
                end if

            end if ! REcoM_Grazing_Variable_Preference


            food2 = fPhyN2 + fDiaN2 + fHetN + fMicZooN2

            if (Grazing_detritus) then
                food2 = food2 + fDetN2 + fDetZ2N2
            end if

            if (enable_coccos) then
                food2 = food2 + fCoccoN2 + fPhaeoN2
            end if

            foodsq2 = food2 ** 2
            grazingFlux2 = (Graz_max2 * foodsq2) / (epsilon2 + foodsq2) * Zoo2N &
                    * arrFuncZoo2


            grazingFlux_phy2 = (grazingFlux2 * fPhyN2) / food2
            grazingFlux_Dia2 = (grazingFlux2 * fDiaN2) / food2
            grazingFlux_miczoo2 = (grazingFlux2 * fMicZooN2) / food2
            grazingFlux_het2 = (grazingFlux2 * fHetN) / food2

            if (enable_coccos) then
                grazingFlux_Cocco2 = (grazingFlux2 * fCoccoN2) / food2
                grazingFlux_Phaeo2 = (grazingFlux2 * fPhaeoN2) / food2
            end if

            if (Grazing_detritus) then
                grazingFlux_Det2 = (grazingFlux2 * fDetN2) / food2
                grazingFlux_DetZ22 = (grazingFlux2 * fDetZ2N2) / food2
            end if


            grazingFluxcarbonzoo2 = (grazingFlux_phy2 * recipQuota * grazEff2) + &
                    (grazingFlux_Dia2 * recipQuota_Dia * grazEff2) + &
                    (grazingFlux_het2 * recipQZoo * grazEff2) + &
                    (grazingFlux_miczoo2 * recipQZoo3 * grazEff2)

            if (Grazing_detritus) then
                grazingFluxcarbonzoo2 = grazingFluxcarbonzoo2 + &
                        (grazingFlux_Det2 * recipDet * grazEff2) + &
                        (grazingFlux_DetZ22 * recipDet2 * grazEff2)
            end if

            if (enable_coccos) then
                grazingFluxcarbonzoo2 = grazingFluxcarbonzoo2 + &
                        (grazingFlux_Cocco2 * recipQuota_Cocco * grazEff2) + &
                        (grazingFlux_Phaeo2 * recipQuota_Phaeo * grazEff2)
            end if

        end if ! enable_3zoo2det

        if (enable_3zoo2det) then
            if (REcoM_Grazing_Variable_Preference) then

                aux = pzPhy3 * PhyN + pzDia3 * DiaN

                if (enable_coccos) then
                    aux = aux + pzCocco3 * CoccoN + pzPhaeo3 * PhaeoN
                end if

                varpzPhy3 = (pzPhy3 * PhyN) / aux
                varpzDia3 = (pzDia3 * DiaN) / aux

                if (enable_coccos) then
                    varpzCocco3 = (pzCocco3 * CoccoN) / aux
                    varpzPhaeo3 = (pzPhaeo3 * PhaeoN) / aux
                end if

                fPhyN3 = varpzPhy3 * PhyN
                fDiaN3 = varpzDia3 * DiaN

                if (enable_coccos) then
                    fCoccoN3 = varpzCocco3 * CoccoN
                    fPhaeoN3 = varpzPhaeo3 * PhaeoN
                end if

            else

                fPhyN3 = pzPhy3 * PhyN
                fDiaN3 = pzDia3 * DiaN

                if (enable_coccos) then
                    fCoccoN3 = pzCocco3 * CoccoN
                    fPhaeoN3 = pzPhaeo3 * PhaeoN
                end if

            end if ! REcoM_Grazing_Variable_Preference


            food3 = fPhyN3 + fDiaN3

            if (enable_coccos) then
                food3 = food3 + fCoccoN3 + fPhaeoN3
            end if

            foodsq3 = food3 ** 2
            grazingFlux3 = (Graz_max3 * foodsq3) / (epsilon3 + foodsq3) * MicZooN * q10_mic

            grazingFlux_phy3 = (grazingFlux3 * fPhyN3) / food3
            grazingFlux_Dia3 = (grazingFlux3 * fDiaN3) / food3

            if (enable_coccos) then
                grazingFlux_Cocco3 = (grazingFlux3 * fCoccoN3) / food3
                grazingFlux_Phaeo3 = (grazingFlux3 * fPhaeoN3) / food3
            end if
        end if ! enable_3zoo2det

        if (het_resp_noredfield) then
            if (enable_3zoo2det) then
                HetRespFlux = res_het * q10_mes_res * HetC
            else
                HetRespFlux = res_het * arrFunc * HetC
            end if
        else
            HetRespFlux = recip_res_het * arrFunc * (HetC * recip_hetN_plus - redfield) &
                    * HetC
            HetRespFlux = max(zero, HetRespFlux)
        end if

        if (ciso) then
            HetRespFlux_13 = HetRespFlux * HetC_13 / HetC
            if (ciso_14 .and. ciso_organic_14) then
                HetRespFlux_14 = HetRespFlux * HetC_14 / HetC
            end if
        end if

        hetLossFlux = loss_het * HetN * HetN

        if (enable_3zoo2det) then
            _GET_GLOBAL_(self%id_days_since_start_of_year, daynew)
            call krill_resp(daynew, Latd(1))
            if ((grazingFluxcarbonzoo2 / Zoo2C) <= 0.1) then
                res_zoo2_f = 0.1 * (grazingFluxcarbonzoo2 / Zoo2C * 100.0)
            else
                res_zoo2_f = 1.0
            end if
            recip_res_zoo22 = res_zoo2 * (1.0 + res_zoo2_f + res_zoo2_a)
            Zoo2RespFlux = recip_res_zoo22 * Zoo2C
            Zoo2LossFlux = loss_zoo2 * zoo2N * zoo2N
            MicZooRespFlux = res_miczoo * q10_mic_res * MicZooC
            MicZooLossFlux = loss_miczoo * MicZooN * MicZooN
        end if ! enable_3zoo2det

        if (enable_3zoo2det) then
            Zoo2fecalloss_n = fecal_rate_n * grazingFlux2 ! Nitrogen
            Zoo2fecalloss_c = fecal_rate_c * grazingFluxcarbonzoo2 ! Carbon
            mesfecalloss_n = fecal_rate_n_mes * grazingFlux ! Nitrogen
            mesfecalloss_c = fecal_rate_c_mes * grazingFluxcarbon_mes ! Carbon
        end if ! enable_3zoo2det

        if (diatom_mucus) then
            qlimitFac = recom_limiter(NMinSlope, NCmin_d, quota_dia)
            qlimitFacTmp = recom_limiter(SiMinSlope, SiCmin, qSiC)
            qlimitFac = min(qlimitFac, qlimitFacTmp) ! Most limiting nutrient
            feLimitFac = Fe / (k_Fe_d + Fe)
            qlimitFac = min(qlimitFac, feLimitFac) ! Most limiting of all nutrients
            aggregationrate = agg_PP * (1.0 - qlimitFac) * DiaN
        else
            aggregationrate = agg_PP * DiaN
        end if

        aggregationrate = aggregationrate + agg_PD * DetN + agg_PP * PhyN

        if (enable_3zoo2det) then
            aggregationrate = aggregationrate + agg_PD * DetZ2N
        end if

        if (enable_coccos) then
            aggregationrate = aggregationrate + agg_PP * CoccoN + agg_PP * PhaeoN
        end if

        if (enable_coccos) then
            if (Temp < 10.6) then
                PICPOCtemp = 0.104d0 * Temp - 0.108d0
            else
                PICPOCtemp = 1.0d0
            end if

            PICPOCtemp = max(tiny, PICPOCtemp)
            PICPOCCO2 = a_co2_calc * HCO3_watercolumn(1) * Cunits &
                    / (b_co2_calc + HCO3_watercolumn(1) * Cunits) &
                    - exp(-c_co2_calc * CO2_watercolumn(1) * Cunits) &
                    - d_co2_calc * 10.d0 ** (-pH_watercolumn(1))
            PICPOCCO2 = min(PICPOCCO2, 3.d0) ! Upper limit (April 2022 modification)
            PICPOCCO2 = max(0.d0, PICPOCCO2) ! Lower limit (July 2022 modification)
            PICPOCN = -0.31 * (DIN / (DIN + k_din_c)) + 1.31
            PICPOCN = max(tiny, PICPOCN) ! Prevent negative values
            calcification = 1.d0 * Cphot_cocco * CoccoC * PICPOCtemp * PICPOCN

            if (CO2lim) then
                calcification = calcification * PICPOCCO2
            end if
        else
            calcification = calc_prod_ratio * Cphot * PhyC
        end if

        calc_loss_agg = aggregationrate * PhyCalc
        if (enable_coccos) then
            aux = recipQuota_Cocco / (CoccoC + tiny) * PhyCalc
            calc_loss_gra = grazingFlux_Cocco * aux
            if (enable_3zoo2det) then
                calc_loss_gra2 = grazingFlux_Cocco2 * aux
                calc_loss_gra3 = grazingFlux_Cocco3 * aux
            end if
        else
            aux = recipQuota / (PhyC + tiny) * PhyCalc
            calc_loss_gra = grazingFlux_phy * aux
            if (enable_3zoo2det) then
                calc_loss_gra2 = grazingFlux_phy2 * aux
                calc_loss_gra3 = grazingFlux_phy3 * aux
            end if
        end if

        if (ciso) then
            calcification_13 = calcification * alpha_calc_13
            calc_loss_agg_13 = aggregationRate * PhyCalc_13
            calc_loss_gra_13 = grazingFlux_phy * recipQuota_13 / (PhyC_13 + tiny) &
                    * PhyCalc_13
            if (ciso_14 .and. ciso_organic_14) then
                calcification_14 = calc_prod_ratio * Cphot * PhyC_14 * alpha_calc_14
                calc_loss_agg_14 = aggregationRate * PhyCalc_14
                calc_loss_gra_14 = grazingFlux_phy * recipQuota_14 / (PhyC_14 + tiny) &
                        * PhyCalc_14
            end if ! ciso_14 .and. ciso_organic_14
        end if ! ciso


        sms(idin) = ( &
                -N_assim * PhyC & ! Small phytoplankton
                - N_assim_Dia * DiaC & ! Diatoms
                - N_assim_Cocco * CoccoC * is_coccos & ! Coccolithophores
                - N_assim_Phaeo * PhaeoC * is_coccos & ! Phaeocystis
                + rho_N * arrFunc * O2Func * DON & ! Temperature and O2 dependent
                ) * dt_b + sms(idin)


        sms(idic) = ( &
                -Cphot * PhyC & ! Small phytoplankton
                - Cphot_Dia * DiaC & ! Diatoms
                - Cphot_Cocco * CoccoC * is_coccos & ! Coccolithophores
                - Cphot_Phaeo * PhaeoC * is_coccos & ! Phaeocystis
                - calcification &
                + phyRespRate * PhyC & ! Small phytoplankton
                + phyRespRate_Dia * DiaC & ! Diatoms
                + phyRespRate_Cocco * CoccoC * is_coccos & ! Coccolithophores
                + phyRespRate_Phaeo * PhaeoC * is_coccos & ! Phaeocystis
                + rho_C1 * arrFunc * O2Func * EOC & ! Temperature and O2 dependent
                + HetRespFlux & ! Mesozooplankton
                + Zoo2RespFlux * is_3zoo2det & ! Macrozooplankton
                + MicZooRespFlux * is_3zoo2det & ! Microzooplankton
                + calc_diss * DetCalc & ! Slow-sinking calcite
                + calc_loss_gra * calc_diss_guts & ! Mesozooplankton gut
                + calc_loss_gra2 * calc_diss_guts * is_3zoo2det & ! Macrozooplankton gut
                + calc_loss_gra3 * calc_diss_guts * is_3zoo2det & ! Microzooplankton gut
                + calc_diss2 * DetZ2Calc * is_3zoo2det & ! Fast-sinking calcite
                ) * dt_b + sms(idic)

        sms(ialk) = ( &
                +1.0625 * N_assim * PhyC & ! Small phytoplankton
                + 1.0625 * N_assim_Dia * DiaC & ! Diatoms
                + 1.0625 * N_assim_Cocco * CoccoC * is_coccos & ! Coccolithophores
                + 1.0625 * N_assim_Phaeo * PhaeoC * is_coccos & ! Phaeocystis

                - 1.0625 * rho_N * arrFunc * O2Func * DON &
                + 2.d0 * calc_diss * DetCalc & ! Slow-sinking calcite
                + 2.d0 * calc_loss_gra * calc_diss_guts & ! Mesozooplankton gut
                + 2.d0 * calc_loss_gra2 * calc_diss_guts * is_3zoo2det &
                + 2.d0 * calc_loss_gra3 * calc_diss_guts * is_3zoo2det &
                + 2.d0 * calc_diss2 * DetZ2Calc * is_3zoo2det & ! Fast-sinking calcite

                - 2.d0 * calcification &
                ) * dt_b + sms(ialk)


        sms(iphyn) = ( &
                +N_assim * PhyC &
                - lossN * limitFacN * PhyN & ! DON excretion (N:C regulated)
                - aggregationRate * PhyN & ! Aggregation to detritus
                - grazingFlux_phy & ! Mesozooplankton
                - grazingFlux_phy2 * is_3zoo2det & ! Macrozooplankton
                - grazingFlux_phy3 * is_3zoo2det & ! Microzooplankton
                ) * dt_b + sms(iphyn)


        sms(iphyc) = ( &
                +Cphot * PhyC & ! Gross photosynthesis
                - phyRespRate * PhyC & ! Autotrophic respiration
                - lossC * limitFacN * PhyC & ! DOC excretion (regulated)
                - aggregationRate * PhyC & ! Aggregation to detritus
                - grazingFlux_phy * recipQuota & ! Mesozooplankton (N->C)
                - grazingFlux_phy2 * recipQuota * is_3zoo2det & ! Macrozooplankton
                - grazingFlux_phy3 * recipQuota * is_3zoo2det & ! Microzooplankton
                ) * dt_b + sms(iphyc)


        sms(ipchl) = ( &
                +chlSynth * PhyC & ! Photoacclimation

                - KOchl * PhyChl & ! Natural degradation
                - aggregationRate * PhyChl & ! Aggregation to detritus
                - grazingFlux_phy * Chl2N & ! Mesozooplankton
                - grazingFlux_phy2 * Chl2N * is_3zoo2det & ! Macrozooplankton
                - grazingFlux_phy3 * Chl2N * is_3zoo2det & ! Microzooplankton
                ) * dt_b + sms(ipchl)


        if (Grazing_detritus) then
            if (enable_3zoo2det) then
                sms(idetn) = ( &
                        +grazingFlux_phy3 - grazingFlux_phy3 * grazEff3 &
                        + grazingFlux_dia3 - grazingFlux_dia3 * grazEff3 & ! Diatoms

                        + (grazingFlux_Cocco3 - grazingFlux_Cocco3 * grazEff3) * is_coccos &

                        + (grazingFlux_Phaeo3 - grazingFlux_Phaeo3 * grazEff3) * is_coccos &
                        + aggregationRate * PhyN &
                        + aggregationRate * DiaN &
                        + aggregationRate * CoccoN * is_coccos &
                        + aggregationRate * PhaeoN * is_coccos &
                        + miczooLossFlux &
                        - grazingFlux_Det * grazEff & ! Mesozooplankton
                        - grazingFlux_Det2 * grazEff2 & ! Macrozooplankton
                        - reminN * arrFunc * O2Func * DetN & ! Bacterial decomposition
                        ) * dt_b + sms(idetn)
            else
                sms(idetn) = ( &
                        +grazingFlux_phy - grazingFlux_phy * grazEff & ! Small phytoplankton
                        + grazingFlux_dia - grazingFlux_dia * grazEff & ! Diatoms
                        + (grazingFlux_Cocco - grazingFlux_Cocco * grazEff) * is_coccos &
                        + (grazingFlux_Phaeo - grazingFlux_Phaeo * grazEff) * is_coccos &
                        + aggregationRate * PhyN &
                        + aggregationRate * DiaN &
                        + aggregationRate * CoccoN * is_coccos &
                        + aggregationRate * PhaeoN * is_coccos &
                        + hetLossFlux &
                        - grazingFlux_Det * grazEff & ! Mesozooplankton
                        - grazingFlux_Det2 * grazEff2 & ! Macrozooplankton
                        - reminN * arrFunc * O2Func * DetN &
                        ) * dt_b + sms(idetn)
            end if
        else
            if (enable_3zoo2det) then
                sms(idetn) = ( &
                        +grazingFlux_phy3 & ! Microzooplankton->small phyto
                        + grazingFlux_dia3 & ! Microzooplankton->diatoms
                        + grazingFlux_Cocco3 * is_coccos & ! Microzooplankton->coccoliths
                        + grazingFlux_Phaeo3 * is_coccos & ! Microzooplankton->Phaeocystis
                        + aggregationRate * PhyN &
                        + aggregationRate * DiaN &
                        + aggregationRate * CoccoN * is_coccos &
                        + aggregationRate * PhaeoN * is_coccos &
                        + miczooLossFlux &
                        - grazingFlux * grazEff3 &
                        - reminN * arrFunc * O2Func * DetN &
                        ) * dt_b + sms(idetn)

            else
                sms(idetn) = ( &
                        +grazingFlux_phy & ! Mesozooplankton->small phyto
                        + grazingFlux_dia & ! Mesozooplankton->diatoms
                        + grazingFlux_Cocco * is_coccos & ! Mesozooplankton->coccoliths
                        + grazingFlux_Phaeo * is_coccos & ! Mesozooplankton->Phaeocystis
                        + aggregationRate * PhyN &
                        + aggregationRate * DiaN &
                        + aggregationRate * CoccoN * is_coccos &
                        + aggregationRate * PhaeoN * is_coccos &
                        + hetLossFlux &
                        - grazingFlux * grazEff &
                        - reminN * arrFunc * O2Func * DetN &
                        ) * dt_b + sms(idetn)
            end if
        end if

        if (Grazing_detritus) then
            if (enable_3zoo2det) then
                sms(idetc) = ( &
                        +grazingFlux_phy3 * recipQuota * (1.d0 - grazEff3) & ! Small phyto
                        + grazingFlux_Dia3 * recipQuota_Dia * (1.d0 - grazEff3) & ! Diatoms
                        + grazingFlux_Cocco3 * recipQuota_Cocco * (1.d0 - grazEff3) * &
                        is_coccos & ! Coccoliths
                        + grazingFlux_Phaeo3 * recipQuota_Phaeo * (1.d0 - grazEff3) * &
                        is_coccos & ! Phaeocystis
                        + aggregationRate * PhyC &
                        + aggregationRate * DiaC &
                        + aggregationRate * CoccoC * is_coccos &
                        + aggregationRate * PhaeoC * is_coccos &
                        + miczooLossFlux * recipQZoo3 & ! N->C conversion
                        - grazingFlux_Det * recipDet * grazEff & ! Mesozooplankton
                        - grazingFlux_Det2 * recipDet * grazEff2 & ! Macrozooplankton
                        - reminC * arrFunc * O2Func * DetC & ! Bacterial respiration
                        ) * dt_b + sms(idetc)

            else
                sms(idetc) = ( &
                        +grazingFlux_phy * recipQuota * (1.d0 - grazEff) &
                        + grazingFlux_Dia * recipQuota_Dia * (1.d0 - grazEff) &
                        + grazingFlux_Cocco * recipQuota_Cocco * (1.d0 - grazEff) * &
                        is_coccos &
                        + grazingFlux_Phaeo * recipQuota_Phaeo * (1.d0 - grazEff) * &
                        is_coccos &
                        + aggregationRate * phyC &
                        + aggregationRate * DiaC &
                        + aggregationRate * CoccoC * is_coccos &
                        + aggregationRate * PhaeoC * is_coccos &
                        + hetLossFlux * recipQZoo &
                        - grazingFlux_Det * recipDet * grazEff &
                        - reminC * arrFunc * O2Func * DetC &
                        ) * dt_b + sms(idetc)
            end if
        else
            if (enable_3zoo2det) then
                sms(idetc) = ( &
                        +grazingFlux_phy3 * recipQuota * (1.d0 - grazEff3) &
                        + grazingFlux_Dia3 * recipQuota_Dia * (1.d0 - grazEff3) &
                        + grazingFlux_Cocco3 * recipQuota_Cocco * (1.d0 - grazEff3) * &
                        is_coccos &
                        + grazingFlux_Phaeo3 * recipQuota_Phaeo * (1.d0 - grazEff3) * &
                        is_coccos &
                        + aggregationRate * PhyC &
                        + aggregationRate * DiaC &
                        + aggregationRate * CoccoC * is_coccos &
                        + aggregationRate * PhaeoC * is_coccos &
                        + miczooLossFlux * recipQZoo3 &
                        - reminC * arrFunc * O2Func * DetC &
                        ) * dt_b + sms(idetc)
            else
                sms(idetc) = ( &
                        +grazingFlux_phy * recipQuota * (1.d0 - grazEff) &
                        + grazingFlux_Dia * recipQuota_Dia * (1.d0 - grazEff) &
                        + grazingFlux_Cocco * recipQuota_Cocco * (1.d0 - grazEff) * &
                        is_coccos &
                        + grazingFlux_Phaeo * recipQuota_Phaeo * (1.d0 - grazEff) * &
                        is_coccos &
                        + aggregationRate * phyC &
                        + aggregationRate * DiaC &
                        + aggregationRate * CoccoC * is_coccos &
                        + aggregationRate * PhaeoC * is_coccos &
                        + hetLossFlux * recipQZoo &
                        - reminC * arrFunc * O2Func * DetC &
                        ) * dt_b + sms(idetc)
            end if
        end if


        sms(ihetn) = ( &
                +grazingFlux * grazEff & ! Assimilated N
                - grazingFlux_het2 * is_3zoo2det & ! Predation by macrozooplankton
                - Mesfecalloss_n * is_3zoo2det & ! Fecal pellets
                - hetLossFlux & ! Mortality
                - lossN_z * HetN & ! DON excretion
                ) * dt_b + sms(ihetn)


        if (Grazing_detritus) then
            sms(ihetc) = ( &
                    +grazingFlux_phy * recipQuota * grazEff & ! Small phytoplankton
                    + grazingFlux_Dia * recipQuota_Dia * grazEff & ! Diatoms
                    + grazingFlux_Cocco * recipQuota_Cocco * grazEff * is_coccos &
                    + grazingFlux_Phaeo * recipQuota_Phaeo * grazEff * is_coccos &
                    + grazingFlux_miczoo * recipQZoo3 * grazEff * is_3zoo2det &
                    + grazingFlux_DetZ2 * recipDet2 * grazEff * is_3zoo2det &
                    + grazingFlux_Det * recipDet * grazEff & ! Slow-sinking detritus
                    - grazingFlux_het2 * recipQZoo * is_3zoo2det & ! Predation
                    - Mesfecalloss_c * is_3zoo2det & ! Fecal pellets
                    - hetLossFlux * recipQZoo & ! Mortality
                    - lossC_z * HetC & ! DOC excretion
                    - hetRespFlux & ! Respiration to CO2
                    ) * dt_b + sms(ihetc)

        else
            sms(ihetc) = ( &
                    +grazingFlux_phy * recipQuota * grazEff &
                    + grazingFlux_Dia * recipQuota_Dia * grazEff &
                    + grazingFlux_Cocco * recipQuota_Cocco * grazEff * is_coccos &
                    + grazingFlux_Phaeo * recipQuota_Phaeo * grazEff * is_coccos &
                    + grazingFlux_miczoo * recipQZoo3 * grazEff * is_3zoo2det &
                    - grazingFlux_het2 * recipQZoo * is_3zoo2det &
                    - Mesfecalloss_c * is_3zoo2det &
                    - hetLossFlux * recipQZoo &
                    - lossC_z * HetC &
                    - hetRespFlux &
                    ) * dt_b + sms(ihetc)
        end if


        if (enable_3zoo2det) then
            sms(izoo2n) = ( &
                    +grazingFlux2 * grazEff2 & ! Assimilated N
                    - Zoo2LossFlux & ! Mortality
                    - lossN_z2 * Zoo2N & ! DON excretion
                    - Zoo2fecalloss_n & ! Fecal pellets
                    ) * dt_b + sms(izoo2n)

            if (Grazing_detritus) then
                sms(izoo2c) = ( &
                        +grazingFlux_phy2 * recipQuota * grazEff2 & ! Small phytoplankton
                        + grazingFlux_Dia2 * recipQuota_Dia * grazEff2 & ! Diatoms
                        + grazingFlux_Cocco2 * recipQuota_Cocco * grazEff2 * is_coccos &
                        + grazingFlux_Phaeo2 * recipQuota_Phaeo * grazEff2 * is_coccos &
                        + grazingFlux_het2 * recipQZoo * grazEff2 &
                        + grazingFlux_miczoo2 * recipQZoo3 * grazEff2 & ! Microzooplankton
                        + grazingFlux_Det2 * recipDet * grazEff2 & ! Slow-sinking detritus
                        + grazingFlux_DetZ22 * recipDet2 * grazEff2 &
                        - zoo2LossFlux * recipQZoo2 & ! Mortality
                        - lossC_z2 * Zoo2C & ! DOC excretion
                        - Zoo2RespFlux & ! Respiration to CO2
                        - Zoo2fecalloss_c & ! Fecal pellets
                        ) * dt_b + sms(izoo2c)

            else
                sms(izoo2c) = ( &
                        +grazingFlux_phy2 * recipQuota * grazEff2 &
                        + grazingFlux_Dia2 * recipQuota_Dia * grazEff2 &
                        + grazingFlux_Cocco2 * recipQuota_Cocco * grazEff2 * is_coccos &
                        + grazingFlux_Phaeo2 * recipQuota_Phaeo * grazEff2 * is_coccos &
                        + grazingFlux_het2 * recipQZoo * grazEff2 &
                        + grazingFlux_miczoo2 * recipQZoo3 * grazEff2 &
                        - zoo2LossFlux * recipQZoo2 &
                        - lossC_z2 * Zoo2C &
                        - Zoo2RespFlux &
                        - Zoo2fecalloss_c &
                        ) * dt_b + sms(izoo2c)
            end if

            sms(imiczoon) = ( &
                    +grazingFlux3 * grazEff3 & ! Assimilated N
                    - grazingFlux_miczoo & ! Predation by mesozooplankton
                    - grazingFlux_miczoo2 & ! Predation by macrozooplankton
                    - MicZooLossFlux & ! Mortality
                    - lossN_z3 * MicZooN & ! DON excretion
                    ) * dt_b + sms(imiczoon)


            sms(imiczooc) = ( &
                    +grazingFlux_phy3 * recipQuota * grazEff3 & ! Small phytoplankton
                    + grazingFlux_Dia3 * recipQuota_Dia * grazEff3 & ! Diatoms
                    + grazingFlux_Cocco3 * recipQuota_Cocco * grazEff3 * is_coccos &
                    + grazingFlux_Phaeo3 * recipQuota_Phaeo * grazEff3 * is_coccos &
                    - MicZooLossFlux * recipQZoo3 & ! Mortality
                    - grazingFlux_miczoo * recipQZoo3 & ! Predation by mesozooplankton
                    - grazingFlux_miczoo2 * recipQZoo3 & ! Predation by macrozooplankton
                    - lossC_z3 * MicZooC & ! DOC excretion
                    - MicZooRespFlux & ! Respiration to CO2
                    ) * dt_b + sms(imiczooc)
        end if


        if (enable_3zoo2det) then
            if (Grazing_detritus) then
                sms(idetz2n) = ( &
                        +grazingFlux_phy2 * (1.d0 - grazEff2) & ! Small phytoplankton
                        + grazingFlux_dia2 * (1.d0 - grazEff2) & ! Diatoms
                        + grazingFlux_Cocco * (1.d0 - grazEff) * is_coccos &
                        + grazingFlux_Cocco2 * (1.d0 - grazEff2) * is_coccos &
                        + grazingFlux_Phaeo * (1.d0 - grazEff) * is_coccos &
                        + grazingFlux_Phaeo2 * (1.d0 - grazEff2) * is_coccos &
                        + grazingFlux_het2 * (1.d0 - grazEff2) &
                        + grazingFlux_miczoo2 * (1.d0 - grazEff2) & ! Microzooplankton
                        + grazingFlux_phy * (1.d0 - grazEff) & ! Small phytoplankton
                        + grazingFlux_dia * (1.d0 - grazEff) & ! Diatoms
                        + grazingFlux_miczoo * (1.d0 - grazEff) & ! Microzooplankton
                        + Zoo2LossFlux & ! Macrozooplankton
                        + hetLossFlux & ! Mesozooplankton
                        + Zoo2fecalloss_n & ! Macrozooplankton
                        + Mesfecalloss_n & ! Mesozooplankton
                        - grazingFlux_DetZ2 * grazEff & ! Mesozooplankton
                        - grazingFlux_DetZ22 * grazEff2 & ! Macrozooplankton
                        - reminN * arrFunc * O2Func * DetZ2N & ! Bacterial decomposition
                        ) * dt_b + sms(idetz2n)

            else
                sms(idetz2n) = ( &
                        +grazingFlux_phy2 & ! All grazing enters detritus
                        + grazingFlux_dia2 &
                        + grazingFlux_Cocco * is_coccos &
                        + grazingFlux_Cocco2 * is_coccos &
                        + grazingFlux_Phaeo * is_coccos &
                        + grazingFlux_Phaeo2 * is_coccos &
                        + grazingFlux_het2 &
                        + grazingFlux_miczoo2 &
                        - grazingFlux2 * grazEff2 & ! Minus assimilated portion
                        + grazingFlux_phy &
                        + grazingFlux_dia &
                        + grazingFlux_miczoo &
                        - grazingFlux * grazEff & ! Minus assimilated portion
                        + Zoo2LossFlux &
                        + hetLossFlux &
                        + Zoo2fecalloss_n &
                        + Mesfecalloss_n &
                        - reminN * arrFunc * O2Func * DetZ2N &
                        ) * dt_b + sms(idetz2n)
            end if

            if (Grazing_detritus) then
                sms(idetz2c) = ( &
                        +grazingFlux_phy2 * recipQuota * (1.d0 - grazEff2) & ! Small phyto
                        + grazingFlux_Dia2 * recipQuota_Dia * (1.d0 - grazEff2) & ! Diatoms
                        + grazingFlux_Cocco * recipQuota_Cocco * (1.d0 - grazEff) * &
                        is_coccos & ! Coccoliths (meso)
                        + grazingFlux_Cocco2 * recipQuota_Cocco * (1.d0 - grazEff2) * &
                        is_coccos & ! Coccoliths (macro)
                        + grazingFlux_Phaeo * recipQuota_Phaeo * (1.d0 - grazEff) * &
                        is_coccos & ! Phaeocystis (meso)
                        + grazingFlux_Phaeo2 * recipQuota_Phaeo * (1.d0 - grazEff2) * &
                        is_coccos & ! Phaeocystis (macro)
                        + grazingFlux_het2 * recipQZoo * (1.d0 - grazEff2) &
                        + grazingFlux_miczoo2 * recipQZoo3 * (1.d0 - grazEff2) &
                        + grazingFlux_phy * recipQuota * (1.d0 - grazEff) & ! Small phyto
                        + grazingFlux_Dia * recipQuota_Dia * (1.d0 - grazEff) & ! Diatoms
                        + grazingFlux_miczoo * recipQZoo3 * (1.d0 - grazEff) &
                        + Zoo2LossFlux * recipQZoo2 & ! Macrozooplankton (N->C)
                        + hetLossFlux * recipQZoo & ! Mesozooplankton (N->C)
                        + Zoo2fecalloss_c & ! Macrozooplankton
                        + Mesfecalloss_c & ! Mesozooplankton
                        - grazingFlux_DetZ2 * recipDet2 * grazEff & ! Mesozooplankton
                        - grazingFlux_DetZ22 * recipDet2 * grazEff2 & ! Macrozooplankton
                        - reminC * arrFunc * O2Func * DetZ2C & ! Bacterial respiration
                        ) * dt_b + sms(idetz2c)

            else
                sms(idetz2c) = ( &
                        +grazingFlux_phy2 * recipQuota * (1.d0 - grazEff2) &
                        + grazingFlux_Dia2 * recipQuota_Dia * (1.d0 - grazEff2) &
                        + grazingFlux_Cocco * recipQuota_Cocco * (1.d0 - grazEff) * &
                        is_coccos &
                        + grazingFlux_Cocco2 * recipQuota_Cocco * (1.d0 - grazEff2) * &
                        is_coccos &
                        + grazingFlux_Phaeo * recipQuota_Phaeo * (1.d0 - grazEff) * &
                        is_coccos &
                        + grazingFlux_Phaeo2 * recipQuota_Phaeo * (1.d0 - grazEff2) * &
                        is_coccos &
                        + grazingFlux_het2 * recipQZoo * (1.d0 - grazEff2) &
                        + grazingFlux_miczoo2 * recipQZoo3 * (1.d0 - grazEff2) &
                        + grazingFlux_phy * recipQuota * (1.d0 - grazEff) &
                        + grazingFlux_Dia * recipQuota_Dia * (1.d0 - grazEff) &
                        + grazingFlux_miczoo * recipQZoo3 * (1.d0 - grazEff) &
                        + Zoo2LossFlux * recipQZoo2 &
                        + hetLossFlux * recipQZoo &
                        + Zoo2fecalloss_c &
                        + Mesfecalloss_c &
                        - reminC * arrFunc * O2Func * DetZ2C &
                        ) * dt_b + sms(idetz2c)
            end if


            sms(idetz2si) = ( &
                    +grazingFlux_dia2 * qSiN & ! Macrozooplankton grazing
                    + grazingFlux_dia * qSiN & ! Mesozooplankton grazing
                    - reminSiT * DetZ2Si & ! Temperature-dependent
                    ) * dt_b + sms(idetz2si)

            sms(idetz2calc) = ( &
                    +calc_loss_gra2 * (1.d0 - calc_diss_guts) & ! Macrozooplankton (net)
                    + calc_loss_gra * (1.d0 - calc_diss_guts) & ! Mesozooplankton (net)
                    - calc_diss2 * DetZ2Calc & ! CaCO3 dissolution
                    ) * dt_b + sms(idetz2calc)
        end if ! enable_3zoo2det

        sms(idon) = ( &
                +lossN * limitFacN * phyN & ! Small phytoplankton
                + lossN_d * limitFacN_Dia * DiaN & ! Diatoms
                + lossN_c * limitFacN_Cocco * CoccoN * is_coccos & ! Coccolithophores
                + lossN_p * limitFacN_Phaeo * PhaeoN * is_coccos & ! Phaeocystis
                + reminN * arrFunc * O2Func * DetN & ! Slow-sinking detritus
                + reminN * arrFunc * O2Func * DetZ2N * is_3zoo2det & ! Fast-sinking detritus
                + lossN_z * HetN & ! Mesozooplankton
                + lossN_z2 * Zoo2N * is_3zoo2det & ! Macrozooplankton
                + lossN_z3 * MicZooN * is_3zoo2det & ! Microzooplankton
                - rho_N * arrFunc * O2Func * DON & ! Bacterial remineralization
                ) * dt_b + sms(idon)


        sms(idoc) = ( &
                +lossC * limitFacN * phyC & ! Small phytoplankton
                + lossC_d * limitFacN_dia * DiaC & ! Diatoms
                + lossC_c * limitFacN_cocco * CoccoC * is_coccos & ! Coccolithophores
                + lossC_p * limitFacN_Phaeo * PhaeoC * is_coccos & ! Phaeocystis
                + reminC * arrFunc * O2Func * DetC & ! Slow-sinking detritus
                + reminC * arrFunc * O2Func * DetZ2C * is_3zoo2det & ! Fast-sinking detritus
                + lossC_z * HetC & ! Mesozooplankton
                + lossC_z2 * Zoo2C * is_3zoo2det & ! Macrozooplankton
                + lossC_z3 * MicZooC * is_3zoo2det & ! Microzooplankton
                - rho_c1 * arrFunc * O2Func * EOC & ! Bacterial respiration
                ) * dt_b + sms(idoc)


        sms(idian) = ( &
                +N_assim_dia * DiaC &
                - lossN_d * limitFacN_dia * DiaN &
                - aggregationRate * DiaN &
                - grazingFlux_Dia & ! Mesozooplankton
                - grazingFlux_Dia2 * is_3zoo2det & ! Macrozooplankton
                - grazingFlux_Dia3 * is_3zoo2det & ! Microzooplankton
                ) * dt_b + sms(idian)


        sms(idiac) = ( &
                +Cphot_dia * DiaC & ! Gross photosynthesis
                - lossC_d * limitFacN_dia * DiaC &
                - phyRespRate_dia * DiaC &
                - aggregationRate * DiaC &
                - grazingFlux_dia * recipQuota_dia & ! Mesozooplankton (N->C)
                - grazingFlux_dia2 * recipQuota_dia * is_3zoo2det & ! Macrozooplankton
                - grazingFlux_dia3 * recipQuota_dia * is_3zoo2det & ! Microzooplankton
                ) * dt_b + sms(idiac)


        sms(idchl) = ( &
                +chlSynth_dia * DiaC & ! Photoacclimation
                - KOchl_dia * DiaChl &
                - aggregationRate * DiaChl &
                - grazingFlux_dia * Chl2N_dia & ! Mesozooplankton (N->Chl)
                - grazingFlux_dia2 * Chl2N_dia * is_3zoo2det & ! Macrozooplankton
                - grazingFlux_dia3 * Chl2N_dia * is_3zoo2det & ! Microzooplankton
                ) * dt_b + sms(idchl)


        sms(idiasi) = ( &
                +Si_assim * DiaC &
                - lossN_d * limitFacN_dia * DiaSi &
                - aggregationRate * DiaSi &
                - grazingFlux_dia * qSiN & ! Mesozooplankton (N->Si)
                - grazingFlux_dia2 * qSiN * is_3zoo2det & ! Macrozooplankton
                - grazingFlux_dia3 * qSiN * is_3zoo2det & ! Microzooplankton
                ) * dt_b + sms(idiasi)


        if (enable_coccos) then
            sms(icocn) = ( &
                    +N_assim_cocco * CoccoC &
                    - lossN_c * limitFacN_cocco * CoccoN &
                    - aggregationRate * CoccoN &
                    - grazingFlux_Cocco & ! Mesozooplankton
                    - grazingFlux_Cocco2 * is_3zoo2det & ! Macrozooplankton
                    - grazingFlux_Cocco3 * is_3zoo2det & ! Microzooplankton
                    ) * dt_b + sms(icocn)


            sms(icocc) = ( &
                    +Cphot_cocco * CoccoC & ! Gross photosynthesis
                    - lossC_c * limitFacN_cocco * CoccoC &
                    - phyRespRate_cocco * CoccoC &
                    - aggregationRate * CoccoC &
                    - grazingFlux_cocco * recipQuota_cocco & ! Mesozooplankton (N->C)
                    - grazingFlux_Cocco2 * recipQuota_cocco * is_3zoo2det &
                    - grazingFlux_Cocco3 * recipQuota_cocco * is_3zoo2det &
                    ) * dt_b + sms(icocc)

            if (sms(icocc) > 100) then
                print*, 'ERROR: Unrealistic CoccoC growth detected!'
                print*, 'dt= ', dt
                print*, 'dt_b= ', dt_b
                print*, 'state(icocc): ', state(icocc)
                print*, 'CoccoC: ', CoccoC
                print*, 'CoccoN: ', CoccoN
                print*, 'Cphot_cocco: ', Cphot_cocco * CoccoC
                print*, 'lossC_c: ', lossC_c
                print*, 'limitFacN_cocco: ', limitFacN_cocco
                print*, 'phyRespRate_cocco: ', phyRespRate_cocco
                print*, 'grazingFlux_cocco: ', grazingFlux_cocco
                print*, 'grazingFlux_Cocco2: ', grazingFlux_Cocco2
                print*, 'grazingFlux_Cocco3: ', grazingFlux_Cocco3
                print*, 'recipQuota_cocco: ', recipQuota_cocco
                stop
            end if


            sms(icchl) = ( &
                    +ChlSynth_cocco * CoccoC & ! Photoacclimation
                    - KOchl_cocco * CoccoChl &
                    - aggregationRate * CoccoChl &
                    - grazingFlux_cocco * Chl2N_cocco & ! Mesozooplankton (N->Chl)
                    - grazingFlux_Cocco2 * Chl2N_cocco * is_3zoo2det & ! Macrozooplankton
                    - grazingFlux_Cocco3 * Chl2N_cocco * is_3zoo2det & ! Microzooplankton
                    ) * dt_b + sms(icchl)


            sms(iphan) = ( &
                    +N_assim_phaeo * PhaeoC &
                    - lossN_p * limitFacN_phaeo * PhaeoN &
                    - aggregationRate * PhaeoN &
                    - grazingFlux_phaeo & ! Mesozooplankton
                    - grazingFlux_phaeo2 * is_3zoo2det & ! Macrozooplankton
                    - grazingFlux_phaeo3 * is_3zoo2det & ! Microzooplankton
                    ) * dt_b + sms(iphan)


            sms(iphac) = ( &
                    +Cphot_phaeo * PhaeoC & ! Gross photosynthesis
                    - lossC_p * limitFacN_phaeo * PhaeoC &
                    - phyRespRate_phaeo * PhaeoC &
                    - aggregationRate * PhaeoC &
                    - grazingFlux_phaeo * recipQuota_phaeo & ! Mesozooplankton (N->C)
                    - grazingFlux_phaeo2 * recipQuota_phaeo * is_3zoo2det &
                    - grazingFlux_phaeo3 * recipQuota_phaeo * is_3zoo2det &
                    ) * dt_b + sms(iphac)


            sms(iphachl) = ( &
                    +chlSynth_phaeo * PhaeoC & ! Photoacclimation
                    - KOchl_phaeo * PhaeoChl &
                    - aggregationRate * PhaeoChl &
                    - grazingFlux_phaeo * Chl2N_phaeo & ! Mesozooplankton (N->Chl)
                    - grazingFlux_phaeo2 * Chl2N_phaeo * is_3zoo2det & ! Macrozooplankton
                    - grazingFlux_phaeo3 * Chl2N_phaeo * is_3zoo2det & ! Microzooplankton
                    ) * dt_b + sms(iphachl)

        end if ! enable_coccos

        sms(idetsi) = ( &
                +aggregationRate * DiaSi &
                + lossN_d * limitFacN_dia * DiaSi &
                + grazingFlux_dia3 * qSiN * is_3zoo2det & ! Microzooplankton
                + grazingFlux_dia * qSiN * (1.0 - is_3zoo2det) &
                - reminSiT * DetSi & ! Temperature-dependent
                ) * dt_b + sms(idetsi)


        sms(isi) = ( &
                -Si_assim * DiaC &
                + reminSiT * DetSi & ! Slow-sinking detritus
                + reminSiT * DetZ2Si * is_3zoo2det & ! Fast-sinking detritus
                ) * dt_b + sms(isi)


        sms(ife) = ( &
                Fe2N * ( &
                -N_assim * PhyC & ! Small phytoplankton
                - N_assim_dia * DiaC & ! Diatoms
                - N_assim_cocco * CoccoC * is_coccos & ! Coccolithophores
                - N_assim_phaeo * PhaeoC * is_coccos & ! Phaeocystis
                + lossN * limitFacN * PhyN & ! Small phytoplankton
                + lossN_d * limitFacN_dia * DiaN & ! Diatoms
                + lossN_c * limitFacN_cocco * CoccoN * is_coccos & ! Coccolithophores
                + lossN_p * limitFacN_phaeo * PhaeoN * is_coccos & ! Phaeocystis
                + reminN * arrFunc * O2Func * DetN & ! Slow-sinking detritus
                + reminN * arrFunc * O2Func * DetZ2N * is_3zoo2det & ! Fast-sinking detritus
                + lossN_z * HetN & ! Mesozooplankton
                + lossN_z2 * Zoo2N * is_3zoo2det & ! Macrozooplankton
                + lossN_z3 * MicZooN * is_3zoo2det & ! Microzooplankton
                ) &
                - kScavFe * DetC * FreeFe & ! Slow-sinking detritus
                - kScavFe * DetZ2C * FreeFe * is_3zoo2det & ! Fast-sinking detritus
                ) * dt_b + sms(ife)


        if (enable_coccos) then
            sms(iphycal) = ( &
                    +calcification & ! New CaCO3 production
                    - lossC_c * limitFacN_cocco * PhyCalc & ! Excretion/exudation
                    - phyRespRate_cocco * PhyCalc & ! Respiration-associated loss
                    - calc_loss_agg & ! Aggregation/sinking
                    - calc_loss_gra & ! Mesozooplankton grazing
                    - calc_loss_gra2 * is_3zoo2det & ! Macrozooplankton grazing
                    - calc_loss_gra3 * is_3zoo2det & ! Microzooplankton grazing
                    ) * dt_b + sms(iphycal)
        else
            sms(iphycal) = ( &
                    +calcification &
                    - lossC * limitFacN * PhyCalc &
                    - phyRespRate * PhyCalc &
                    - calc_loss_agg &
                    - calc_loss_gra &
                    - calc_loss_gra2 * is_3zoo2det &
                    - calc_loss_gra3 * is_3zoo2det &
                    ) * dt_b + sms(iphycal)
        end if


        if (enable_coccos) then
            if (enable_3zoo2det) then
                sms(idetcal) = ( &
                        +lossC_c * limitFacN_cocco * PhyCalc & ! Excretion
                        + phyRespRate_cocco * PhyCalc & ! Respiration products
                        + calc_loss_agg & ! Aggregation products
                        + calc_loss_gra3 & ! Microzooplankton grazing
                        - calc_loss_gra3 * calc_diss_guts & ! Gut dissolution
                        - calc_diss * DetCalc & ! Water column dissolution
                        ) * dt_b + sms(idetcal)
            else
                sms(idetcal) = ( &
                        +lossC_c * limitFacN_cocco * PhyCalc &
                        + phyRespRate_cocco * PhyCalc &
                        + calc_loss_agg &
                        + calc_loss_gra &
                        - calc_loss_gra * calc_diss_guts &
                        - calc_diss * DetCalc &
                        ) * dt_b + sms(idetcal)
            end if
        else
            if (enable_3zoo2det) then
                sms(idetcal) = ( &
                        +lossC * limitFacN * PhyCalc &
                        + phyRespRate * PhyCalc &
                        + calc_loss_agg &
                        + calc_loss_gra3 &
                        - calc_loss_gra3 * calc_diss_guts &
                        - calc_diss * DetCalc &
                        ) * dt_b + sms(idetcal)
            else
                sms(idetcal) = ( &
                        +lossC * limitFacN * PhyCalc &
                        + phyRespRate * PhyCalc &
                        + calc_loss_agg &
                        + calc_loss_gra &
                        - calc_loss_gra * calc_diss_guts &
                        - calc_diss * DetCalc &
                        ) * dt_b + sms(idetcal)
            end if
        end if


        sms(ioxy) = ( &
                + Cphot * phyC & ! Small phytoplankton
                + Cphot_dia * diaC & ! Diatoms
                + Cphot_cocco * CoccoC * is_coccos & ! Coccolithophores
                + Cphot_phaeo * PhaeoC * is_coccos & ! Phaeocystis
                - phyRespRate * phyC & ! Small phytoplankton
                - phyRespRate_dia * diaC & ! Diatoms
                - phyRespRate_cocco * CoccoC * is_coccos & ! Coccolithophores
                - phyRespRate_phaeo * PhaeoC * is_coccos & ! Phaeocystis
                - rho_C1 * arrFunc * O2Func * EOC & ! DOC remineralization
                - hetRespFlux & ! Mesozooplankton
                - Zoo2RespFlux * is_3zoo2det & ! Macrozooplankton
                - MicZooRespFlux * is_3zoo2det & ! Microzooplankton
                ) * redO2C * dt_b + sms(ioxy)


        if (ciso) then
            sms(idic_13) = ( &
                    -Cphot * PhyC_13 & ! Small phyto photosynthesis
                    - Cphot_Dia * DiaC_13 & ! Diatom photosynthesis
                    + phyRespRate * PhyC_13 & ! Small phyto respiration
                    + phyRespRate_Dia * DiaC_13 & ! Diatom respiration
                    + rho_C1 * arrFunc * EOC_13 & ! DOC remineralization
                    + HetRespFlux_13 & ! Heterotroph respiration
                    + calc_diss_13 * DetCalc_13 & ! Water column dissolution
                    + calc_loss_gra_13 * calc_diss_guts & ! Gut dissolution
                    - calcification_13 & ! CaCO3 precipitation
                    ) * dt_b + sms(idic_13)

            sms(iphyc_13) = ( &
                    +Cphot * PhyC_13 & ! 13C fixation
                    - lossC * limitFacN * PhyC_13 & ! Nutrient-stress mortality
                    - phyRespRate * PhyC_13 & ! Respiration
                    - aggregationRate * PhyC_13 & ! Aggregation loss
                    - grazingFlux_phy * recipQuota_13 & ! Grazing loss (N->C conversion)
                    ) * dt_b + sms(iphyc_13)

            sms(idetc_13) = ( &
                    +grazingFlux_phy * recipQuota_13 & ! Total small phyto grazing
                    - grazingFlux_phy * recipQuota_13 * grazEff &
                    + grazingFlux_Dia * recipQuota_dia_13 & ! Total diatom grazing
                    - grazingFlux_Dia * recipQuota_dia_13 * grazEff &
                    + aggregationRate * phyC_13 & ! Small phyto aggregation
                    + aggregationRate * DiaC_13 & ! Diatom aggregation
                    + hetLossFlux * recipQZoo_13 & ! Heterotroph mortality
                    - reminC * arrFunc * DetC_13 & ! Aerobic respiration
                    ) * dt_b + sms(idetc_13)

            sms(ihetc_13) = ( &
                    +grazingFlux_phy * recipQuota_13 * grazEff & ! Small phyto consumption
                    + grazingFlux_Dia * recipQuota_dia_13 * grazEff & ! Diatom consumption
                    - hetLossFlux * recipQZoo_13 & ! Mortality flux
                    - lossC_z * HetC_13 & ! Non-predatory losses
                    - hetRespFlux_13 & ! Respiration
                    ) * dt_b + sms(ihetc_13)

            sms(idoc_13) = ( &
                    +lossC * limitFacN * phyC_13 & ! Small phyto exudation
                    + lossC_d * limitFacN_dia * DiaC_13 & ! Diatom exudation
                    + reminC * arrFunc * DetC_13 & ! Detritus solubilization
                    + lossC_z * HetC_13 & ! Heterotroph exudation
                    + LocRiverDOC * r_iorg_13 & ! River 13C input
                    - rho_c1 * arrFunc * EOC_13 & ! Microbial respiration
                    ) * dt_b + sms(idoc_13)

            sms(idiac_13) = ( &
                    +Cphot_dia * DiaC_13 & ! 13C fixation
                    - lossC_d * limitFacN_dia * DiaC_13 & ! Nutrient-stress mortality
                    - phyRespRate_dia * DiaC_13 & ! Respiration
                    - aggregationRate * DiaC_13 & ! Aggregation loss
                    - grazingFlux_dia * recipQuota_dia_13 & ! Grazing loss
                    ) * dt_b + sms(idiac_13)

            sms(iphycal_13) = ( &
                    +calcification_13 & ! CaCO3 precipitation
                    - lossC * limitFacN * phyCalc_13 & ! Mortality to detritus
                    - phyRespRate * phyCalc_13 & ! Death to detritus
                    - calc_loss_agg_13 & ! Aggregation
                    - calc_loss_gra_13 & ! Grazing
                    ) * dt_b + sms(iphycal_13)

            sms(idetcal_13) = ( &
                    +lossC * limitFacN * phyCalc_13 & ! Mortality
                    + phyRespRate * phyCalc_13 & ! Death
                    + calc_loss_agg_13 & ! Aggregation
                    + calc_loss_gra_13 & ! Grazing (to fecal pellets)
                    - calc_loss_gra_13 * calc_diss_guts & ! Gut dissolution
                    - calc_diss_13 * DetCalc_13 & ! Water column dissolution
                    ) * dt_b + sms(idetcal_13)

            if (ciso_14) then
                if (ciso_organic_14) then
                    sms(idic_14) = ( &
                            -Cphot * PhyC_14 &
                            + phyRespRate * PhyC_14 &
                            - Cphot_Dia * DiaC_14 &
                            + phyRespRate_Dia * DiaC_14 &
                            + rho_C1 * arrFunc * EOC_14 &
                            + HetRespFlux_14 &
                            + calc_diss_14 * DetCalc_14 &
                            + calc_loss_gra_14 * calc_diss_guts &
                            - calcification_14 &
                            ) * dt_b + sms(idic_14)

                    sms(iphyc_14) = ( &
                            +Cphot * PhyC_14 &
                            - lossC * limitFacN * PhyC_14 &
                            - phyRespRate * PhyC_14 &
                            - aggregationRate * PhyC_14 &
                            - grazingFlux_phy * recipQuota_14 &
                            ) * dt_b + sms(iphyc_14)

                    sms(idetc_14) = ( &
                            +grazingFlux_phy * recipQuota_14 &
                            - grazingFlux_phy * recipQuota_14 * grazEff &
                            + grazingFlux_Dia * recipQuota_dia_14 &
                            - grazingFlux_Dia * recipQuota_dia_14 * grazEff &
                            + aggregationRate * phyC_14 &
                            + aggregationRate * DiaC_14 &
                            + hetLossFlux * recipQZoo_14 &
                            - reminC * arrFunc * DetC_14 &
                            ) * dt_b + sms(idetc_14)

                    sms(ihetc_14) = ( &
                            +grazingFlux_phy * recipQuota_14 * grazEff &
                            + grazingFlux_Dia * recipQuota_dia_14 * grazEff &
                            - hetLossFlux * recipQZoo_14 &
                            - lossC_z * HetC_14 &
                            - hetRespFlux_14 &
                            ) * dt_b + sms(ihetc_14)

                    sms(idoc_14) = ( &
                            +lossC * limitFacN * phyC_14 &
                            + lossC_d * limitFacN_dia * DiaC_14 &
                            + reminC * arrFunc * DetC_14 &
                            + lossC_z * HetC_14 &
                            - rho_c1 * arrFunc * EOC_14 &
                            + LocRiverDOC * r_iorg_14 &
                            ) * dt_b + sms(idoc_14)

                    sms(idiac_14) = ( &
                            +Cphot_dia * DiaC_14 &
                            - lossC_d * limitFacN_dia * DiaC_14 &
                            - phyRespRate_dia * DiaC_14 &
                            - aggregationRate * DiaC_14 &
                            - grazingFlux_dia * recipQuota_dia_14 &
                            ) * dt_b + sms(idiac_14)

                    sms(iphycal_14) = ( &
                            +calcification_14 &
                            - lossC * limitFacN * phyCalc_14 &
                            - phyRespRate * phyCalc_14 &
                            - calc_loss_agg_14 &
                            - calc_loss_gra_14 &
                            ) * dt_b + sms(iphycal_14)

                    sms(idetcal_14) = ( &
                            +lossC * limitFacN * phyCalc_14 &
                            + phyRespRate * phyCalc_14 &
                            + calc_loss_agg_14 &
                            + calc_loss_gra_14 &
                            - calc_loss_gra_14 * calc_diss_guts &
                            - calc_diss_14 * DetCalc_14 &
                            ) * dt_b + sms(idetcal_14)
                else
                    sms(idic_14) = sms(idic)
                end if ! ciso_organic_14
            end if ! ciso_14
        end if ! ciso

        _ADD_SOURCE_(self%id_dissolved_inorganic_nitrogen, sms(idin))
        _ADD_SOURCE_(self%id_dissolved_inorganic_carbon, sms(idic))
        _ADD_SOURCE_(self%id_alkalinity, sms(ialk))
        _ADD_SOURCE_(self%id_phytoplankton_nitrogen, sms(iphyn))
        _ADD_SOURCE_(self%id_phytoplankton_carbon, sms(iphyc))
        _ADD_SOURCE_(self%id_phytoplankton_chlorophyll, sms(ipchl))
        _ADD_SOURCE_(self%id_detrital_nitrogen, sms(idetn))
        _ADD_SOURCE_(self%id_detrital_carbon, sms(idetc))
        _ADD_SOURCE_(self%id_heterotroph_nitrogen, sms(ihetn))
        _ADD_SOURCE_(self%id_heterotroph_carbon, sms(ihetc))
        _ADD_SOURCE_(self%id_dissolved_organic_nitrogen, sms(idon))
        _ADD_SOURCE_(self%id_dissolved_organic_carbon, sms(idoc))
        _ADD_SOURCE_(self%id_diatom_nitrogen, sms(idian))
        _ADD_SOURCE_(self%id_diatom_carbon, sms(idiac))
        _ADD_SOURCE_(self%id_diatom_chlorophyll, sms(idchl))
        _ADD_SOURCE_(self%id_diatom_silica, sms(idiasi))
        _ADD_SOURCE_(self%id_detrital_silica, sms(idetsi))
        _ADD_SOURCE_(self%id_silica, sms(isi))
        _ADD_SOURCE_(self%id_iron, sms(ife))
        _ADD_SOURCE_(self%id_phytoplankton_calcite, sms(iphycal))
        _ADD_SOURCE_(self%id_detrital_calcite, sms(idetcal))
        _ADD_SOURCE_(self%id_oxygen, sms(ioxy))

        if (enable_coccos) then
            _ADD_SOURCE_(self%id_coccolithophore_nitrogen, sms(icocn))
            _ADD_SOURCE_(self%id_coccolithophore_carbon, sms(icocc))
            _ADD_SOURCE_(self%id_coccolithophore_chlorophyll, sms(icchl))
            _ADD_SOURCE_(self%id_phaeocystis_nitrogen, sms(iphan))
            _ADD_SOURCE_(self%id_phaeocystis_carbon, sms(iphac))
            _ADD_SOURCE_(self%id_phaeocystis_chlorophyll, sms(iphachl))
        end if

        if (enable_3zoo2det) then
            _ADD_SOURCE_(self%id_macrozooplankton_nitrogen, sms(izoo2n))
            _ADD_SOURCE_(self%id_macrozooplankton_carbon, sms(izoo2c))
            _ADD_SOURCE_(self%id_microzooplankton_nitrogen, sms(imiczoon))
            _ADD_SOURCE_(self%id_microzooplankton_carbon, sms(imiczooc))

            _ADD_SOURCE_(self%id_macrozooplankton_detrital_nitrogen, sms(idetz2n))
            _ADD_SOURCE_(self%id_macrozooplankton_detrital_carbon, sms(idetz2c))
            _ADD_SOURCE_(self%id_macrozooplankton_detrital_silica, sms(idetz2si))
            _ADD_SOURCE_(self%id_macrozooplankton_detrital_calcite, sms(idetz2calc))
        end if

       _LOOP_END_
    end subroutine do

    subroutine read_recom_namelist()
        use recom_config
        use recom_ciso

        implicit none

        integer :: fileunit

        open (newunit=fileunit, file='namelist.recom')
        read (fileunit, NML=parecomsetup)
        read (fileunit, NML=pavariables)
        read (fileunit, NML=pasinking)
        read (fileunit, NML=painitialization_N)
        read (fileunit, NML=paArrhenius)
        read (fileunit, NML=palimiter_function)
        read (fileunit, NML=palight_calculations)
        read (fileunit, NML=paphotosynthesis)
        read (fileunit, NML=paassimilation)
        read (fileunit, NML=pairon_chem)
        read (fileunit, NML=pazooplankton)
        read (fileunit, NML=pasecondzooplankton)
        read (fileunit, NML=pathirdzooplankton)
        read (fileunit, NML=pagrazingdetritus)
        read (fileunit, NML=paaggregation)
        read (fileunit, NML=padin_rho_N)
        read (fileunit, NML=padic_rho_C1)
        read (fileunit, NML=paphytoplankton_N)
        read (fileunit, NML=paphytoplankton_C)
        read (fileunit, NML=paphytoplankton_ChlA)
        read (fileunit, NML=padetritus_N)
        read (fileunit, NML=padetritus_C)
        read (fileunit, NML=paheterotrophs)
        read (fileunit, NML=paseczooloss)
        read (fileunit, NML=pathirdzooloss)
        read (fileunit, NML=paco2lim)
        read (fileunit, NML=pairon)
        read (fileunit, NML=pacalc)
        read (fileunit, NML=pabenthos_decay_rate)
        read (fileunit, NML=paco2_flux_param)
        read (fileunit, NML=paalkalinity_restoring)
        read (fileunit, NML=paballasting)
        read (fileunit, NML=paciso)
        close (fileunit)
    end subroutine read_recom_namelist

end module awi_recom_sms
