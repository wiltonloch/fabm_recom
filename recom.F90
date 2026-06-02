#include "fabm_driver.h"

module awi_recom

    use fabm_types

    implicit none

    private

    type, extends(type_base_model), public :: type_awi_recom

        ! Dependencies
        type (type_horizontal_dependency_id) :: id_surface_air_pressure
        type (type_dependency_id) :: id_temperature
        type (type_dependency_id) :: id_practical_salinity

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
        procedure :: do_surface
    end type type_awi_recom

contains

    subroutine initialize(self, configunit)
        use fabm_types, only: standard_variables
        use recom_init_interface, only: initialize_tracer_ids, get_tracer_init_value
        use recom_declarations, only: tracer_ids

        implicit none

        class(type_awi_recom), intent(inout), target :: self
        integer, intent(in) :: configunit

        call read_recom_namelist()
        call initialize_tracer_ids()

        call self%register_dependency(self%id_surface_air_pressure, standard_variables%surface_air_pressure)
        call self%register_dependency(self%id_temperature, standard_variables%temperature)
        call self%register_dependency(self%id_practical_salinity, standard_variables%practical_salinity)

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
       use recom_config
       use recom_declarations
       use recom_ciso
        
       implicit none

       class (type_awi_recom), intent(in) :: self
       _DECLARE_ARGUMENTS_DO_

        real(kind=wp) :: dt_d !< Size of time steps [day]
        real(kind=wp) :: dt_b !< Size of time steps [day]

        real(kind=wp) :: recip_hetN_plus !< MB's addition to heterotrophic respiration

        real(kind=wp) :: recip_res_het
        real(kind=wp) :: Sink_Vel
        real(kind=wp) :: aux
        integer :: k, step

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

end module awi_recom
