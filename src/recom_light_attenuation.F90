#include "fabm_driver.h"

module awi_recom_light_attenuation

   use fabm_types

   implicit none

   private

   type, extends(type_base_model), public :: type_awi_recom_light_attenuation
      type (type_dependency_id)            :: id_cell_thickness
      type (type_dependency_id)            :: id_phytoplankton_chlorophyll
      type (type_dependency_id)            :: id_diatom_chlorophyll
      type (type_surface_dependency_id)    :: id_surface_shortwave_radiation
      type (type_horizontal_dependency_id) :: id_latitude
      type (type_global_dependency_id)     :: id_days_since_start_of_year

      type (type_diagnostic_variable_id)   :: id_par  ! Photosynthetically active radiation
   contains
      procedure :: initialize
      procedure :: do_column
   end type type_awi_recom_light_attenuation

contains

   subroutine initialize(self, configunit)
      class (type_awi_recom_light_attenuation), intent(inout), target :: self
      integer,                 intent(in)            :: configunit

      ! Register diagnostic variables
      call self%register_diagnostic_variable(self%id_par, 'par', 'W m-2', 'photosynthetically active radiation', &
         standard_variable=standard_variables%downwelling_photosynthetic_radiative_flux, source=source_do_column)

      ! Register environmental dependencies (temperature, shortwave radiation)
      call self%register_dependency(self%id_surface_shortwave_radiation, standard_variables%surface_downwelling_shortwave_flux)
      call self%register_dependency(self%id_cell_thickness, standard_variables%cell_thickness)

      call self%register_dependency(self%id_phytoplankton_chlorophyll, 'phytoplankton_chlorophyll', 'mg m-3', 'Phytoplankton Chlorophyll')
      call self%register_dependency(self%id_diatom_chlorophyll, 'diatom_chlorophyll', 'mg m-3', 'Diatom Chlorophyll')
      call self%register_dependency(self%id_latitude, standard_variables%latitude)
      call self%register_dependency(self%id_days_since_start_of_year, standard_variables%number_of_days_since_start_of_the_year)
   end subroutine

   subroutine do_column(self, _ARGUMENTS_DO_COLUMN_)

      use recom_declarations
      use recom_config
      use recom_extra

      class (type_awi_recom_light_attenuation), intent(in) :: self
      _DECLARE_ARGUMENTS_DO_COLUMN_

      real(rk) :: surface_shortwave_radiation, cell_thickness, PhyChl, DiaChl, kappa
      real(rk) :: declination, latitude, incidence_angle_cosine
      integer :: days
      logical :: on_surface

      on_surface = .true.

      _GET_GLOBAL_(self%id_days_since_start_of_year, days)
      _GET_HORIZONTAL_(self%id_latitude, latitude)
      _GET_SURFACE_(self%id_surface_shortwave_radiation, surface_shortwave_radiation)

      call calculate_solar_declination(days, 365, declination)

      _DOWNWARD_LOOP_BEGIN_

         _GET_(self%id_phytoplankton_chlorophyll, PhyChl)
         _GET_(self%id_diatom_chlorophyll, DiaChl)

         if (on_surface) then

            chl_upper = (PhyChl + DiaChl) ! Base groups (always present)
            PARave = max(tiny, surface_shortwave_radiation)
            _SET_DIAGNOSTIC_(self%id_par, PARave) ! Photosynthetically active radiation at layer centre

            on_surface = .false.

         else

            _GET_(self%id_cell_thickness, cell_thickness)
            incidence_angle_cosine = solar_incidence_cosine(latitude, declination)

            chl_lower = PhyChl + DiaChl
            Chlave = (chl_upper + chl_lower) * 0.5
            kappa = k_w + a_chl * Chlave
            kappastar = kappa / incidence_angle_cosine
            kappastar = kappa
            kdzLower = kdzUpper + kappastar * cell_thickness

            Lowerlight = surface_shortwave_radiation * exp(-kdzLower)
            Lowerlight = max(tiny, Lowerlight) ! Ensure positive value
            PARave = Lowerlight
            chl_upper = chl_lower ! Current lower becomes next upper
            kdzUpper = kdzLower ! Current cumulative depth for next layer

            _SET_DIAGNOSTIC_(self%id_par,PARave) ! Photosynthetically active radiation at layer centre
         end if

      _DOWNWARD_LOOP_END_
   end subroutine do_column

end module awi_recom_light_attenuation
