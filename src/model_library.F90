module awi_model_library

    use fabm_types, only: type_base_model_factory, type_base_model

    use awi_recom_sms
    use awi_recom_light_attenuation

    implicit none

    private

    type, extends(type_base_model_factory) :: type_factory
    contains
        procedure :: create
    end type type_factory

    type(type_factory), save, target, public :: awi_model_factory

contains

    subroutine create(self, name, model)

        class(type_factory), intent(in) :: self
        character(*), intent(in) :: name
        class(type_base_model), pointer :: model

        select case (name)
        case ('recom_sms')
            allocate(type_awi_recom_sms :: model)
        case ('recom_light_attenuation')
            allocate(type_awi_recom_light_attenuation :: model)
        case default
            write(*, *) 'Error: Model ', trim(name), ' is not available in the REcoM model library.'
            stop
        end select

    end subroutine create

end module awi_model_library
