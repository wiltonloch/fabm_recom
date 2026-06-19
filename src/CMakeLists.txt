add_library(fabm_models_awi OBJECT
            model_library.F90
            recom_sms.F90
            recom_light_attenuation.F90
           )

set(RECOM_LIB_PATH "" CACHE FILEPATH "Path to librecom.a")
set(RECOM_INCLUDE_PATH "" CACHE FILEPATH "Path to librecom.a")

if(NOT RECOM_LIB_PATH)
    message(FATAL_ERROR "RECOM_LIB_PATH is not set. Configure with -DRECOM_LIB_PATH=/path/to/librecom.a")
endif()

if(NOT RECOM_INCLUDE_PATH)
    message(FATAL_ERROR "RECOM_INCLUDE_PATH is not set. Configure with -DRECOM_INCLUDE_PATH=/path/to/includefiles")
endif()

if(NOT EXISTS "${RECOM_LIB_PATH}")
    message(FATAL_ERROR "Static ReCom library not found at: ${RECOM_LIB_PATH}")
endif()

target_link_libraries(fabm_models_awi PRIVATE fabm_base)
target_link_libraries(fabm_models_awi PRIVATE ${RECOM_LIB_PATH})
target_include_directories(fabm_models_awi PRIVATE ${RECOM_INCLUDE_PATH})
