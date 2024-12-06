# (C) Copyright 2024 ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

macro( fckit_install_venv )

    # Create a virtualenv
    set( VENV_PATH ${CMAKE_CURRENT_BINARY_DIR}/fckit_venv )
    ecbuild_info( "Create Python virtual environment ${VENV_PATH}" )
    execute_process( COMMAND ${Python3_EXECUTABLE} -m venv --copies "${VENV_PATH}" )

    # Make the virtualenv portable by automatically deducing the VIRTUAL_ENV path from
    # the 'activate' script's location in the filesystem
    execute_process(
        COMMAND
            sed -i "s/^VIRTUAL_ENV=\".*\"$/VIRTUAL_ENV=\"$(cd \"$(dirname \"$(dirname \"\${BASH_SOURCE[0]}\" )\")\" \\&\\& pwd)\"/" "${VENV_PATH}/bin/activate"
    )

    # Change the context of the search to only find the venv
    set( Python3_FIND_VIRTUALENV ONLY )
    set( Python3_EXECUTABLE_CACHE ${Python3_EXECUTABLE} )

    # Unset Python3_EXECUTABLE because it is also an input variable
    #  (see documentation, Artifacts Specification section)
    unset( Python3_EXECUTABLE )
    # To allow cmake to discover the newly created venv if Python3_ROOT_DIR
    # was passed as an argument at build-time
    set( Python3_ROOT_DIR "${VENV_PATH}" )

    # Find newly created python venv
    find_package( Python3 COMPONENTS Interpreter REQUIRED )

    if( Python3_VERSION VERSION_EQUAL 3.8 )
       execute_process( COMMAND ${Python3_EXECUTABLE} -m pip --disable-pip-version-check
                        install --upgrade ${PIP_OPTIONS} pip OUTPUT_QUIET ERROR_QUIET )
    endif()

    # install pip dependencies
	ecbuild_info( "Install fckit_yaml_reader dependencies in virtual environment ${VENV_PATH}" )
    if( HAVE_FCKIT_VENV_OFFLINE )
        execute_process( COMMAND ${Python3_EXECUTABLE} -m pip --disable-pip-version-check
                         install -r ${CMAKE_CURRENT_SOURCE_DIR}/cmake/fckit_venv_requirements.txt
                         --no-index --find-links ${FCKIT_VENV_WHEEL_DIR} OUTPUT_QUIET )
        list( APPEND PIP_OPTIONS "--no-build-isolation" )
    else()
        execute_process( COMMAND ${Python3_EXECUTABLE} -m pip --disable-pip-version-check
                         install -r ${CMAKE_CURRENT_SOURCE_DIR}/cmake/fckit_venv_requirements.txt
                         OUTPUT_QUIET )
    endif()


    if( HAVE_FCKIT_VENV_EDITABLE )
        # Use checked-out source instead of installing into venv
        list( APPEND PIP_OPTIONS "-e" )
    endif()

    # install virtual environment from requirements
    set( _pkg_name "fckit_yaml_reader")
    ecbuild_info( "Install fckit_yaml_reader in virtual environment ${VENV_PATH}" )
    execute_process( COMMAND ${Python3_EXECUTABLE} -m pip --disable-pip-version-check
                     install ${PIP_OPTIONS} ${CMAKE_CURRENT_SOURCE_DIR}/src/fckit/${_pkg_name} OUTPUT_QUIET )

    # install fypp
    if( NOT HAVE_FCKIT_VENV_EDITABLE )
       list( APPEND PIP_OPTIONS "--use-pep517" )
    endif()
    ecbuild_info( "Install fypp in virtual environment ${VENV_PATH}" )
    execute_process( COMMAND ${Python3_EXECUTABLE} -m pip --disable-pip-version-check
                     install ${PIP_OPTIONS} ${CMAKE_CURRENT_SOURCE_DIR}/contrib/fypp-3.2-b8dd58b-20230822 OUTPUT_QUIET )

    if( ECBUILD_INSTALL_LIBRARY_HEADERS )
       install( DIRECTORY ${VENV_PATH} DESTINATION . PATTERN "bin/*" PERMISSIONS ${install_permissions} )
    endif()

    # add python interpreter of venv as executable target
    set( FCKIT_VENV_EXE ${Python3_EXECUTABLE} )

    # compute relative path to venv to aid with installation
    string(REPLACE "${CMAKE_CURRENT_BINARY_DIR}/" "" rel_venv_exe_path ${FCKIT_VENV_EXE})

    set( FYPP ${CMAKE_CURRENT_SOURCE_DIR}/tools/fckit-eval.sh ${FCKIT_VENV_EXE} -m fypp )

    # reset Python3_EXECUTABLE to the system install
    set( Python3_EXECUTABLE ${Python3_EXECUTABLE_CACHE} )

endmacro()

