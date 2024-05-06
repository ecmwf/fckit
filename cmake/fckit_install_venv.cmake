# (C) Copyright 2013 ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

macro( fckit_install_venv )

    # Discover only system install Python 3
    set( Python3_FIND_VIRTUALENV STANDARD )
    find_package( Python3 COMPONENTS Interpreter REQUIRED )
    
    # Create a loki virtualenv
    set( VENV_PATH ${CMAKE_CURRENT_BINARY_DIR}/venv )
    message( STATUS "Create Python virtual environment ${VENV_PATH}" )
    execute_process( COMMAND ${Python3_EXECUTABLE} -m venv --copies "${VENV_PATH}" )

    # Make the virtualenv portable by automatically deducing the VIRTUAL_ENV path from
    # the 'activate' script's location in the filesystem
    execute_process(
        COMMAND
            sed -i "s/^VIRTUAL_ENV=\".*\"$/VIRTUAL_ENV=\"$(cd \"$(dirname \"$(dirname \"\${BASH_SOURCE[0]}\" )\")\" \\&\\& pwd)\"/" "${VENV_PATH}/bin/activate"
    )

    # Change the context of the search to only find the venv
    set( Python3_FIND_VIRTUALENV ONLY )

    # Unset Python3_EXECUTABLE because it is also an input variable
    #  (see documentation, Artifacts Specification section)
    set( Python3_EXECUTABLE_CACHE ${Python3_EXECUTABLE} )
    unset( Python3_EXECUTABLE )
    # To allow cmake to discover the newly created venv if Python3_ROOT_DIR
    # was passed as an argument at build-time
    set( Python3_ROOT_DIR "${VENV_PATH}" )

    # Find newly created python venv
    find_package( Python3 COMPONENTS Interpreter REQUIRED )

    set( _pkg_name "fckit_yaml_reader")
    if( HAVE_TESTS )
       set( _pkg_name "fckit_yaml_reader/[tests]")
    endif()

    message( STATUS "Install fckit_yaml_reader in virtual environment ${VENV_PATH}" )
    execute_process( COMMAND ${Python3_EXECUTABLE} -m pip install --disable-pip-version-check ${CMAKE_CURRENT_SOURCE_DIR}/src/fckit/${_pkg_name} OUTPUT_QUIET )

    # install ruamel
    message( STATUS "Install ruamel.yaml in virtual environment ${VENV_PATH}" )
    execute_process( COMMAND ${Python3_EXECUTABLE} -m pip install --disable-pip-version-check ${CMAKE_CURRENT_SOURCE_DIR}/contrib/ruamel.yaml-0.18.6 OUTPUT_QUIET )
   
    # install fypp
    message( STATUS "Install fypp in virtual environment ${VENV_PATH}" )
    execute_process( COMMAND ${Python3_EXECUTABLE} -m pip install --disable-pip-version-check ${CMAKE_CURRENT_SOURCE_DIR}/contrib/fypp-3.2-b8dd58b-20230822 OUTPUT_QUIET )

    install( DIRECTORY ${VENV_PATH} DESTINATION . PATTERN "bin/*" PERMISSIONS ${install_permissions} )

    # add python interpreter of venv as executable target
    set( FCKIT_VENV_EXE ${Python3_EXECUTABLE} )

    # compute relative path to venv to aid with installation
    cmake_path( RELATIVE_PATH FCKIT_VENV_EXE BASE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR} OUTPUT_VARIABLE rel_venv_exe_path )

    set( FYPP ${CMAKE_CURRENT_SOURCE_DIR}/tools/fckit-eval.sh ${FCKIT_VENV_EXE} -m fypp )

    # reset Python3_EXECUTABLE to the system install
    set( Python3_EXECUTABLE ${Python3_EXECUTABLE_CACHE} )

endmacro()    