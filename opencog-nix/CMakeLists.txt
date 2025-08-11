# OpenCog Nix packaging
cmake_minimum_required(VERSION 3.10)

project(opencog-nix)

# Install Nix packaging files
file(GLOB_RECURSE NIX_FILES "*.nix" "*.lock" "*.md")
if(NIX_FILES)
    foreach(nix_file ${NIX_FILES})
        file(RELATIVE_PATH rel_path ${CMAKE_CURRENT_SOURCE_DIR} ${nix_file})
        get_filename_component(rel_dir ${rel_path} DIRECTORY)
        install(FILES ${nix_file}
                DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/packaging/nix/${rel_dir})
    endforeach()
endif()

# Install shell scripts with executable permissions
file(GLOB SCRIPT_FILES "*.sh")
if(SCRIPT_FILES)
    install(FILES ${SCRIPT_FILES}
            DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/packaging/nix
            PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)
endif()