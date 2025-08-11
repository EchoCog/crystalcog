# ROS OpenCog Robot Embodiment
cmake_minimum_required(VERSION 3.10)

project(ros_opencog_robot_embodiment)

# Install ROS robot embodiment files
file(GLOB_RECURSE ROS_FILES "*")
if(ROS_FILES)
    foreach(ros_file ${ROS_FILES})
        file(RELATIVE_PATH rel_path ${CMAKE_CURRENT_SOURCE_DIR} ${ros_file})
        get_filename_component(rel_dir ${rel_path} DIRECTORY)
        install(FILES ${ros_file}
                DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/ros_opencog_robot_embodiment/${rel_dir})
    endforeach()
endif()