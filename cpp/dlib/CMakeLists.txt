project(playground)

add_subdirectory(dlib dlib_build)


add_executable(face_detection_ex face_detection_ex.cpp)
target_link_libraries(face_detection_ex dlib::dlib)
