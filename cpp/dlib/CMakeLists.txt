cmake_minimum_required(VERSION 3.13)
project(playground)

add_subdirectory(dlib dlib_build)


add_executable(face_landmark_detection_ex face_landmark_detection_ex.modified.cpp)
target_link_libraries(face_landmark_detection_ex dlib::dlib)
