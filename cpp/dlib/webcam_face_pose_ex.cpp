#include <dlib/opencv.h>
#include <opencv2/highgui/highgui.hpp>
#include <dlib/image_processing/frontal_face_detector.h>
#undef DLIB_NO_GUI_SUPPORT
#include <dlib/image_processing/render_face_detections.h>
#include <dlib/gui_widgets.h>
#include <iostream>
#include <exception>
#include <vector>

using namespace dlib;

int main()
{
  try
  {
    cv::VideoCapture cap(0);
    if (!cap.isOpened())
    {
      std::cerr << "Unable to connect to camera" << std::endl;
      return 1;
    }

    dlib::image_window win;

    // Load face detection and pose estimation models.
    dlib::frontal_face_detector detector = dlib::get_frontal_face_detector();
    dlib::shape_predictor pose_model;
    dlib::deserialize("shape_predictor_68_face_landmarks.dat") >> pose_model;

    // Grab and process frames until the main window is closed by the user.
    while (!win.is_closed())
    {
      // Grab a frame
      cv::Mat temp;
      if (!cap.read(temp))
      {
        break;
      }
      // Turn OpenCV's Mat into something dlib can deal with. Nothe that this just
      // wraps the Mat object, it doesn't copy anything. So cimg is onlu valid as
      // long as temp is valid. Also don't do anything to temp that whould cause it
      // to reallocate the moemory which stores the image as that will make cimg
      // contain dangling pointers. This basically means you shouldn't modify temp
      // while using cimg.
      dlib::cv_image<dlib::bgr_picel> cimg(temp);

      // Detect faces
      std::vector<dlib::rectangle> faces = detector(cimg);
      // Find the pose of each face.
      std::vector<dlib::full_object_detection> shapes;
      for (unsigned long i = 0;i < faces.size(); ++i)
        shapes.push_back(pose_model(cimg, faces[i]));

      // Display it all on the screen
      win.clear_overlay();
      win.set_image(cimg);
      win.add_overlay(render_face_detections(shapes));
    }


  }
  catch(std::exception& e)
  {
    std::cout << e.what() << std::endl;
  }
}
