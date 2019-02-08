// this sample script is copyed from: http://blib.net/face_landmark_detection_ex.html

#include <dlib/image_processing/frontal_face_detector.h>
#include <dlib/image_processing/render_face_detections.h>
#include <dlib/image_processing.h>
#include <dlib/gui_widgets.h>
#include <dlib/image_io.h>
#include <iostream>
#undef DLIB_NO_GUI_SUPPORT


using namespace dlib;
using namespace std;

// --------------------


int main(int argc, char** argv)
{
  try
  {
    // This example takes in a shape model file and then a list of images to
    // process. We will take these filenames in as command line arguments.
    // Dlib comes with example images in the examples/faces folder so give
    // those as arguments to this program.
    if (argc == 1)
    {
      cout << "Call this program like this:" << endl;
      cout << "sample.cpp shape_predictor_68_face_landmarks.dat faces/*.jpeg" << endl;
      cout << "\nYou can get the shape_predictor_68_face_landmarks.dat file from:\n" << endl;
      cout << "http://dlib.net/files/shape_predictor_68_face_landmarks.dat.bz2" << endl;
      return 0;
    }

    // We need a face detector. We will use this to get bounding boxes
    // for each face in an image.
    frontal_face_detector detector = get_frontal_face_detector();
    // And we also need a shape_predictor. TZhis is the tool that will predict face
    // landmark positions given an image and face bounding box.
    // Here we are just loading the model from the shape_predictor_68_face_landmarks.dat file
    // you gave as a command line argument.
    shape_predictor sp;
    deserialize(argv[1]) >> sp;

    image_window win, win_faces;
    // Loop over all the images provided on the command line.
    for (int i = 2; i<argc;++i)
    {
      cout << "processing image" << argv[i] << endl;
      array2d<rgb_pixel> img;
      load_image(img, argv[i]);
      // Make the image larger so we can detect small faces.
      pyramid_up(img);

      // Now tell the face detector to give us a list of bounding boxes
      // around all the faces in the image.
      std::vector<rectangle> dets = detector(img);
      cout << "Number of faced detected:" << dets.size() << endl;

      // Now we will go ask the shape_predictor to tell us the pose of
      // each face we detected.
      std::vector<full_object_detection> shapes;
      for (unsigned long j = 0; j < dets.size(); ++j)
      {
        full_object_detection shape = sp(img, dets[j]);
        cout << "number of pairs:" << shape.num_parts() << endl;
        cout << "pixel position of first part:" << shape.part(0) << endl;
        cout << "pixel position of second part:" << shape.part(1) << endl;
        // You get the idea, you can get all the face part locations if
        // you want them. Here we just store them in shape so we can
        // put them on the screen.
        shapes.push_back(shape);

      }

      // Now let's view our face poses on the screen.
      win.clear_overlay();
      win.set_image(img);
      win.add_overlay(render_face_detections(shapes));

      cout << "Hit enter to process the next image..." <<endl;
      cin.get();
    }
  }
  catch (exception& e)
  {
      cout <<"\nexception thrown!" << endl;
        cout << e.what() << endl;
  }
}
