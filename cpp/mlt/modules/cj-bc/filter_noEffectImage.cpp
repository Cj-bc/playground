#include <framework/mlt_filter.h>
#include <stdio.h>


static int filter_get_image(mlt_frame frame, uint8_t **image, mlt_image_format *format, int *width, int *height, int writable) {
  return 0;
}

static mlt_frame filter_process(mlt_filter filter, mlt_frame frame) {
  mlt_frame_push_service(frame, filter);
  mlt_frame_push_get_image(frame, filter_get_image);
  return frame;
}

extern "C" {
  mlt_filter filter_noEffectImage_init(mlt_profile profile, mlt_service_type type, const char *id, char *arg) {
    mlt_filter filter = mlt_filter_new();

    if (filter != NULL) {
      // Get the properties
      mlt_properties properties = MLT_FILTER_PROPERTIES(filter);

      // Assign the process method
      filter->process = filter_process;


    }
    return filter;
  }
}  
