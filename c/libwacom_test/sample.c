#include <libwacom/libwacom.h>
#include <stdio.h>

WacomDeviceDatabase *db;
WacomDevice *device;
WacomError *error;


int main() {
  db = libwacom_database_new();
  error = libwacom_error_new();
  if (!db) {
    fprintf(stderr, "Failed to initialize database\n");
    return -1;
  }
  device = libwacom_new_from_path(db, "/dev/input/event0", WFALLBACK_NONE, NULL);
  
  if (!device)
    fprintf(stderr, "No device was found\n");
    return -1; // should check for error here
  
  if (libwacom_is_builtin(device))
    printf("This is a built-in device\n");
  
  libwacom_destroy(device);
  libwacom_database_destroy(db);
}
