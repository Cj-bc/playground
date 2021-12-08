#define GLEW_STATIC
#include <GL/glew.h>
#include <GLFW/glfw3.h>
#include <thread>
#include <iostream>

int main()
{
  glfwInit();

  // Initialize Window hints
  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 4);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 6);
  glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
  glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_FALSE);

  glfwWindowHint(GLFW_RESIZABLE, GL_FALSE);

  GLFWwindow* window = glfwCreateWindow(800, 600, "test OpenGL", nullptr, nullptr);

  glewExperimental = GL_TRUE;


  glfwMakeContextCurrent(window);
  // #define GL_VERSION 0x1F02
  GLenum err = glewInit();
  if (GLEW_OK != err) {
    fprintf(stderr, "Error: %s\n", glewGetErrorString(err));
  }

  // GLuint vertexBuffer;
  // glGenBuffers(1, &vertexBuffer);
  // printf("%u\n", vertexBuffer);

  while(!glfwWindowShouldClose(window)) {
    glfwSwapBuffers(window);
    glfwPollEvents();

    if (glfwGetKey(window, GLFW_KEY_SPACE) == GLFW_PRESS)
      glfwSetWindowShouldClose(window, GL_TRUE);

  }

  glfwTerminate();
}
