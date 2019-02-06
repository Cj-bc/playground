#include "all.h"

// check the size of each type
int main()
{
  auto print = [](std::size_t s)
    { std::cout << s << "\n";};

  print(sizeof(float));
  print(sizeof(double));
  print(sizeof(long double));
}
