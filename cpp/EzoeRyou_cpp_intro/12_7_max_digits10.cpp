
#include "all.h"

int main()
{
  std::cout <<  "float: "<< std::numeric_limits<float>::digits10 << "\n";
  std::cout <<  "double: "<< std::numeric_limits<double>::digits10 << "\n";
  std::cout <<  "long double: " << std::numeric_limits<long double>::digits10 << "\n";
}
