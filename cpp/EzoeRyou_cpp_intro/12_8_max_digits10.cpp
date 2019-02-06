#include "all.h"

int main()
{
  std::cout << "result of std::numeric_limits<T>::max_digits10" << "\n"
            << "float: " << std::numeric_limits<float>::max_digits10 << "\n"
            << "double: " << std::numeric_limits<double>::max_digits10 << "\n"
            << "long double: " << std::numeric_limits<long double>::max_digits10 << "\n";
}
