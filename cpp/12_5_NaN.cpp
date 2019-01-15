#include "all.h"

int main()
{
  double NaN = std::numeric_limits<double>::quiet_NaN();
  std::cout << "NaN: " << NaN << "\n";

  bool b = 12.345 == NaN;
  std::cout << "double == NaN: " << b << "\n";
}
