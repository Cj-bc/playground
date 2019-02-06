#include "all.h"

int main()
{
  std::cout << "plus: " << 0.0 << "\n";
  std::cout << "minus: " << -0.0 << "\n";

  float plus = 0.0f;
  float minus = -0.0f;
  bool is_same = plus == minus;

  std::cout << "plus == minus: " << is_same << "\n";
    
}
