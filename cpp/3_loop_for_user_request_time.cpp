#include <iostream>

void echo_n(int num)
{
  if ( num > 0 )
  {
    std::cout << "[" << num << "] Hello, world\n";
    return echo_n(num - 1);
  }
  else
    return;
}

int main()
{
  int times;
  std::cout << "How long to echo?: ";
  std::cin >> times;

  echo_n(times);
}
