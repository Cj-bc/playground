#include <iostream>

int convert(int n)
{
  return 10110;
}

int main()
{
  int decimal;

  std::cout << "decimal number?: ";
  std::cin >> decimal;

  std::cout << "binary number is '" << convert(decimal) << "'";

}
