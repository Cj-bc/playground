#include <iostream>

// convert decimal to binary
int solve(int n)
{
  if (n <= 1)
    return n;
  else
    return n%2 + 2 * solve(n/10);
}


int convert(int n)
{
  if (n > 0)
    return solve(n);
  else
    return - solve(-n);
}

int main()
{
  int decimal;

  std::cout << "decimal number?: ";
  std::cin >> decimal;

  std::cout << "binary number is '" << convert(decimal) << "'";

}
