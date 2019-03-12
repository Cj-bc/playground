#include<string>
#include<iostream>


std::string fizzbuzz(int n) {
  if (n == 1)
    return std::to_string(n);
  else if (n % 15 == 0)
    return fizzbuzz (n-1) + " " + "fizzbuzz";
  else if (n % 5 == 0)
    return fizzbuzz (n-1) + " " + "buzz";
  else if (n % 3 == 0)
    return fizzbuzz (n-1) + " " + "fizz";
  else
    return fizzbuzz (n-1) + " " +std::to_string(n);

}

int main() {
  int max;
  std::cout << "Number?: ";
  std::cin >> max;
  std::cout << fizzbuzz(max);
}
