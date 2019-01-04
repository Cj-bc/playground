#include <iostream>
#include <vector>
#include <string>

std::string input()
{
  std::string ret;

  std::cout << "word?('quit' to quit): ";
  std::cin >> ret;

  return ret;
}

int main()
{
  std::vector<std::string> vect;
  std::string reciever;

  while ( (reciever= input()) != "quit")
  {
    vect.push_back(reciever);
  }

  std::cout << "your input was---------\n";

  for (size_t i=vect.size(); i > 0; --i)
  {
    std::cout << vect.at(i-1) << "\n";
  }
}
