#include <iostream>

int main() {
  double height;
  double mass;
  double bmi;

  std::cout << "How is your height: ";
  std::cin >> height;
  std::cout << "\nHow is your mass: ";
  std::cin >> mass;

  bmi = mass / (height*height);

  std::cout << "Your BMI is: " << bmi;
}
