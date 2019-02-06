#include <iostream>

int main() {
  double height;
  double mass;
  double bmi;

  std::cout << "How is your height: ";
  std::cin >> height;
  std::cout << "How is your mass: ";
  std::cin >> mass;

  bmi = mass / (height*height);

  std::cout << "Your BMI is: " << bmi << "\n";

  auto status = []( double bmi )
  {
    if ( bmi < 18.5 )
      return "underweight";
    else if ( bmi < 25.0 )
      return "normal";
    else if ( bmi < 30.0 )
      return "overweight";
    else
      return "obase";
  };

  std::cout << "You're " << status(bmi);
}
