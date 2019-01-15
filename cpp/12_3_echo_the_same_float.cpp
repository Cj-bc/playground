

int main()
{
  auto print = [](float a)
  {std::cout << a << "\n";};
  float a = 12.345f;
  float b = 1.2345e1f;
  float c = 12345e-3f;

  print(a);
  print(b);
  print(c);
}
