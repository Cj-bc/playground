

int main()
{
  //仮数部の有効桁数
  std::cout << "float: " << std::numeric_limits<float>::digits10 << "\n"
            << "double: " << std::numeric_limits<double>::digits10 << "\n"
            << "long double: " << std::numeric_limits<long double>::digits10 << "\n";
}
