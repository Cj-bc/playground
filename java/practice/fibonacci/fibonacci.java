class fibonacci {
  static int fibonacci(int num) {
    if (num == 0) {
      return 0;
    } else if (num == 1) {
      return 1;
    }

    return fibonacci(num-1) + fibonacci(num-2);
  }


  public static void main(String[] argv) {
    System.out.println("5th Fibonacci number");
    System.out.println(fibonacci(5));
  }
}
