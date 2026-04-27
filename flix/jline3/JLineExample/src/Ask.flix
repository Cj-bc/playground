

pub mod Ask {
  use Sys.Console

  pub def runWithConsole(f: Unit -> a \ af): a \ af - Ask + Console
    = run { f() } with handler Ask {
      def ask(prompt, k) = k(Console.readlnWithDefault(prompt, ""))
    }


}

pub eff Ask {
  def ask(prompt: String): String
}
