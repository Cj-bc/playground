use Fs.FileRead
use Fs.FileStat
use Sys.Env
use Sys.Console

// The main entry point.
def main(): Unit \ { FileRead, FileStat, Env, Console } =
  Env.getArgs() |> ForEach.forEach(fname ->
 match (wc(fname)) {
    case Ok(stat) => Console.println("${stat#words} ${stat#lines} ${stat#bytes} ${fname}")
    case Err(err) => Console.eprintln(ToString.toString(err))
  })

def wc(fpath: String): Result[IoError, WcStat] \ { FileRead, FileStat }
  = forM (
        _exists <- FileStat.exists(fpath);
        _isRegular <- FileStat.isRegularFile(fpath);
        _isReadable <- FileStat.isReadable(fpath);
        contents <- FileRead.read(fpath)
    ) yield {
      // if (exists and isRegular qnd isReadable)
      let c = String.foldLeft((counter, c) -> match (c) {
           case '\n' => { lines = counter#lines + 1, words = counter#words + 1, wasSpace = true | counter }
           case ' ' if (not counter#wasSpace) => { words = counter#words + 1, wasSpace = true | counter }
           case ' ' => { wasSpace = true | counter }
           case _ => { wasSpace = false | counter }
        }, { lines = 0, words = 0, wasSpace = false }, contents);
      let length = contents.length();

      { lines = if (length == 0) 0 else c#lines + 1
      , words = if (length == 0) 0 else c#words + 1
      , bytes = length }
    }
     


type alias WcStat = { lines = Int32, words = Int32, bytes = Int32 }

 
