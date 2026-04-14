use Fs.FileRead
use Fs.FileStat
use Sys.Env

// The main entry point.
def main(): Unit \ { IO, FileRead, FileStat, Env } =
  Env.getArgs() |> ForEach.forEach(fname ->
 match (wc(fname)) {
    case Ok(stat) => println("${stat#words} ${stat#lines} ${stat#bytes} ${fname}")
    case Err(err) => println(err)
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
           case '\n' => { lines = counter#lines + 1, wasSpace = false | counter }
           case ' ' if (not counter#wasSpace) => { words = counter#words + 1, wasSpace = true | counter }
           case _ => { wasSpace = false | counter }
        }, { lines = 1, words = 1, wasSpace = false }, contents);
      { lines = c#lines, words = c#words, bytes = contents.length() }
    }
     


type alias WcStat = { lines = Int32, words = Int32, bytes = Int32 }

 
