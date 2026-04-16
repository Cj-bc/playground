use Time.{Clock,TimeUnit}
use Fs.{FileSystem,FileStat,FileRead}

def runTestCase(f: Unit -> Unit \ ef): Unit \ {ef - FileSystem - FileStat - FileRead}
  = run {f() 
    with FileStat.runWithFileSystem
    with FileRead.runWithFileSystem
    with FileSystem.withInMemoryFS
    with handler Clock {
      def currentTime(u, k) = match (u) {
        case TimeUnit.Days => k(0i64)
        case TimeUnit.Hours => k(0i64)
        case TimeUnit.Microseconds => k(0i64)
        case TimeUnit.Milliseconds => k(0i64)
        case TimeUnit.Minutes => k(0i64)
        case TimeUnit.Nanoseconds => k(0i64)
        case TimeUnit.Seconds => k(0i64)
      }
    }

@Test
def oneFile_shouldReturnCorrectly(): Unit \ Assert
  = runTestCase(_ -> {
      let filename = "test.txt";
      let _ = FileSystem.write({ str = "test" }, filename);

      match wc(filename) {
        case Err(_e) => Assert.fail("failed")
        case Ok(v) => {
          Assert.assertEq(expected = 1, v#lines);
          Assert.assertEq(expected = 1, v#words);
          Assert.assertEq(expected = 4, v#bytes)
        }
      }
    })

@Test
def emptyFile_shouldReturnCorrectly(): Unit \ {Assert}
  = runTestCase(_ -> {
      let filename = "test.txt";
      let _ = FileSystem.write({ str = "" }, filename);

      match wc(filename) {
        case Err(_e) => Assert.fail("failed")
        case Ok(v) => {
          Assert.assertEq(expected = 0, v#lines);
          Assert.assertEq(expected = 0, v#words);
          Assert.assertEq(expected = 0, v#bytes)
        }
      }
    })
