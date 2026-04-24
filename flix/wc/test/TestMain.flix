use Time.{Clock,TimeUnit}
use Fs.{FileSystem,FileStat,FileRead}

def runTestCase(f: Unit -> Unit \ ef): Unit \ {ef - FileSystem - FileStat - FileRead}
  = run {f()}
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
def oneLetter_shouldSetLinesToOne(): Unit \ {Assert}
  = runTestCase (_ -> {
      let filename = "test.txt";
      let _ = FileSystem.write({ str = "a" }, filename);

      match (wc(filename)) {
        case Err(e) => Assert.fail("ioError: ${e}")
        case Ok(v) => Assert.assertEq(expected = 1, v#lines)
      }
    })

@Test
def oneLetter_shouldSetWordsToOne(): Unit \ {Assert}
  = runTestCase (_ -> {
      let filename = "test.txt";
      let _ = FileSystem.write({ str = "a" }, filename);

      match (wc(filename)) {
        case Err(e) => Assert.fail("ioError: ${e}")
        case Ok(v) => Assert.assertEq(expected = 1, v#words)
      }
    })

@Test
def emptyFile_shouldSetBytesToOne(): Unit \ {Assert}
  = runTestCase(_ -> {
      let filename = "test.txt";
      let _ = FileSystem.write({ str = "" }, filename);

      match wc(filename) {
        case Err(_e) => Assert.fail("failed")
        case Ok(v) => Assert.assertEq(expected = 0, v#bytes)
      }
    })

@Test
def newline_shouldIncrementWordCount(): Unit \ {Assert}
  = runTestCase (_ -> {
      let filename = "test.txt";
      let _ = FileSystem.write({ str = "abc\ndef" }, filename);

      match (wc(filename)) {
        case Err(e) => Assert.fail("ioError: ${e}")
        case Ok(v) => Assert.assertEq(expected = 2, v#words)
      }
    })

@Test
def newline_shouldIncrementLineCount(): Unit \ {Assert}
  = runTestCase (_ -> {
      let filename = "test.txt";
      let _ = FileSystem.write({ str = "abc\ndef" }, filename);

      match (wc(filename)) {
        case Err(e) => Assert.fail("ioError: ${e}")
        case Ok(v) => Assert.assertEq(expected = 2, v#lines)
      }
    })

@Test
def repeatedSpaces_shouldBeTreatedAsOne(): Unit \ {Assert}
  = runTestCase (_ -> {
      let filename = "test.txt";
      let _ = FileSystem.write({ str = "abc   def" }, filename);

      match (wc(filename)) {
        case Err(e) => Assert.fail("ioError: ${e}")
        case Ok(v) => Assert.assertEq(expected = 2, v#words)
      }
    })

@Test
def trailingSpace_shouldNotIncrementWordCount(): Unit \ {Assert}
  = runTestCase (_ -> {
      let filename = "test.txt";
      let _ = FileSystem.write({ str = "abc " }, filename);

      match (wc(filename)) {
        case Err(e) => Assert.fail("ioError: ${e}")
        case Ok(v) => Assert.assertEq(expected = 1, v#words)
      }
    })

@Test
def multipleTrailingSpaces_shouldNotIncrementWordCount(): Unit \ {Assert}
  = runTestCase (_ -> {
      let filename = "test.txt";
      let _ = FileSystem.write({ str = "abc  " }, filename);

      match (wc(filename)) {
        case Err(e) => Assert.fail("ioError: ${e}")
        case Ok(v) => Assert.assertEq(expected = 1, v#words)
      }
    })

@Test
def trailingSpace_shouldNotIncrementLineCount(): Unit \ {Assert}
  = runTestCase (_ -> {
      let filename = "test.txt";
      let _ = FileSystem.write({ str = "abc " }, filename);

      match (wc(filename)) {
        case Err(e) => Assert.fail("ioError: ${e}")
        case Ok(v) => Assert.assertEq(expected = 1, v#lines)
      }
    })

@Test
def multipleTrailingSpaces_shouldNotIncrementLineCount(): Unit \ {Assert}
  = runTestCase (_ -> {
      let filename = "test.txt";
      let _ = FileSystem.write({ str = "abc  " }, filename);

      match (wc(filename)) {
        case Err(e) => Assert.fail("ioError: ${e}")
        case Ok(v) => Assert.assertEq(expected = 1, v#lines)
      }
    })

