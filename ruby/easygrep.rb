def easyGrep(regex, file)
  case
  when not(File.exist?(file))
    $stderr.puts "grep: #{file}: No such file or directory"
    return
  when not(File.readable?(file))
    $stderr.puts "grep: #{file}: Cannot read file"
    return
  end

  IO.readlines(file).each do |line|
    puts line if line.match?(regex);
  end
end


if __FILE__ == $0
  if ARGV.length < 2
    $stderr.puts "Requires two arguments: REGEX, FILENAME"
    return
  end

  easyGrep(ARGV[0], ARGV[1])
end
