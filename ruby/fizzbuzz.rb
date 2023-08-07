def fizzBuzz(num)
  puts case
       # Prevent num = 0 from determined as "fizzBuzz"
       when num.zero?
         "0"
       when num % 15 == 0
         "fizzbuzz"
       when num % 3 == 0
         "fizz"
       when num % 5 == 0
         "buz"
       else
         num.to_s
       end

  if num > 0 then
    fizzBuzz(num-1)
  end
end


if __FILE__ == $0
  begin
    num = Integer(ARGV[0])
    if ARGV.length > 0
      fizzBuzz(num)
    else
      puts "Please supply number as first argument"
    end
  rescue ArgumentError
    puts "Failed to convert ARGV[1] to integer"
  end
end
