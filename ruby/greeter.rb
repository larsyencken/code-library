#!/usr/bin/env ruby

class Greeter
  attr_accessor :names
  
  def initialize(names = "World")
    @names = names
  end
  
  def say_hi
    if @names.nil?
      puts "..."
    elsif @names.respond_to?("each")
      @names.each do |name|
        puts "Hello #{name}!"
      end
    else
      puts "Hello #{@names}!"
    end
  end
  
  def say_bye
    if @names.nil?
      puts "..."
    elsif @names.respond_to?("join")
      puts "Goodbye #{@names.join(", ")}. Come back soon."
    else
      puts "Goodbye #{@names}. Come back soon."
    end
  end
end

if __FILE__ == $0
  g = Greeter.new
  g.say_hi
  g.say_bye
  
  g.names = "Zeke"
  g.say_hi
  g.say_bye
  
  g.names = ["Lars", "Lauren", "Laura", "Loraine"]
  g.say_hi
  g.say_bye
  
  g.names = nil
  g.say_hi
  g.say_bye
end