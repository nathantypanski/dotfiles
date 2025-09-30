#!/usr/bin/env ruby
# typed: strict

require 'sorbet-runtime'

class Launcher
  extend T::Sig

  sig { returns(T::Array[String]) }
  def self.commands
    # Get actual executables from PATH
    paths = ENV['PATH'].split(':')
    commands = []

    paths.each do |path|
      next unless File.directory?(path)
      Dir.foreach(path) do |file|
        filepath = File.join(path, file)
        if File.executable?(filepath) && File.file?(filepath)
          commands << file
        end
      end
    end

    commands.uniq.sort.reject { |cmd| cmd.include?('fzf') }
  end

  sig { void }
  def self.run
    commands.each { |cmd| puts cmd }
  end
end

Launcher.run if __FILE__ == $0
