#!/usr/bin/env ruby
# typed: strict

require 'sorbet-runtime'

class SystemMenu
  extend T::Sig

  ACTIONS = T.let({
    "Lock Screen" => "system-swaylock",
    "Suspend" => "systemctl suspend", 
    "Reboot" => "systemctl reboot",
    "Shutdown" => "systemctl poweroff",
    "Reload River" => "sh ~/.config/river/init",
    "Rebuild Home" => "rebuild-home"
  }.freeze, T::Hash[String, String])

  sig { void }
  def self.run
    ACTIONS.keys.each { |action| puts action }
  end

  sig { params(action: String).void }
  def self.execute(action)
    command = ACTIONS[action]
    if command
      if command == "rebuild-home"
        system("foot --title=rebuild-home -e #{command}")
      else
        system(command)
      end
    end
  end
end

if ARGV.empty?
  SystemMenu.run
else
  SystemMenu.execute(ARGV[0])
end