#!/usr/bin/env ruby
# typed: strict

require 'sorbet-runtime'

class WifiMenu
  extend T::Sig

  sig { returns(T::Array[String]) }
  def self.networks
    # Use iwctl to scan and list networks
    `iwctl station wlan0 scan 2>/dev/null; sleep 1; iwctl station wlan0 get-networks 2>/dev/null`
      .split("\n")
      .map(&:strip)
      .select { |line| line.match(/^\s*[>*]?\s*\S/) && !line.match(/^---+/) && !line.match(/Available networks/) }
      .map { |line| line.gsub(/^\s*[>*]?\s*/, '').split(/\s+/).first }
      .reject(&:empty?)
      .uniq
      .sort
  end

  sig { void }
  def self.run
    networks.each { |ssid| puts ssid }
  end
end

WifiMenu.run if __FILE__ == $0