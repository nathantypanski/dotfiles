#!/usr/bin/env ruby
# typed: strict

# require 'sorbet-runtime'

class WifiMenu
  # extend T::Sig

  # sig { returns(T::Array[String]) }
  def self.networks
    # Start scan with user feedback
    STDERR.puts "Scanning for WiFi networks..."
    `iwctl station wlan0 scan 2>/dev/null`

    # Progressive updates with network discovery
    networks_found = []
    [1, 2, 3, 4].each do |attempt|
      STDERR.puts "Scan attempt #{attempt}/4..."
      sleep 1

      output = `iwctl station wlan0 get-networks 2>/dev/null`
      current_networks = parse_networks(output)

      if current_networks.length > networks_found.length
        networks_found = current_networks
        STDERR.puts "Found #{networks_found.length} networks..."
      end
    end

    STDERR.puts "Scan complete - #{networks_found.length} networks found"
    networks_found
  end

  # sig { params(output: String).returns(T::Array[String]) }
  def self.parse_networks(output)

    output
      .split("\n")
      .map(&:strip)
      # Remove all ANSI color codes including embedded ones
      .map { |line| line.gsub(/\e\[[0-9;]*[mK]/, '') }
      # Skip header lines and separators
      .reject { |line| line.match(/^Available networks|^-+$|^Network name|^\s*$/) }
      # Extract network names using column-based parsing
      .filter_map do |line|
        # Match lines that contain network data
        if line.match(/^\s*>?\s*.+\s+(psk|none|wep)\s+/)
          # Parse by columns - network name is from start to security column
          # Remove leading > marker and whitespace, then find where security starts
          cleaned_line = line.gsub(/^\s*>?\s*/, '')
          # Find security type position (psk, none, or wep)
          security_match = cleaned_line.match(/(.*?)\s+(psk|none|wep)\s+/)
          if security_match
            network_name = security_match[1].strip
            network_name unless network_name.nil? || network_name.empty?
          end
        end
      end
      .uniq
      .sort
  end

  # sig { void }
  def self.run
    networks.each { |ssid| puts ssid }
  end
end

WifiMenu.run if __FILE__ == $0