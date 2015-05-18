#!/usr/bin/env ruby

hidden_windows = `hidden -c`.split(/\n/)

map = {}
hidden_windows.each do |line|
    match = /'(.*)':'(.*)'/.match(line)
    key = match[1]
    val = match[2]
    while map.has_key? key
        key = key + '*'
    end
    map[key] = val
end

choice = nil

IO.popen('dmenu', 'r+') do |dmenu|
    map.keys.each do |name|
        dmenu.write(name)
        dmenu.write("\n")
    end
    dmenu.close_write
    choice = dmenu.gets.rstrip
end

IO.popen(map[choice])
