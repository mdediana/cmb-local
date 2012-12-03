#!/usr/bin/ruby

d="res.1"

lines = File.readlines("#{d}/percentiles.csv").sort

File.open("#{d}/percentiles_mixed.csv",'w') do |f|
  f.write(lines[0].gsub(",op", ''))
  1.step(lines.size - 1, 2) do |i|
    # l1 - gets, l2 - puts
    l1, l2 = [lines[i], lines[i + 1]].map { |l| l.split(',') }

    f.write("#{l1[0..4].join(',')}")

    r, w = l1[1].split(':')
    6.upto(105) do |j|
      get, upd = [l1[j], l2[j]].map { |v| v.to_f }
      if get == 5e6 || upd == 5e6
        f.write(",NA")
      else
        p = (r.to_i * l1[j].to_f + w.to_i * l2[j].to_f) / (r.to_i + w.to_i)
        f.write(",#{p.round(1)}")
      end
    end
    f.write("\n")
  end
end
