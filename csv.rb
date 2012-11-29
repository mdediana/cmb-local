#!/usr/bin/ruby

require 'csv'

if ARGV.length != 1
  puts "Usage: #{$0} res_dir"
  exit(1)
end

d = ARGV[0]
summ = "#{d}/summary.csv"
perc = "#{d}/percentiles.csv"

def load_props(f)
  h = Hash.new
  File.read(f).each_line do |l|
    kv = l.chomp.split('=')
    h[kv[0].to_sym] = kv[1]
  end
  h[:delay] = (2 * h[:delay].to_i).to_s # rtt delay
  h
end

def load_latencies(f)
  h = Hash.new
  CSV.foreach(f,
      :headers => :true,
      :header_converters => lambda { |h| h.strip.to_sym },
      :converters => :numeric) do | row |
    [:elapsed, :n, :errors].each { |k| h[k] = row[k] }
    h[:mean] = row[:mean] / 1e6 # us -> s
  end
  h
end

def key(conf)
  k = ""
  [:consistency, :tl_mode, :w, :rw_ratio, :locality, :popularity,
   :delay].each { | p | k << conf[p] << " " }
  k.strip!
end

def mode(conf)
  conf[:consistency] == "ev" ? "#{conf[:consistency]}#{conf[:w]}" :
                               conf[:tl_mode]
end

scenarios = Hash.new
Dir.new(d).each do |dd|
  next if dd == '.' or dd == '..' or not File.directory?("#{d}/#{dd}")

  # load
  conf = load_props(File.join(d, dd, "conf_info"))
  metrics = load_props(File.join(d, dd, "metrics"))
  unless conf[:rw_ratio] == "0:1"
    get = load_latencies(File.join(d, dd, "get_latencies.csv"))
    get_perc = IO.readlines(File.join(d, dd, "get_percentiles.csv"))[-1].chomp
  end
  unless conf[:rw_ratio] == "1:0"
    upd = load_latencies(File.join(d, dd, "update-existing_latencies.csv"))
    upd_perc = IO.readlines(
            File.join(d, dd, "update-existing_percentiles.csv"))[-1].chomp
  end

  # adjust
  if conf[:rw_ratio] == "1:0"
    upd = { :elapsed => 0, :n => 0, :mean => 0, :errors => 0 }
  elsif
    conf[:rw_ratio] == "0:1" # do nothing, see below
  else
    upd[:mean] -= get[:mean]
    get[:n] += upd[:n]
    r, w = conf[:rw_ratio].split(':')
    conf[:rw_ratio] = "#{r.to_i + 1}:#{w}"
  end

  scenarios[key(conf)] = { :conf => conf, :metrics => metrics,
                           :get => get, :upd => upd,
                           :get_perc => get_perc, :upd_perc => upd_perc }
end

# adjust 0:1
scenarios.each do |k, v|
  next unless v[:conf][:rw_ratio] == "0:1"

  k_2_1 = key(v[:conf]).sub(/0:1/, "2:1")
  v[:get] = scenarios[k_2_1][:get].clone
  v[:upd][:mean] -= v[:get][:mean]
  v[:conf][:rw_ratio] = "1:1"
end

# write summary
parent = File.dirname(d)
CSV.open("#{parent}/#{summ}", "w") do |csv|
  csv << ["consist","r_w", "loc", "pop", "delay",
          "ops_s", "get", "upd", "confl", "mig", "err"]
  scenarios.each_value do |v|
    conf = v[:conf]; metrics = v[:metrics]
    get = v[:get]; upd = v[:upd]

    n = get[:n] + upd[:n]
    tp = n.to_f / get[:elapsed] # same as upd[:elapsed]
    confl = metrics[:conflicts].to_f / get[:n]
    mig = conf[:rw_ratio] == "1:0" ? 0 : metrics[:migrations].to_f / upd[:n]
    err = (get[:errors] + upd[:errors]).to_f / n

    csv << [mode(conf),
            conf[:rw_ratio],
            conf[:locality],
            conf[:popularity],
            conf[:delay],
            tp,
            get[:mean],
            upd[:mean],
            confl,
            mig,
            err]
  end
end

# write percentiles
File.open(perc, "w") do |f|
  # header
  conf_header = "consist,r_w,loc,pop,delay"
  perc_header = ""
  1.upto(99) { |p| perc_header << "p#{p}," }
  perc_header << "p99.9"
  f.write("#{conf_header},op,#{perc_header}\n")

  # values
  scenarios.each_value do |v|
    conf_csv = "#{mode(v[:conf])},#{v[:conf][:rw_ratio]}," + 
               "#{v[:conf][:locality]},#{v[:conf][:popularity]}," + 
               "#{v[:conf][:delay]}"

    f.write("#{conf_csv},get,#{v[:get_perc]}\n") if not v[:get_perc].nil?
    f.write("#{conf_csv},upd,#{v[:upd_perc]}\n") if not v[:upd_perc].nil?
  end
end
