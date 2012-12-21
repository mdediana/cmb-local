#!/usr/bin/ruby

require 'csv'

if ARGV.length != 1
  puts "Usage: #{$0} res_dir"
  exit(1)
end

d = ARGV[0]
summ = "#{d}/summary.csv"
perc = "#{d}/percentiles.csv"

# Actually used:
# :consistency, :tl_mode, :w, :rw_ratio, :locality, :delay, :delay_var, :loss
FACTORS = [:consistency, :tl_mode, :w, :locality, :popularity, :rw_ratio,
           :delay, :delay_var, :loss, :dupl, :corrupt, :reorder]

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

def load_percs(f)
  h = Hash.new
  CSV.foreach(f,
      :headers => :true,
      :header_converters => lambda { |h| h.strip.to_sym },
      :converters => :numeric) do |row|
    k_perc.each { |k| h[k] = row[k].to_f / 1e6 } # us -> s
  end
  h
end

def k_perc()
  percs = Array.new
  1.upto(99) { |p| percs << "p#{p}".to_sym }
  percs << "p99_9".to_sym
end

def key(conf)
  k = ""
  FACTORS.each { |f| k << conf[f] << " " }
  k.strip!
end

def mode(conf)
  conf[:consistency] == "ev" ? "#{conf[:consistency]}#{conf[:w]}" :
                               conf[:tl_mode]
end

def factor_header()
  short = { "consistency" => "consist", "rw_ratio" => "r_w",
            "locality" => "loc", "popularity" => "pop" }
  header = (FACTORS - [:tl_mode, :w]).map { |f| f.to_s }
  header.map! { |f| short[f].nil? ? f : short[f] }
end

scenarios = Hash.new
Dir.new(d).each do |dd|
  next if dd == '.' or dd == '..' or not File.directory?("#{d}/#{dd}")

  # load
  conf = load_props(File.join(d, dd, "conf_info"))
  metrics = load_props(File.join(d, dd, "metrics"))
  unless conf[:rw_ratio] == "0:1"
    get = load_latencies(File.join(d, dd, "get-existing_latencies.csv"))
    get_perc = load_percs(File.join(d, dd, "get-existing_percentiles.csv"))
  end
  unless conf[:rw_ratio] == "1:0"
    upd = load_latencies(File.join(d, dd, "update-existing_latencies.csv"))
    upd_perc = load_percs(File.join(d, dd, "update-existing_percentiles.csv"))
  end

  # adjust
  if conf[:rw_ratio] == "1:0"
    upd = { :elapsed => 0, :n => 0, :mean => 0, :errors => 0 }
    upd_perc = Array.new(k_perc.size, 0.0)
  elsif conf[:rw_ratio] == "0:1"
    # a bit of lying since there is a get for every update
    get = { :elapsed => 0, :n => 0, :mean => 0, :errors => 0 }
    get_perc = Hash.new
    k_perc.each { |k| get_perc[k] = 0.0 }
  else
    get[:n] += upd[:n]
    r, w = conf[:rw_ratio].split(':')
    conf[:rw_ratio] = "#{r.to_i + 1}:#{w}"
  end

  scenarios[key(conf)] = { :conf => conf, :metrics => metrics,
                           :get => get, :upd => upd,
                           :get_perc => get_perc, :upd_perc => upd_perc }
end

# write summary
parent = File.dirname(d)
CSV.open("#{parent}/#{summ}", "w") do |csv|
  csv << factor_header + ["ops_s", "get", "upd", "confl", "mig", "err"]
  scenarios.each_value do |v|
    conf = v[:conf]; metrics = v[:metrics]
    get = v[:get]; upd = v[:upd]

    n = get[:n] + upd[:n]
    tp = n.to_f / get[:elapsed] # same as upd[:elapsed]
    confl = metrics[:conflicts].to_f / get[:n]
    mig = conf[:rw_ratio] == "1:0" ? 0 : metrics[:migrations].to_f / upd[:n]
    err = (get[:errors] + upd[:errors]).to_f / n

    factor_vs = (FACTORS - [:consistency, :w, :tl_mode]).map { |f| conf[f] }
    csv << [mode(conf)] +
           factor_vs +
           [tp,
            get[:mean],
            upd[:mean],
            confl,
            mig,
            err]
  end
end

# write percentiles
parent = File.dirname(d)
CSV.open("#{parent}/#{perc}", "w") do |csv|
  csv << factor_header + ["op"] + k_perc
  scenarios.each_value do |v|
    conf = v[:conf]
    get_perc = v[:get_perc]; upd_perc = v[:upd_perc]

    { "get" => v[:get_perc], "upd" => v[:upd_perc] }.each do |op, perc|
      factor_vs = (FACTORS - [:consistency, :w, :tl_mode]).map { |f| conf[f] }
      csv << [mode(conf)] +
             factor_vs +
             [op] +
             perc.values
    end
  end
end
