#!/usr/bin/ruby

require 'csv'

d = ARGV[0]
out = "#{d}/res.csv"

def load_props(f)
  h = Hash.new
  File.read(f).each_line do |l|
    kv = l.chomp.split('=')
    h[kv[0].to_sym] = kv[1]
  end
  h[:tl_mode] = "lat" if h[:tl_mode] == "latest"
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
  [:consistency, :tl_mode, :rw_ratio, :locality, :popularity,
   :delay].each { | p | k << conf[p] << " " }
  k.strip!
end

scenarios = Hash.new
Dir.new(d).each do |dd|
  next if dd == '.' or dd == '..' or not File.directory?("#{d}/#{dd}")

  # load
  f = File.join(d, dd, "conf_info")
  conf = load_props(f)
  f = File.join(d, dd, "metrics")
  metrics = load_props(f)
  f = File.join(d, dd, "get_latencies.csv")
  get = load_latencies(f) unless conf[:rw_ratio] == "0:1"
  f = File.join(d, dd, "update-existing_latencies.csv")
  upd = load_latencies(f) unless conf[:rw_ratio] == "1:0"

  # adjust
  if conf[:rw_ratio] == "1:0"
    upd = { :elapsed => 0, :n => 0, :mean => 0, :errors => 0 }
  elsif conf[:rw_ratio] == "0:1" # do nothing, see below
  else
    upd[:mean] -= get[:mean]
    get[:n] += upd[:n]
    r, w = conf[:rw_ratio].split(':')
    conf[:rw_ratio] = "#{r.to_i + 1}:#{w}"
  end

  scenarios[key(conf)] = { :conf => conf, :metrics => metrics,
                           :get => get, :upd => upd }
end

# adjust 0:1
scenarios.each do |k, v|
  next unless v[:conf][:rw_ratio] == "0:1"

  k_2_1 = key(v[:conf]).sub(/0:1/, "2:1")
  v[:get] = scenarios[k_2_1][:get].clone
  v[:upd][:mean] -= v[:get][:mean]
  v[:conf][:rw_ratio] = "1:1"
end

# print
parent = File.dirname(d)
CSV.open("#{parent}/#{out}", "w") do |csv|
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

    csv << ["#{conf[:consistency]}(#{conf[:tl_mode]})",
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
