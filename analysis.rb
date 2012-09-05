#!/usr/bin/ruby

require 'csv'

f = ARGV[0]

class Numeric
  def variation(x)
    (x - self).to_f / self
  end
end

class Scenario
  attr_accessor :results

  def initialize(row)
    @results = Hash.new
  end

  def self.group_by(row, *params)
    initial = params.shift
   params.reduce(row[initial].to_s) do |k, p|
      k + '_' + row[p].to_s
    end
  end
  
  def add_result(row, metric, *params)
    key = Scenario.group_by(row, *params)
    @results[key] = row[metric]
  end

  def sorted_results
    @results.to_a.sort { |x, y| x[1] <=> y[1] }
  end
end

@scenarios = Hash.new
CSV.foreach(f,
    :headers => :true,
    :header_converters => lambda { |h| h.strip.to_sym },
    :converters => :numeric) do |row|
  #key = Scenario.group_by(row, :consist, :r_w, :delay)
  key = Scenario.group_by(row, :consist, :r_w, :loc, :pop)
  scenario = @scenarios[key]
  scenario ||= Scenario.new(row)
  scenario.add_result(row, :ops_s, :delay)
  @scenarios[key] = scenario
end

def print_comp(scenario)
  r = scenario.sorted_results
  (0..r.size - 1).each do |i|
    if i != 0 then
      diff = r[i][1].variation(r[i - 1][1]).abs
      print(diff < 0.05 ? '=' : '<')
    end
    printf(" %s ", r[i][0])
  end
end

def print_ops_s(scenario)
  scenario.results.collect { |v| printf("%6d ", v[1]) }
end

def print_pop_diff(scenario)
  r = scenario.results
  diff_0_5 = r["0.5_uni"].variation(r["0.5_par"])
  diff_0_9 = r["0.9_uni"].variation(r["0.9_par"])
  printf("%4.2f %4.2f", diff_0_5, diff_0_9)
end

def print_loc_diff(scenario)
  r = scenario.results
  diff_uni = r["0.5_uni"].variation(r["0.9_uni"])
  diff_par = r["0.5_par"].variation(r["0.9_par"])
  printf("%4.2f %4.2f", diff_uni, diff_par)
end

def print_delay_diff(scenario)
  r = scenario.results
  diff_100 = r["0"].variation(r["100"])
  diff_200 = r["0"].variation(r["200"])
  diff_300 = r["0"].variation(r["300"])
  printf("%4.2f %4.2f %4.2f", diff_100, diff_200, diff_300)
end

@scenarios.each do |k, v|
  printf("%-15s ", k)
  print_ops_s(v)
  print_delay_diff(v)
  puts
end

