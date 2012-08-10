#!/usr/bin/ruby

require 'fastercsv'

cons_type = ARGV[0]
outfile = ARGV[1]

@tables = []
ARGV.drop(2).each do |f| 
  @tables << FCSV.read(f, 
      :headers => :first_row,
      :header_converters => lambda { |h| h.strip.to_sym },
      :converters => :numeric)
end

OpSum = Proc.new { |o| o.reduce(:+) }
OpMin = Proc.new { |o| o.min }
OpMax = Proc.new { |o| o.max }

def consolidate(field, op = OpSum)
  cols = @tables.map { |t| t[field] }
  cons = []
  cols.transpose.each { |c| cons << op.call(c) }
  cons
end

# overwriting tables[0] is not elegant but
# is much easier than creating a new table
cons_t = @tables[0]

case cons_type
when "-lat"
  cons_t[:n] = consolidate(:n)
  cons_t[:min] = consolidate(:min, OpMin)
  # mean of means
  cons_t[:mean] = consolidate(:mean).map { |sum| sum / @tables.size }
  #puts consolidate(:median)
  #puts consolidate(:'95th')
  #puts consolidate(:'99th')
  #puts consolidate(:'99_9th')
  cons_t[:max] = consolidate(:max, OpMax)
  cons_t[:errors] = consolidate(:errors)
when "-tp"
  cons_t[:total] = consolidate(:total)
  cons_t[:successful] = consolidate(:successful)
  cons_t[:failed] = consolidate(:failed)
end

FCSV.open(outfile, "w") do |csv|
  csv << cons_t.headers
  cons_t.each { |l| csv << l }
end
