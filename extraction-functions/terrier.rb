#!/usr/bin/env ruby

# quit unless our script gets one argument
unless ARGV.length == 1
  puts "Just input a doi.\n"
  exit
end

doi = "%s" % ARGV[0]

doi.sub!("_", "/")

# file is hardcoded into this

require 'terrier'
require 'csv'

begin
	temp = Terrier.new('doi:%s' % doi)
rescue
	puts 'Some error occurred, putting all metadata to NA'
	journal = 'NA'
	year = 'NA'
end

if temp.citation_data[:publication_year]
	year = temp.citation_data[:publication_year]
else
	year = 'NA'
end

if temp.citation_data[:journal]
	journal = temp.citation_data[:journal]
else
	journal = 'NA'
end


CSV.open('data/metadata/%s.csv' % doi.sub!("/", "_"), "wb") do |csv|
	csv << [journal, year]
end
