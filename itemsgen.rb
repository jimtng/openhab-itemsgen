#!/usr/bin/env ruby
# frozen_string_literal: true

require 'yaml'
require 'erb'

module OpenhabGenerator
  #
  # Helper Methods that can be used inside a template
  #
  module HelperMethods
    # Ignore hash value access on non-existent variables
    #
    class ::NilClass
      def [](key); end
    end

    def make_groups(*groups)
      make_list(groups.flatten.compact.uniq, outer: '()')
    end

    def make_tags(*tags)
      make_list(tags.flatten.compact, outer: '[]', inner: '"')
    end

    #
    # Convenience method to generate additional metadata from the given list of elements
    # nil arguments are allowed and silently ignored
    #
    # @param [List] *meta a list of array, or individual elements that make up the metadata
    #
    # @return [String] the metadata string prefixed with a comma, suitable for adding to an existing metadata
    #
    def add_metadata(*meta)
      metadata_array(meta.flatten.compact.uniq).join(', ').tap do |metadata|
        metadata.prepend(', ') unless metadata.empty?
      end
    end

    def make_metadata(*meta)
      make_list(metadata_array(meta.flatten.compact.uniq), outer: '{  }')
    end

    def make_list(arr, outer:, inner: nil, delimiter: ', ')
      return '' if arr.empty?

      arr.map { |entry| enclose(entry, inner) }
         .join(delimiter)
         .then { |str| enclose(str, outer) }
    end

    #
    # Convert CamelCase_ID to "Camel Case ID"
    #
    # @return [String] the humanized version of the self
    #
    def humanize(str)
      str.gsub(/[a-z](?=[A-Z])/, '\0 ').gsub('_', ' ')
    end

    #
    # Enclose string with double quotes
    #
    # @return [String] string enclosed with double quotes
    #
    def quote(str)
      %("#{str}")
    end

    #
    # Return str enclosed by the given enclosure
    # enclosure supports multiple characters to specify different left and right symbols
    # Examples: enclose('x', '"') # => "x"
    #           enclose('x', '-') # => -x-
    #           enclose('x', '{}') # => {x}
    #           enclose('x', '[]') # => [x]
    #           enclose('x', '[  ]') # => [ x ]
    #           enclose('x', '<%%>') # => <%x%>
    #
    # @param [String] str the string to be enclosed
    # @param [String] enclosure symbols used to enclose the string. nil enclosure will return the original string
    #
    # @return [String] the string enclosed with the given enclosure
    #
    def enclose(str, enclosure)
      return str unless enclosure

      if enclosure.length.odd?
        left = right = enclosure
      else
        left, right = enclosure.chars.each_slice(enclosure.length / 2).map(&:join)
      end
      "#{left}#{str}#{right}"
    end

    private

    def metadata_array(arr)
      arr.map do |entry|
        case entry
        when Hash then metadata_from_hash(entry)
        when String then entry
        end
      end.flatten
    end

    def metadata_from_hash(hash)
      hash.map do |key, value|
        # if %w[true false].include? value
        #   %(#{key}=#{value})
        # else
        %(#{key}="#{value}")
        # end
      end
    end
  end

  # Template caching and path resolution
  class Template
    @templates = {}

    def self.get(name, template_dir: nil)
      @templates[name] ||= load(name, template_dir)
    end

    def self.load(name, template_dir)
      template_dir ||= 'templates/'
      filename = File.join(template_dir, "#{name}.erb")
      template = File.read(filename)
      ERB.new(template, trim_mode: '%>')
    end
  end

  # Splits the given multi-line string into things and items
  module Splitter
    def self.things_items(src)
      @things_nest_level = 0
      @things = []
      @items = []
      @comments = []
      process_src_lines(src)
      [@things.join("\n"), @items.join("\n")]
    end

    def self.process_src_lines(src)
      src.lines.map(&:rstrip).reject(&:empty?).each do |line|
        case line
        when %r{^\s*//}
          process_comment(line)
        when /^\s*(Thing|Bridge)\s+/
          process_thing_bridge(line)
        else
          process_else(line)
        end
      end
    end

    def self.process_comment(line)
      @comments << line
    end

    def self.process_thing_bridge(line)
      @things.concat @comments.slice!(0..), [line]
      @things_nest_level += 1 if line[-1] == '{'
    end

    def self.process_else(line)
      if @things_nest_level.positive?
        @things.concat @comments.slice!(0..), [line]
        @things_nest_level -= 1 if line[-1] == '}'
      else
        @items.concat @comments.slice!(0..), [line]
      end
    end
  end

  #
  # Methods to format items and things file
  #
  module Formatter
    # rubocop: disable Metrics/MethodLength
    def self.align_items(items)
      regex = /
      ^\s*
        (?<type>\S+)\s+
        (?<name>\S+)\s+
        (?<label>"[^"]*")?\s*
        (?<icon><[^>]*>)?\s*
        (?<group>\([^)]*\))?\s*
        (?<tag>\[[^\]]*\])?\s*
        (?<metadata>\{.*)?
        $
      /x
      align_fields(items, regex) do |line|
        line[6] = format_metadata(line[6]) if line.is_a?(Array) && line.length >= 7
        line
      end
    end
    # rubocop: enable Metrics/MethodLength

    def self.align_things(things)
      regex = /
      ^(?<space>\s*)
        (?<type_label>Type)\s+
        (?<type>\S+)\s*
        (?<colon>:)\s*
        (?<name>\S+)?\s*
        (?<params>\[.*)
        $
      /x
      align_fields(things, regex)
    end

    #
    # Align the fields within the string based on the given splitter regex
    # Use the given block to format each field
    #
    # @param [String] str a multi-line string
    # @param [Regexp] regex that splits the string into multiple fields
    # @param [Block] &block an optional block that can format each field
    #
    # @return [String] return the string with its fields aligned
    #
    def self.align_fields(str, regex, &block)
      # split lines into array of fields for each line, or a string if it doesn't match the pattern
      tokenized_lines = str.lines.map(&:chomp).map { |line| split_line(line, regex) }

      tokenized_lines = tokenized_lines.map(&block) if block_given?

      field_widths = calculate_field_widths(tokenized_lines)

      # reassemble the array of lines, padding each field to the max width of that field
      tokenized_lines.map { |line| pad_line(line, field_widths) }.join("\n")
    end

    def self.split_line(line, regex)
      match = regex.match(line)
      match ? match.to_a.slice(1..).map(&:to_s) : line
    end

    def self.pad_line(line, field_widths)
      if line.is_a?(Array)
        line.each_with_index.map { |field, i| field.ljust(field_widths[i]) }.reject(&:empty?).join(' ')
      else
        line
      end
    end

    def self.calculate_field_widths(lines)
      lines.grep(Array)
           .transpose
           .map { |field| field.map(&:length) }
           .map(&:max)
           .tap { |arr| arr[-1] = 0 if arr.any? } # don't pad the last field
    end

    ## TODO: nicely format the metadata, adding / removing excess spaces, etc
    ## The metadata is in the format of { key="value", key2="value" [opt1="x", opt2="y"] }
    def self.format_metadata(str)
      str
    end
  end

  # A class to render the template for one device
  # Also provides convenience methods to be used from inside the template
  # because ERB's binding is derived from this class' instance
  # To use it, simply use the static Device.output method.
  class Device
    include HelperMethods

    def initialize(id, details)
      raise KeyError, "No template was specified for #{id}" unless details['template']

      @id = id
      @details = details
    end

    #
    # Apply the template and render the given id and details
    #
    # @param [<Type>] id <description>
    # @param [<Type>] details <description>
    #
    # @return [Array] An array with two elements: [ rendered_things, rendered_items ]
    #
    def self.output(id, details, template_dir: nil)
      Splitter.things_items Device.new(id, details).parse(template_dir: template_dir)
    end

    def parse(template_dir: nil)
      template_obj(template_dir: template_dir).result(binding)
    end

    def template_obj(template_dir: nil)
      Template.get(template_name, template_dir: template_dir)
    end

    def template_name
      @details['template']
    end

    def thingid
      @details['thingid'] || @id.downcase.gsub('_', '-')
    end

    def name
      @details['name'] || @id
    end

    def label
      @details['label'] || humanize(name)
    end

    def room
      @details['room'] || humanize(name_parts[0])
    end

    def name_parts
      name.split('_')
    end

    # return the missing method/variable from the details data
    def method_missing(method, *args)
      if @details.respond_to? method
        @details.send(method, *args)
      else
        @details[method.to_s]
      end
    end

    def respond_to_missing?(_method_name, _include_private = false)
      true
    end
  end

  #
  # Process devices/settings
  # render each device into output
  # combine them, then write to file
  #
  class Devices
    attr_accessor :items_header, :things_header, :template_dir
    attr_reader :output

    def initialize(input_hash)
      @settings = input_hash.delete('settings')
      @devices = input_hash
      @items_header = @settings&.dig('items', 'header') || ''
      @things_header = @settings&.dig('things', 'header') || ''
      @output = {}
    end

    #
    # Render the templates and write the output to the given file paths
    # or use the paths from the template not specified
    #
    # @return [Hash] returns the generated items and things as { 'things' => data, 'items' => data }
    #
    def generate
      render_devices(@devices) { |id, data, output| yield(id, data, output) if block_given? }.map do |type, value|
        [type, format_content(type, value).prepend(header_for_type(type))]
      end.to_h
    end

    def items_file
      @settings&.dig('items', 'output_file')
    end

    def things_file
      @settings&.dig('things', 'output_file')
    end

    private

    def header_for_type(type)
      case type
      when 'items' then items_header
      when 'things' then things_header
      else ''
      end
    end

    def render_devices(devices)
      # map { id1 => details, id2 => details,...} hash into:
      # [ [rendered_things1, rendered_items2], [rendered_things2, rendered_items2], ... ]
      # then transpose it into:
      # [ [rendered_things1, rendered_things2, ...], [rendered_items1, rendered_items2, ...]]
      output = devices.map do |id, details|
        Device.output(id, details, template_dir: @template_dir).tap { |out| yield(id, details, out) if block_given? }
      end.transpose

      { 'things' => output[0], 'items' => output[1] }
    end

    def format_content(type, content)
      content = content.join("\n\n")
      case type
      when 'items' then Formatter.align_items(content)
      when 'things' then Formatter.align_things(content)
      else content
      end
    end
  end
end

if __FILE__ == $PROGRAM_NAME
  def overwrite_ok?(filename, force)
    if !force && File.exist?(filename)
      puts "WARNING: Output file exists: '#{filename}'. Aborting."
      puts 'Specify --force to overwrite'
      return false
    end
    true
  end

  require 'optparse'

  options = {}
  optparser = OptionParser.new do |opt|
    opt.banner = "Usage: #{$PROGRAM_NAME} [options] yamlfile"
    opt.on('-t', '--things THINGS_FILE', 'The path to things file output') { |o| options[:things_file] = o }
    opt.on('-i', '--items  ITEMS_FILE', 'The path to items file output') { |o| options[:items_file] = o }
    opt.on('-v', '--verbose', 'Print details') { |o| options[:verbose] = o }
    opt.on('-n', '--dry-run', 'Run process but do not write to output files') { |o| options[:dry_run] = o }
    opt.on('-f', '--force', 'Overwrite output files') { |o| options[:force] = o }
    opt.on('-d', '--template-dir PATH', 'Path to look for the template files') { |o| options[:template_dir] = o }
    opt.on('-h', '--help', 'Print this help') do
      puts opt
      exit(-1)
    end
  end

  optparser.parse!

  if ARGV.empty?
    puts 'Missing YAML file argument'
    puts optparser.help
    exit(-1)
  end
  yaml = YAML.load_file(ARGV[0])
  device_count = 0
  gen = OpenhabGenerator::Devices.new(yaml)
  items_file = options[:items_file] || gen.items_file
  things_file = options[:things_file] || gen.things_file
  gen.template_dir = options[:template_dir] # nil will use the default 'templates/' path
  begin
    output = gen.generate do |id, details, _output|
      puts "Processed: #{id}, template: #{details['template']}" if options[:verbose]
      device_count += 1
    end
  rescue Errno::ENOENT => e
    puts e.message
    exit(-1)
  end

  puts "Finished processing #{device_count} entries" if options[:verbose]

  if options[:dry_run]
    puts 'Dry run specified. Skip writing to output file.'
    exit 0
  end

  [
    { name: 'Things',
      file: things_file,
      data: output['things'] },
    { name: 'Items',
      file: items_file,
      data: output['items'] }
  ].each do |entity|
    if entity[:file]
      exit(-1) unless overwrite_ok? entity[:file], options[:force]
      File.write(entity[:file], entity[:data])
      puts "#{entity[:name]} written to #{entity[:file]}"
    else
      puts "#{entity[:name]} file not specified"
    end
  end
end
