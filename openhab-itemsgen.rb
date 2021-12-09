require 'yaml'
require 'erb'

# Template caching and path resolution
class Template
  @templates = {}

  def self.get(name)
    @templates[name] ||= load(name)
  end

  def self.load(name)
    template = load_template_file(template_filename(name))
    ERB.new(template, trim_mode: '%>')
  end

  def self.template_filename(name)
    "templates/#{name}.erb"
  end

  def self.load_template_file(filename)
    File.read(filename)
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
      return line unless line.is_a?(Array)

      line[6] = format_metadata(line[6]) if line.length >= 7
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
    line.is_a?(Array) ? line.each_with_index.map { |field, i| field.ljust(field_widths[i]) }.join(' ') : line
  end

  def self.calculate_field_widths(lines)
    lines.grep(Array)
         .transpose
         .map { |field| field.map(&:length) }
         .map(&:max)
         .tap { |arr| arr[-1] = 0 } # don't pad the last field
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
  def self.output(id, details)
    puts "Processing #{id} with template: #{details['template']}"
    Splitter.things_items Device.new(id, details).parse
  end

  def parse
    template_obj.result(binding)
  end

  def template_obj
    Template.get(template_name)
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
    @details['label'] || name.humanize
  end

  def room
    @details['room'] || name_parts[0].humanize
  end

  def name_parts
    name.split('_')
  end

  # return the missing method/variable from the details data
  def method_missing(method, *args)
    if @details.respond_to? method
      # puts "Method called: #{method} #{args}"
      @details.send(method, *args)
    else
      @details[method.to_s]
    end
  end

  def respond_to_missing?(_method_name, _include_private = false)
    true
  end

  def make_groups(*groups)
    groups.flatten.compact.groups
  end

  def make_tags(*tags)
    tags.flatten.compact.tags
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
    meta.flatten.compact.metadata
  end

  def make_metadata(*meta)
    meta.flatten.compact.complete_metadata
  end
end

# Add convenience methods to array for building group, tag, and metadata elements
module TemplateArray
  def groups
    return '' if empty?

    uniq.enclose(outer: '()')
  end

  def tags
    return '' if empty?

    uniq.enclose(outer: '[]', inner: '"')
  end

  def metadata(complete: false)
    return '' if empty?

    complete ? metadata_array.enclose(outer: '{}') : metadata_array.join(', ').prepend(', ')
  end

  def complete_metadata
    metadata(complete: true)
  end

  def enclose(outer:, inner: nil, delimiter: ', ')
    map { |entry| "#{inner}#{entry}#{inner}" }
      .join(delimiter)
      .tap { |str| str.prepend(outer[0]).concat(outer[1] || outer[0]) if outer }
  end

  private

  def metadata_array
    map do |entry|
      case entry
      when Hash then metadata_from_hash(entry)
      when String then entry
      end
    end.compact.flatten.uniq
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

class Array
  prepend TemplateArray
end

#
# Ignore hash value access on non-existent variables
#
module TemplateNil
  def [](key); end
end

class NilClass
  prepend TemplateNil
end

# Add helper methods to String
class String
  #
  # Convert CamelCase_ID to "Camel Case ID"
  #
  # @return [String] the humanized version of the self
  #
  def humanize
    gsub(/[a-z](?=[A-Z])/, '\0 ').gsub('_', ' ')
  end

  #
  # Enclose string with double quotes
  #
  # @return [String] string enclosed with double quotes
  #
  def quote
    %("#{self}")
  end
end

#
# Process devices/settings hash
# render each device into output
# combine them, then write to file
#
class Devices
  def initialize(input_hash)
    @settings = input_hash.delete('settings')
    @devices = input_hash
  end

  #
  # Render the templates and write the output to the given file paths
  # or use the paths from the template not specified
  #
  # @param [String] things_file path to the things file
  # @param [String] items_file path to the items file
  #
  def generate(things_file: nil, items_file: nil)
    output_file = { 'things' => things_file, 'items' => items_file }

    # map { id1 => details, id2 => details,...} hash into:
    # [ [rendered_things1, rendered_items2], [rendered_things2, rendered_items2], ... ]
    # then transpose it into:
    # [ [rendered_things1, rendered_things2, ...], [rendered_items1, rendered_items2, ...]]
    output = @devices.map { |id, details| Device.output(id, details) }.transpose
    output = { 'things' => output[0], 'items' => output[1] }

    output.each do |type, value|
      header = @settings.dig(type, 'header') || ''
      file_content = format_content(type, value).prepend(header)

      output_file = output_file[type] || @settings[type]['output_file']
      File.write(output_file, file_content)
    end
  end

  def format_content(type, content)
    content = content.join("\n\n")
    content = Formatter.align_items(content) if type == 'items'
    content = Formatter.align_things(content) if type == 'things'
    content
  end
end

device_file = 'test.yaml'
devices = YAML.load_file(device_file)
Devices.new(devices).generate
