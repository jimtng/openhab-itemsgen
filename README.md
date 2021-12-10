# openhab-itemsgen - OpenHAB Things and Items Generator

A template-based OpenHAB .things and .items file generator written in Ruby using the ERB template engine.

`device list` + `template` => `.items` and `.things` files

Things and items definitions are usually repetitive when you have multiple devices of the same type. The process of adding and maintaining .things and .items files involves tedious copy pasting and renaming. Changing how they are all defined is even more tedious.

`openhab-itemsgen` enables you to generate .things and .items files for OpenHAB using pre-set templates.

## Status

- It works, but still in active development, not yet used in production
- This is a complete rewrite of [ohgen](https://github.com/jimtng/ohgen) with a few changes:
  - Rewritten in Ruby
  - Simplified yaml structure
  - Using ERB template engine instead of Jinja2. As a result, the templates from ohgen will need to be rewritten

## Features

- Things channels and Items fields output are nicely aligned
- Use the power of Ruby inside the erb template
- Includes helper methods to make template writing easier
- Easily add default icon, groups, tags, and metadata
- Be as flexible or as simple as desired
- It can be used from the command line or from another ruby script, e.g. within a openhab-jruby rule

## Usage

### Command Line Usage

Execute `itemsgen.rb` from the command line

```bash
$ chmod 755 itemsgen.rb
$ ./itemsgen.rb devices.yaml -f
```

The command line options will be printed out with `-h`:

```
Usage: ./itemsgen.rb [options] yamlfile
    -t, --things THINGS_FILE         The path to things file output
    -i, --items  ITEMS_FILE          The path to items file output
    -v, --verbose                    Print details
    -n, --dry-run                    Run process but do not write to output files
    -f, --force                      Overwrite output files
    -d, --template-dir PATH          Path to look for the template files
    -h, --help                       Print this help
```

When the output files are specified inside the yaml file, the `-t` and `-i` options are not required. When provided, they will override the yaml, however.

When template-dir is not specified, it will look in the `templates/` subdirectory relative to the yaml file.

### Usage from another ruby script

```ruby
require 'itemsgen'

yaml = File.read('devices.yaml')
gen = OpenhabGenerator::Devices.new(yaml)
output = gen.generate
File.write('output.things', output['things'])
File.write('output.items', output['items'])
```

The list of your devices are maintained in a `devices.yaml` file by default. Example:

```yaml
settings:
  things:
    output_file: /openhab/conf/things/generated.things
    header: |+
      // This file is automatically generated

  items:
    output_file: /openhab/conf/items/generated.items
    header: |+
      // This file is automatically generated

LivingRoom_Window: # This is the `Device ID`
  template: aqara-contact # This is a mandatory field for each device
  # From here on, the name and structure of the fields are entirely up to you
  # as long as they match up with your template
  thingid: livingroom-window-contact
  groups:
    - gWindows
    - gLivingRoom
  tags:
    - Window
```

Coupled with an appropriate template (see below), this will render into:

.things file

```
// This file is automatically generated

Thing mqtt:topic:mosquitto:livingroom-window-contact (mqtt:broker:mosquitto) {
    Channels:
         Type contact : contact      [ stateTopic="zigbee/livingroom-window-contact/contact", on="false", off="true"  ]
         Type number  : linkquality  [ stateTopic="zigbee/livingroom-window-contact/linkquality" ]
         Type contact : availability [ stateTopic="zigbee/livingroom-window-contact/availability", on="online", off="offline" ]
         Type number  : battery      [ stateTopic="zigbee/livingroom-window-contact/battery" ]
}
```

.items file:

```
// This file is automatically generated

Contact LivingRoom_Window_State             "Living Room Window"                                               <door>    (gWindows, gLivingRoom, gContactSensor)                    ["Window"]                { channel="mqtt:topic:mosquitto:livingroom-window-contact:contact" }
Number  LivingRoom_Window_Link              "Living Room Window Link"                                          <network> (gSignalStrength)                                                                    { channel="mqtt:topic:mosquitto:livingroom-window-contact:linkquality" }
Number  LivingRoom_Window_Battery           "Living Room Window Battery [%d%%]"                                <battery> (gBatteries)                                                                         { channel="mqtt:topic:mosquitto:livingroom-window-contact:battery", expire="1h" }
Contact LivingRoom_Window_Availability      "Living Room Window Availability [MAP(availability.map):%s]"                 (gAvailability)                                                                      { channel="mqtt:topic:mosquitto:livingroom-window-contact:availability" }
```

The structure of the data for each item is mostly up to you, however it must match with the template. The template would be referring to the data structure from the YAML structure.

## Templates

Template files are stored in `templates/` subdirectory with an `.erb` extension. Each template file contains the template for both the things and items definition. `openhab-itemsgen` will split them into their corresponding .things and .items file.

`openhab-itemsgen` comes with a preset templates that are suitable for me. These templates may be updated / modified / changed from time to time. If you'd like to use it and follow the changes that I've made, feel free to refer to them as is. You should not modify them in-place. If you'd like to customise them, duplicate them into a different file name.

Example for `templates/aqara-contact.erb`

```ruby
Thing mqtt:topic:mosquitto:<%= thingid %> (mqtt:broker:mosquitto) {
    Channels:
        Type contact: contact      [ stateTopic="zigbee/<%= thingid %>/contact", on="false", off="true"  ]
        Type number : linkquality  [ stateTopic="zigbee/<%= thingid %>/linkquality" ]
        Type contact: availability [ stateTopic="zigbee/<%= thingid %>/availability", on="online", off="offline" ]
        Type number : battery      [ stateTopic="zigbee/<%= thingid %>/battery" ]
}

Contact <%= name %>_State   "<%= label %>"  <<%= icon || 'door' %>>     <%= make_groups groups, 'gContactSensor' %> <%= tags&.tags %> { channel="mqtt:topic:mosquitto:<%= thingid %>:contact"<%= metadata&.metadata %> }
Number  <%= name %>_Link    "<%= label %> Link"      <network> (gSignalStrength) { channel="mqtt:topic:mosquitto:<%= thingid %>:linkquality" }
Number  <%= name %>_Battery "<%= label %> Battery [%d%%]" <battery> (gBatteries) { channel="mqtt:topic:mosquitto:<%= thingid %>:battery", expire="1h" }
Contact <%= name %>_Availability "<%= label %> Availability [MAP(availability.map):%s]" (gAvailability) { channel="mqtt:topic:mosquitto:<%= thingid %>:availability" }
```

## Template Helpers

### Helper Methods

These helper methods accepts a list of arguments which can be a combination of String, Array of String, nil, or an empty array. When the resulting output is blank, it will not output the enclosing symbols (`()` for groups, `[]` for tags and `{}` for metadata)

| Method          | Description                                                                                                                                                                                                                                                                                                                                                               |
| --------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `make_groups`   | convert the supplied arguments into the groups syntax: comma separated and enclosed in parentheses, i.e. `(Group1, Group2)`                                                                                                                                                                                                                                               |
| `make_tags`     | convert the supplied arguments into the tags syntax: each element is double quoted, comma separated and enclosed in square brackets, i.e. `("Tag1", "Tag2"]`                                                                                                                                                                                                              |
| `make_metadata` | convert the supplied arguments into the metadata syntax: comma separated and enclosed in braces, i.e. `{xxx, yyy}`                                                                                                                                                                                                                                                        |
| `add_metadata`  | If you've already had a hard coded metadata and would like to add more from an array, `add_metadata` will join the array with a comma and prepend it with a comma. Example: `{channel="this is a hard coded channel"<%= add_metadata 'elem1="somedata"', 'elem2="anotherthing"' %>}` => `{channel="this is a hard coded channel", elem1="somedata", elem2="anotherthing"` |

#### Example:

Given a yaml structure:

```yaml
LightRoom_Light:
  template: example-template
  mygroups:
    - Group1
    - Group2
  light:
    light_tags:
      - light_tag1
      - light_tag2
    light_groups:
      - Light_Group1
      - Light_Group2
    metadata:
      - ga: Light
      - alexa: Lighting
  list_in_array:
    - name: Item1
      type: Switch
      tags:
        - item1_tag1
      metadata:
        - meta1: foo
        - meta2: bar
    - name: Item2
      type: Dimmer
```

The template tag:

| tag                                               | output                             |
| ------------------------------------------------- | ---------------------------------- |
| `<%= make_groups mygroups %>`                     | `(Group1, Group2)`                 |
| `<%= make_groups 'HardCodedGroup', mygroups %>`   | `(HardCodedGroup, Group1, Group2)` |
| `<%= make_groups light['light_groups'] %>`        | `(Light_Group1, Light_Group2)`     |
| `<%= make_groups light['non_existent_entry'] %>`  | `<empty string>`                   |
| `<%= make_tags light['light_tags'] %>`            | `["light_tag1", "light_tag2"]`     |
| `<%= make_metadata light['metadata'] %>`          | `{ga="Light", alexa="Lighting"}`   |
| `<%= add_metadata light['metadata'] %>`           | `, ga="Light", alexa="Lighting"`   |
| `<%= add_metadata light['non_existent_entry'] %>` | `<empty string>`                   |

We can loop through the `list_in_array` using ruby code

`templates/example-template.erb`

```ruby
<% list_in_array.each do |item| %>
<%= item['type'] %> <%= item['name'] %> <%= make_groups item['groups'] %> <%= make_tags item['tags'] %> <%= make_metadata item['metadata'] %>

<% end %>
```

Output:

```
Switch Item1 ["item1_tag1"] {meta1="foo", meta2="bar"}
Dimmer Item2
```

### Predefined Variables

Several predefined variables / methods are also available for the template. These variables are derived from the yaml device id.

- `name`: the name of the device id from the yaml, e.g. `LivingRoom_Light`
- `label`: unless specified in the yaml, the name of the device converted to human readable, e.g. `LivingRoom_Light` => `Living Room Light`
- `thingid`: unless specified in the yaml, a lower case version of `name`, with `_` converted to `-`
- `room`: unless specified in the yaml, the first part of `name` before the underscore, converted to human readable, e.g. `Living Room`
- `name_parts`: Split `name` parts delimited by underscores, e.g. `['LivingRoom', 'Light']`

Any of these variables can be overridden in yaml.
