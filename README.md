# zzz

Simple and boring human readable data format for Zig.

zzz syntax describes a tree with as little noise as possible. It can be thought of as an extension to CSV. zzz is also really easy to implement and does not force any specific rules for string parsing or number parsing. The currently implemented conversions use Zig's standard library.

## Quick example

```
# Comments begin with a hash symbol.

# : describes a parent child relationship
name: Kobold
# , describes a sibling relationship
tags: small, humanoid, lawful evil
armor class: 12
# : can appear on the same line, here (2d6 - 2) is a child of 5
hit points: 5 : (2d6 - 2)
# You can use this for meta attributes, if necessary.
speed: 30 : ft
# Continuing on a newline. Indentation is exact to describe a parent/child relationship
stats
  str:7:-2
  dex:15:2
  con:9:-1
  int:8:-1
  wis:7:-2
  cha:8:-1
# ; can be used to go up in the tree. Here we ascend up from the "ft" meta node
senses: darkvision:60:ft;; passive perception:8
languages: common, draconic
challenge: 1:8
# Multline strings follow the same rules as Lua's. The first newline on an empty line is skipped
abilities
  sunlight sensitivity:[[
While in sunlight, the kobold has disadvantage on attack
rolls, as well as on Wisdom (Perception) checks that rely on sight.]]
  pack tactics:[[
The kobold has advantage on an attack roll against a
creature if at least one of the kobold's allies is within
5 feet of the creature and the ally isn't incapacitated.]]
```

# Sparse spec

A zzz file describes a tree. Special characters (and spaces) are used to go up and down the tree. The tree has an implicit null root node.

### Descending the tree:
```
grandparent:parent:child:grandchild
```
Output:
```
null -> "grandparent" -> "parent" -> "child" -> "grandchild"
```

### Traversing the children of root (siblings):
```
sibling1,sibling2,sibling3
```
Output:
```
null -> "sibling1"
     -> "sibling2"
     -> "sibling3"
```

### Going up to the parent:
```
parent:child;anotherparent
```
Output:
```
null -> "parent" -> "child"
     -> "anotherparent"
```

### White space and newlines are significant. A newline will take you back to the root:
```
parent:child
anotherparent
```
Output:
```
null -> "parent" -> "child"
     -> "anotherparent"
```

### Exactly two spaces are used to to go down a level in the tree:
```
parent:child
  sibling
```
Output:
```
null -> "parent" -> "child"
                 -> "sibling"
```

### You can only go one level deeper than the previous line's depth. Anything more is an error:
```
parent:child
    sibling
```
Output:
```
Error!
```

### Trailing commas, semicolons, and colons are optional. So the above (correct one) can be written as:
```
parent
  child
  sibling
```
Output:
```
null -> "parent" -> "child"
                 -> "sibling"
```

### zzz can contain strings, integers (i32), floats (f32), boolean, and nulls:
```
string:42:42.0:true::
```
Output:
```
null -> "string" -> 42 -> 42.0 -> true -> null
```

### strings are trimmed, they may still contain spaces:
```
parent:     child:      grand child      ;
```
Output:
```
null -> "parent" -> "child" -> "grand child"
```

### strings can be quoted with double quotes or Lua strings:
```
"parent":[[ child ]]:[==[grand child]=]]==];
```
Output:
```
null -> "parent" -> " child " -> "grand child]=]"
```

### Lua strings will skip the first empty newline:
```
[[
some text]]
```
Output:
```
null -> "some text"
```

### Strings are not escaped and taken "as-is".
```
"\n\t\r"
```
Output:
```
null -> "\n\t\r"
```

### Comments begin with # and run up to the end of the line. Their intendation follows the same rules as nodes.
```
# A comment
a node
  # Another comment
  a sibling
```
Output:
```
null -> "a node"
     -> "a sibling"
```