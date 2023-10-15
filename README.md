[![Workflow Status](https://github.com/justinlovinger/tag/workflows/build/badge.svg)](https://github.com/justinlovinger/tag/actions?query=workflow%3A%22build%22)

# tag

A utility to automatically organize files with a simple tagging system.

## Syntax

A tagged file consists of zero or more tags
and a name.
Each tag ends with `-` or `/`.
Tags are separated from name by `_` directly following the end of a tag.
For example,
`foo-bar/_baz`
is a file named `baz`
with tags `foo` and `bar`.
`foo/bar-_baz`,
`foo-bar-_baz`,
and `foo/bar/_baz`
are equivalent.
Tags cannot start with `.`.
Tagged files cannot have the same tag more than once.
Tag order is arbitrary.
Name can be an empty string,
such as `foo-_`.

## Terminology

A "tagged file" is a file with the above syntax.
It may have zero tags.
A directory with the above syntax is also a "tagged file".
This utility makes no distinction
between tagged directories
and regular tagged files.

A "tagged filesystem" is a directory containing tagged files.
The term does *not* imply a separately mounted filesystem.

An inline-tag is a tag ended by `-`.
A directory-tag is a tag ended by `/`.

## Building

Run `cargo build --release` or `nix build`.

## Usage

See `tag --help` and `tag [COMMAND] --help` for detailed usage instructions.

## Tips

Make the top-level of your tagged filesystem a tagged directory.
For example,
`_tagged-files`
instead of `tagged-files`.
This prevents accidentally moving files out of a tagged filesystem
if the utility is called from the wrong directory.
Also,
future changes may require a tagged directory at the top-level of a tagged filesystem.

Nest tagged filesystems.
For example,
`_data/_journal`.
Nesting tagged filesystems prevents files in the nested filesystem from moving out,
but the directory corresponding to the nested tagged filesystem is still organized
in the context of its parent tagged filesystem.

## Planned features

### Implied tags

Tags may imply other tags.
For example,
`rust` may imply `programming`.
Implied tags would be defined by each user
and may require a special directory
at the top-level of a tagged filesystem,
not unlike `.git`.
Details of how implied tags are represented
are not finalized.

### Inferred tags

Tags may be inferred by context,
directory contents,
or file contents.
For example,
a directory containing `Cargo.toml`
may infer `rust`.
Machine-learning may be used to learn inferred tags.
Inferred tags would be defined by each user
or learned on a per-user basis.
Machine-learning would not involve uploading any data.
