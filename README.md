[![Workflow Status](https://github.com/justinlovinger/tag/workflows/build/badge.svg)](https://github.com/justinlovinger/tag/actions?query=workflow%3A%22build%22)

Note: this utility is in the middle of a major rewrite,
including breaking changes to syntax.
See `pre-metadata` for the old and stable version.

# Tag

A utility to automatically organize files with a simple tagging system.

## Syntax

A tagged filesystem stores metadata in `.tag/`
and tagged paths outside `.tag/`.
Tagged filesystems can be nested,
meaning tagged directories can be tagged filesystems.

Files are stored in `.tag/files/`,
with the structure,
`.tag/files/NAME`.

Tags are stored in `.tag/tags/`
with the structure,
`.tag/tags/NAME/NAMESPACE/TAG`.
The Tag program uses the namespace: `tag`.
An arbitrary number of namespaces can be defined.
Tags from all namespaces are used for building tagged paths.

Links to files are stored outside `.tag/`.
Each link is a tagged path.
A tagged path consists of zero or more tags
and a name.
Each tag ends with `-` or `/`.
Tags are separated from name by `_` directly following the end of a tag.
For example,
`foo-bar/_baz`
is a path named `baz`
with tags `foo` and `bar`.
`foo/bar-_baz`,
`foo-bar-_baz`,
and `foo/bar/_baz`
are equivalent.
Tags cannot start with `.`.
Tagged paths cannot have the same tag more than once.
Tag order is arbitrary.

## Terminology

A "tagged path" is a link to a file.
It stores tags and a name in its path.
It may have zero tags.

An "inline-tag" is a tag in a tagged path ended by `-`.

A "directory-tag" is a tag in a tagged path ended by `/`.

A "tagged file" is a file or directory in `.tag/files/`.
The term refers to both tagged directories
and tagged regular files.

A "name" is the name of a file in `.tag/files/`,
including extension.

A "tagged filesystem" is a directory containing tagged files
and tagged paths.
The term does _not_ imply a separately mounted filesystem.

## Building

Run `cargo build --release` or `nix build`.

## Usage

See `tag --help` and `tag [COMMAND] --help` for detailed usage instructions.

Initialize a tagged filesystem using `tag init`.

Files in `.tag/files/`
and tags in `.tag/tags/`
can be manually manipulated,
changed,
added,
moved,
etc.
Tags in the `tag` namespace can also be manually changed.
`tag build` updates tags and tagged paths according to manual changes.
All commands other than `tag build` are technically optional.

## Planned features

### Implied tags

Tags may imply other tags.
For example,
`rust` may imply `code`.
Implied tags may be handled by a separate program.
The program would add tags to its own namespace.

### Inferred tags

Tags may be inferred by context,
directory contents,
or file contents.
For example,
a directory containing `Cargo.toml`
may infer `rust`.
Machine-learning may be used to learn inferred tags.
Inferred tags could be defined by each user
or learned on a per-user basis.
Machine-learning would not involve uploading any data.
