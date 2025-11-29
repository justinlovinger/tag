[![Workflow Status](https://github.com/justinlovinger/tag/workflows/build/badge.svg)](https://github.com/justinlovinger/tag/actions?query=workflow%3A%22build%22)

Note: this utility is in the middle of a major rewrite,
including breaking changes to syntax.
See `0.0.0` for the old and stable version.

# Tag

Manage paths with a simple tagging system.
The code is multithreaded and built with performance in mind.

## Syntax

A tagged path consists of zero or more tags
and an extension.
Each tag ends with `-`, `/`, or `.`.
Characters after the first `.` are the extension.
For example,
`foo-bar.baz` is a path with tags `foo` and `bar`
and extension `baz`.
`foo/bar.baz` is equivalent.
Extensions can contain `.`.
For example,
`foo.tar.gz` has the extension `tar.gz`.

Some functions may add `_` as a tag
to avoid files starting with `.` and being hidden in Linux.
Some functions may add `_[0-9]+` as a tag
to differentiate paths.

Metadata may be stored in `.tag/`.
A directory containing `.tag/` is the root of a tagged filesystem.
Tagged filesystems can be nested,
meaning tagged directories can be tagged filesystems.

## Terminology

A "tagged path" is a string following the tagging syntax.
It stores tags and an extension.
It may have zero tags.
A tagged path can represent a file,
directory,
symlink,
etc.

An "inline-tag" is a tag in a tagged path ended by `-` or `.`.

A "directory-tag" is a tag in a tagged path ended by `/`.

A "tagged filesystem" is a directory containing tagged paths.
The term does _not_ imply a separately mounted filesystem.

## Building

Run `cargo build --release` or `nix build`.

## Usage

See `tag --help` and `tag [COMMAND] --help` for CLI instructions.

See `examples/tag-view.rs` for an example of creating a symlinked view of tagged paths,
including implied and inferred tags.

See `examples/tag-organize.rs` for an example of organizing tagged files in-place.
