#!/usr/bin/env nu

def main [ ...names: string ] {
  let names = if ($names | is-empty) {
    ls --short-names .tag/files/ | get name
  } else {
    $names
  }

  let tags = $names | str join "\n" | ./.tag/tags.sh | split row "\n\n" | each { split row "\n" }
  let exts = $names | each {|name| tag-path ext $name }

  let targets = ($names | each {|name| [".tag/files", $name] | path join | path expand })
  let tmp = (mktemp -d)
  let paths = (
    $tags
    | zip $exts
    | each {|x| [($x.0 | str join "-"), '.', $x.1] | str join }
    | tag-path sort-tags-by-subfrequency ...$in
    | lines
    | tag-path combine ...$in
    | lines
    | each {|x| $tmp | path join $x }
  )
  for parent in ($paths | each { path dirname }) {
    mkdir $parent
  }
  $targets | zip $paths | each {|x| ln -s $x.0 $x.1 }
  $tmp
}
