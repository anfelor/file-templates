# Templates - a simple templating system

Assuming you have a directory `~/.templates/post`:
```shell
$ tree ~/.templates/
/home/me/.templates/
└── post
    ├── author
    │   ├── file.md
    │   └── ??name2?.md
    ├── ?env?LANGUAGE?.md
    └── ??name?.md

2 directories, 4 files
```

Go into a new directory and execute `new post`:
```shell
$ new post
Please give a value for 'name': Hello 
Please give a value for 'name2': World

$ tree
.
├── author
│   ├── file.md
│   └── World.md
├── en_US.md
└── Hello.md

1 directory, 4 files
```

Voilà!

## How it works

It accepts one argument `dir` and then looks up either `~/.templates/dir/`,
or `%APPDATA%/dir/`. It then substitutes 

 - `??xxx?` by asking the user: `Please give a value for 'xxx':`
 - `?env?xxx?` by reading the environment variable `xxx`

in both filenames and file contents, then copying the files into the current
directory (if that introduces slashes in a file name, it will happily split the
filename into  directory structure). This tool works best with other simple tools
like [sos](https://github.com/schell/steeloverseer) and [shake](shakebuild.com)
in my experience.

## Possible issues

 1. It should work on Windows, but that has not been tested.
 2. There are no unit tests.

## Contributing

Contributions are always welcome, as long as they:

 - Keep it simple
 - Work cross-platform

Possible candidates include:

 - Testing
 - Adding new variable resources (git config, etc.)
