# GNATformat

This project implements an opinionated code formatter for the Ada language.
It is based on the [Prettier-Ada](https://github.com/AdaCore/prettier-ada) library,
a port of the [Prettier](https://github.com/prettier/prettier) formatter engine
to the Ada programming language.

The intent of `gnatformat` is to format a valid Ada source code according to the coding
style described in the [GNAT Coding Style](https://gcc.gnu.org/onlinedocs/gnat-style.pdf)
guide.

## Usage

`gnatformat` can be used as a command line tool or as a library.

As a command line tool, for a given GNAT project, execute

```
gnatformat -P [project_name].gpr
```

For the given project, this command will format the files from all closures of mains
or library entry points, recursing on all the subprojects and stopping when a project
is marked as `externally built`.

In order to customize your formatting, a few options are available and can be listed
by executing the `gnatformat --help` command.

The functionality is also available via a library, which is in particular included
in the `Ada Language Server`, and can therefore be used through IDEs like `GNAT Studio`
or ` Visual Studio Code`.

## Building and Installing

### Dependencies

- [Libadalang](https://github.com/AdaCore/libadalang) and its dependencies.


### Instructions

```sh
# To build as LIBRARY_TYPE which can be 'static', 'static-pic' or 'relocatable'
make lib
# Or to build as command line tool the binary
make bin
# Or to build as library and command line tool
make all

# Then install it in a specific location, for exemple in /usr/local
PREFIX=/usr/local make install
```

## Testing

### Test Dependencies

- [e3-testsuite](https://e3-testsuite.readthedocs.io/en/latest/)

### Running the testsuite

```sh
make test
```

## Version control integration

GNATformat suggests the usage of [pre-commit](https://pre-commit.com/).

An example of a `.pre-commit-config.yaml` that you need to add to your repo is:

```yaml
repos:
  - repo: local
    hooks:
      - id: gnatformat
        name: Run gnatformat
        entry: gnatformat
        # Assumes that gnatformat exists on the PATH
        language: system
        files: ".*\\.ad(b|s)$"
        # Assumes that Ada sources have a .ads or .adb extension
        args:
          - -P
          - project.gpr
        # Optional since GNATformat implicitly loads the projects if found
        # and unique.
```

### How to deal with sources from multiple projects

`pre-commit` will call GNATformat with the specified `args` plus a list of committed sources that match the `files` filter. If any of these sources is invisible to the specified project, it is still formatted with the default settings.

The recommended approach to correctly deal with multiple projects is to create an aggregate project for formatting purposes only. For example:

```ada
--  format.gpr

aggregate project Format is
    for Project_files use ("project_a.gpr", "project_b.gpr");
end Format;
```

GNATformat will correctly resolve each source to the correct project and apply the settings specified in it. If the source is not part of any project, it is still formatted with the default settings.

## Work in progress and contributing

This project status is still work in progress.

In particular, the point that will be adressed soon are

- the capability to choose other formatting layouts in addition to the default one
- the possibility to be configured to support other custom styles.

However, contributions are welcome! Please feel free to submit a pull request
or open an issue if you find a bug or have a feature suggestion.

## License

See [LICENSE.txt](LICENSE.txt) file for details.
