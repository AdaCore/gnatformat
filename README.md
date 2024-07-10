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

## Work in progress and contributing

This project status is still work in progress.

In particular, the point that will be adressed soon are

- the capability to choose other formatting layouts in addition to the default one
- the possibility to be configured to support other custom styles.  

However, contributions are welcome! Please feel free to submit a pull request
or open an issue if you find a bug or have a feature suggestion.

## License

See [LICENSE.txt](LICENSE.txt) file for details.
