# grep-haskell

`grep-haskell` is an implementation of the classic `grep` utility, entirely in Haskell. This project aims to replicate the functionality and performance of traditional `grep`, leveraging Haskell's powerful pattern matching and concurrency features to provide a fast and efficient text search utility.

## Features

- **File and Directory Recursion**: Search through files and directories recursively, with options to include or exclude specific file types.
- **Context Control**: Display surrounding lines of content around matches for better context, with customizable lines before and after matches.
- **Performance**: Optimized for speed, aiming to match or come close to the performance of GNU grep by leveraging Haskell's concurrency and efficient text processing capabilities.
- **Color Highlighting**: See your matches highlighted in the terminal for easier scanning.
- **Multiple File Handling**: Search through multiple files or standard input, with clear indications of where matches are found.
- **Customizable Output**: Options to show only filenames, count of matches, line numbers, and more, for flexible output tailored to your needs.

## Installation

To install `grep-haskell`, ensure you have the Haskell Stack tool installed on your system. Follow these steps:

1. Clone the repository:
   ```bash
   git clone https://github.com/yourusername/grep-haskell.git
   cd grep-haskell
   ```

2. Build the project using Stack:
   ```bash
   cabal build
   ```

3. Run your local binary path:
   ```bash
   cabal exec my-grep
   ```

## Usage

Basic usage of `grep-haskell` follows the familiar `grep` syntax, with additional options for its unique features:

```bash
grep-haskell [OPTIONS] PATTERN [FILE...]
```

### Options

- `-i`, `--ignore-case`: Ignore case distinctions in both the pattern and the input files.
- `-n`, `--line-number`: Prefix each line of output with the line number within its input file.

### Examples

Search for the word "Hello Haskell" in all files in the directory "./haskell" and subdirectories:

```bash
./my-grep "Hello Haskell" ./haskell/ -n
```


## Contributing

Contributions to `grep-haskell` are welcome! Whether it's bug reports, feature requests, or pull requests, all forms of contributions help make `grep-haskell` better. Please follow the standard GitHub fork & pull request workflow.

---

This README is just a starting point. As `grep-haskell` grows and evolves, be sure to update documentation to reflect new features and changes.
