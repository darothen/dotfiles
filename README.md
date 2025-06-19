# daniel's dotfiles

These are the dotfiles I use to configure the tools on my commonly-used machines. I wanted a system where I could use the same set of aliases and basic commands no matter what machine I'm logged into, be it my local desktop or a supercomputer in Singapore. A lot of this is inspired by the [dotfiles.github.io](https://dotfiles.github.io/) page, and follows the examples found on medium's [great article on the topic](https://medium.com/@webprolific/getting-started-with-dotfiles-43c3602fd789).

All the files are organized in a "topical" fashion, with sub-directories containing a facsimile of the directory structure that the dotfiles should live in at the `$HOME` directory. Multiple versions of files, specialized for individual systems, are stored with the optional suffix ".<machine>". A number of machines are pre-set in my scripts.

## installation

Should be as simple as cloning this repository and running my `dotfiles` script, which is a work-in-progress:

``` bash
git clone https://github.com/darothen/dotfiles.git ~
cd ~/dotfiles
./dotfiles [-O/--overwrite] <machine>
# or, be fancy and use the built-in uv shebang
# uv run python dotfiles [-O/--overwrite] <machine>
```

The optional argument `machine` lets you choose an "extra" file to be copied into your `$HOME` directory as `.bash_machine`. This is good if, say, you need to load modules or something else that are totally irrelevant for another machine.

## other installation

If that doesn't work, clone the repository into your home directory and execute

```bash
for file in tools/dotfiles/bash/.*; do ln -s -f $file $(basename $file); done
```

then rename *.bash_[your_machine]* to *.bash_machine*

## can i steal this

YES. Please take this and do whatever you want with it. I can't guarantee my configurations are the best - or even particularly good - or that my install system is any better than tools like [GNU Stow](https://www.gnu.org/software/stow/), but they work just great for me.
