# daniel's dotfiles

Thes are my fancy dotfiles for configuring my personal machines. I wanted a system where I could use the same set of aliases and basic commands no matter what machine I'm logged into, be it my local desktop or a supercomputer in Singapore. A lot of this is inspired by the [dotfiles.github.io](https://dotfiles.github.io/) page, and follows the examples found on medium's [great article on the topic](https://medium.com/@webprolific/getting-started-with-dotfiles-43c3602fd789). 

## installation

Should be as simple as cloning my `tools` repo and running my dotfiles script, which is a work-in-progress:

```bash
git clone https://github.com/darothen/tools.git ~
cd ~/tools/dotfiles
./dotfiles install [machine]
```

The optional argument `machine` lets you choose an "extra" file to be copied into your `$HOME` directory as `.bash_machine`. This is good if, say, you need to load modules or something else that are totally irrelevant for another machine.

## other installation

If that doesn't work, clone the repository into your home directory
and execute

```bash
for file in tools/dotfiles/bash/.*; do ln -s -f $file $(basename $file); done
```

then rename *.bash_[your_machine]* to *.bash_machine*

## please for the love of god help me

This could probably be a *way* better system. So please please send feedback to daniel AT danielrothenberg DOT com if you stumble across this!
