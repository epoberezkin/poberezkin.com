# poberezkin.com

Site is generated by [Hakyll](https://jaspervdj.be/hakyll/).

## Build

```sh
stack build
stack exec my-site build
```

## Executable posts

To avoid building the site itself comment `executables/my-site` section in [package.yaml](https://github.com/epoberezkin/poberezkin.com/blob/master/package.yaml#L17).

- `stack run users-api`: example from [Haskell — a higher order language](http://www.poberezkin.com/posts/2020-03-15-haskell-a-higher-order-language.html).
- `stack run elevator`: example from [Modeling state machines with dependent types in Haskell: Part 1](https://www.poberezkin.com/posts/2020-06-29-modeling-state-machine-dependent-types-haskell-1.html).

## License

Content - Creative Commons Attribution-ShareAlike [CC BY-SA](https://creativecommons.org/licenses/by-sa/4.0/)

Code - [MIT](https://github.com/epoberezkin/poberezkin.com/blob/master/LICENSE)
