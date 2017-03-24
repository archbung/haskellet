# Haskellet

Haskellet is a simple wavelet toolbox written in Haskell.  It exposes two modules:

- `Transform` which contains the half-tree and full-tree wavelet transforms and their inverses.
- `Daubechies` which contains the Daubechies wavelet.

A full-tree transform will further decomposes both the low frequency components (approximation coefficients) and high frequency components (detail coefficients) in subsequents stages, resulting in linear frequency allocation.  In contrast, a half-tree transform only further decomposes approximation coefficients in subsequent stages, which results in logarithmic frequency allocation.

As it currently stands, Haskellet is naively implemented using list-based convolution but it is correct AFAIK.  This library is intended as a baseline for future implementations. 

This library uses [Stack], so simply clone this repo and run `stack build` or add this to your `stack.yaml` in your project directory.


[Stack]: https://github.com/commercialhaskell/stack
