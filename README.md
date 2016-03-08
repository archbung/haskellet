Haskellet
=========

Haskellet is a simple wavelet toolbox written in Haskell. Currently it contains multi-stage Discrete Wavelet Transform (DWT) and Undecimated Discrete Wavelet Transform (UDWT), both with full-tree and half-tree variants. It also contains Daubechies 1-20 mother wavelets.

A full-tree transform will further decomposes both the low frequency components (approximation coefficients) and high frequency components (detail coefficients) in subsequents stages, resulting in linear frequency allocation. In contrast, a half-tree transform only further decomposes approximation coefficients in subsequent stages, which results in logarithmic frequency allocation.

Currently it is divided into three parts:

- `Haskellet.hs` which contains the transforms
- `Daubechies.hs` which contains Daubechies mother wavelet, from `db1` to `db20`
- `Spec.hs` contains some test

As it currently stands, Haskellet is naively implemented using list-based convolution, but as far as I know, it is correct. This implementation is intended as a baseline for future implementations. Also, as of right now Haskellet doesn't use tools like [stack] (https://github.com/commercialhaskell/stack), so if you want to try Haskellet, do it manually:

- Clone this repo or download source file(s) that you want
- Load in GHCi/import it to your source
NOTE: to load `Spec.hs` you will need [quickcheck] (https://hackage.haskell.org/package/QuickCheck)
- Profit

For future implementation, I would like to try:

- Using better data structure, like Vector or REPA
- Using better algorithms, like lifting
- Try to make it concurrent
