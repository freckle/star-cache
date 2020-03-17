# \*cache

A tool for caching intermediate build artifacts, with named pointers.

## Motivation

On CI, we want the following cache behaviors:

1. Define one canonical cache key based on build _inputs_

   We find that producing a checksum of the things being cached (as cache-s3
   does), basically means the key always changes. Even when source files have
   not changed, and no compilation has occurred, something inconsequential
   inevitably differs deep within (e.g.) `.stack-work` and we find a new cache
   key. This causes a worse cache hit rate (depending on how the branch fall
   back works out) and time is always spent building and uploading a new cache
   from fully-cached builds.

1. If not available, fall back to the most recent cache for this branch

   This gives good caching as you push changes during your PR.

1. If not available, fall back to the most recent cache for `master`

   This gives good caching for the first build of a PR just branched from
   master.

At its core, \*cache accepts some smart options for building a cache key from
input artifacts, then pushes or pulls an archive from S3 named by that key. The
(not particularly) novel idea is that it also supports any number of floating
"pointers" (hence `*cache`) to that key.

By setting a pointer for the current branch when storing a cache, then trying to
follow that pointer for cache misses later, you are able to find the "best"
cache for your PR. Falling back to `master` when you don't have a branch cache
is just trying another pointer.

## Examples

```console
star-cache \
  --bucket some-bucket \
  --checksum-list <(git ls-files) \
  store --pointer this-branch ~/.stack .stack-work
```

Will:

1. Compute an md5 hash of all contents of all files output by `git ls-files`
1. Create or update an archive of `~/.stack` and `.stack-work` at
   `s3://some-bucket/{hash}`
1. Create or update a pointer from `s3://some-bucket/this-branch` to `{hash}`

```console
star-cache \
  --bucket some-bucket \
  --checksum-list <(git ls-files) \
  restore --pointer this-branch --pointer master
```

Will:

1. Compute an md5 hash of all contents of all files output by `git ls-files`
1. Look for an archive at `s3://some-bucket/{hash}`
1. If not found, read a `{hash}` out of `s3://some-bucket/this-branch` and try
   again
1. If not found, read a `{hash}` out of `s3://some-bucket/master` and try again
1. If an archive is found by any of these, it is restored to the same locations
   on disk

## Installation

```console
git clone http://github.com/freckle/star-cache
cd star-cache
stack install
~/.local/bin/star-cache --help
```

**TODO**: Binaries hosted via GitHub Releases.

## Usage

```console
% star-cache --help
Usage: star-cache [--verbose] [-P|--pigz] (-b|--bucket ARG) [-p|--prefix ARG]
                  ((-k|--key VALUE) | (-c|--checksum-file PATH) |
                    (-C|--checksum-list PATH)) COMMAND

Available options:
  -P,--pigz                Use pigz compression
  -b,--bucket ARG          S3 bucket name
  -p,--prefix ARG          S3 prefix (default: "")
  -k,--key VALUE           Set the cache key to VALUE
  -c,--checksum-file PATH  Set the cache key by checksum of PATH
  -C,--checksum-list PATH  Like --checksum-file, but read a list of files to
                           checksum from PATH
  -h,--help                Show this help text

Available commands:
  store
  restore

% star-cache store --help
Usage: star-cache store [-p|--pointer VALUE] PATH

Available options:
  -p,--pointer VALUE       Create pointer from VALUE
  PATH                     File or directory to cache
  -h,--help                Show this help text

% star-cache restore --help
Usage: star-cache restore [-p|--pointer VALUE]

Available options:
  -p,--pointer VALUE       Fallback to pointer VALUE
  -h,--help                Show this help text

```

## Development & Test

*TODO*

---

[LICENSE](./LICENSE) | [CHANGELOG](./CHANGELOG.md)
