# ikapi

## Install/upgrade

### macOS

#### Installer script

Run:

`curl -sSL https://get.haskellstack.org/ | sh`

#### Using Homebrew

`brew install haskell-stack`

#### Notes

After installation, running stack setup might fail with `configure: error: cannot run C compiled programs.` in which case you should run:

`xcode-select --install`
