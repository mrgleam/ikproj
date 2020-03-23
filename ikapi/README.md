# ikapi

## Install/upgrade

### macOS

#### Installer script

Run:

`curl -sSL https://get.haskellstack.org/ | sh`

#### Using Homebrew

`brew install haskell-stack`

`stack --version`

#### Notes

After installation, running stack setup might fail with `configure: error: cannot run C compiled programs.` in which case you should run:

`xcode-select --install`

#### Running Tests

`stack test`

To run the tests on every file save, run this command instead:

`stack test --file-watch`
