let Containerfile =
      https://raw.githubusercontent.com/softwarefactory-project/dhall-containerfile/0.1.0/package.dhall sha256:9ee58096e7ab5b30041c2a2ff0cc187af5bff6b4d7a6be8a6d4f74ed23fe7cdf

let DhallPackages =
      ( https://raw.githubusercontent.com/podenv/hub/3346e98f3f01073330a4bbe5dbac8a804a04d52a/environments/dhall.dhall sha256:0845346af0f71b187aebb31b923c2c158cf1215f7a5a19c269cc906c332d8799
      ).PackagesStatements

let git-rev = env:GIT_REV as Text ? "master"

let env =
      { XDG_CACHE_HOME = "/root/.cache"
      , PATH =
          "/root/.cabal/bin:/root/.local/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
      }

let libs =
      "shake-0.19.1 dhall-1.34.0 dhall-json-1.7.1 dhall-yaml-1.2.1 dhall-docs-1.0.0 shake-dhall-0.1.0.0 prettyprinter-1.7.0"

in    Containerfile.from "registry.fedoraproject.org/fedora:32"
    # Containerfile.env (toMap env)
    # Containerfile.run
        "Install toolchain"
        [ "dnf update -y"
        , "dnf install -y 'dnf-command(copr)'"
        , "dnf copr enable -y petersen/cabal-install"
        , "dnf install -y iproute rsync git traceroute unzip bzip2 make curl wget tar procps-ng which sudo unzip findutils grep ncurses-devel openssl-devel zlib-devel krb5-devel cabal-install ghc"
        , "dnf clean all"
        ]
    # Containerfile.run
        "Setup cabal packages"
        [ "cabal update"
        , "cabal install ${libs}"
        , "cabal install --lib ${libs}"
        ]
    # (DhallPackages.Prelude "v17.0.0").install
    # DhallPackages.Ansible.install
    # Containerfile.run
        "Install shake-factory"
        [ "rm -Rf ~/.ghc"
        , "cd /usr/src/shake-factory"
        , "echo building ${git-rev}"
        , "cabal install --lib lib:shake-factory dhall shake shake-dhall text bytestring containers"
        ]
    # Containerfile.workdir "/data"
    # Containerfile.entrypoint [ "shake" ]
