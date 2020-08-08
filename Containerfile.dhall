let Containerfile =
        env:DHALL_CONTAINERFILE
      ? https://softwarefactory-project.io/cgit/software-factory/dhall-containerfile/plain/package.dhall

let DhallPackages =
      (   env:DHALL_PODENV_HUB
        ? https://raw.githubusercontent.com/podenv/hub/master/package.dhall
      ).Environments.Dhall.PackagesStatements

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
    # Containerfile.run
        "Install shake-factory"
        [ "rm -Rf ~/.ghc"
        , "cd /usr/src/shake-factory"
        , "cabal install --lib lib:shake-factory dhall shake shake-dhall text bytestring containers"
        ]
    # (DhallPackages.Prelude "v17.0.0").install
    # DhallPackages.Ansible.install
    # Containerfile.workdir "/data"
    # Containerfile.entrypoint [ "shake" ]
