let Containerfile =
      https://raw.githubusercontent.com/softwarefactory-project/dhall-containerfile/0.3.0/package.dhall sha256:03a6e298ff140d430cea8b387fad886ce9f5bee24622c7d1102115cc08ed9cf9

let NpmPackage =
      ~/src/softwarefactory-project.io/software-factory/dhall-npm-package/package.dhall

let Text/concatSep =
      https://prelude.dhall-lang.org/Text/concatSep.dhall sha256:e4401d69918c61b92a4c0288f7d60a6560ca99726138ed8ebc58dca2cd205e58

let CiPackages =
      [ "make"
      , "findutils"
      , "openssl-devel"
      , "rsync"
      , "git"
      , "python3"
      , "curl"
      , "tar"
      , "bzip2"
      , "xz"
      ]

let NpmPackages = [ "nodejs", "yarnpkg" ]

let RpmBuildPackages = [ "createrepo", "rpm-build", "rpmdevtools" ]

let StackPackages =
      [ "Glob"
      , "HsYAML"
      , "QuickCheck"
      , "megaparsec"
      , "quickcheck-instances"
      , "scientific"
      , "tasty-quickcheck"
      , "tasty-expected-failure"
      , "aeson"
      , "aeson-casing"
      , "ansi-wl-pprint"
      , "async"
      , "blaze-html"
      , "bytestring"
      , "containers"
      , "co-log"
      , "cryptonite"
      , "directory"
      , "dhall"
      , "doctest"
      , "extra"
      , "filepath"
      , "haxr"
      , "http-client"
      , "http-client-tls"
      , "http-directory"
      , "http-types"
      , "irc-client"
      , "managed"
      , "mtl"
      , "network-uri"
      , "optparse-generic"
      , "random"
      , "relude"
      , "simple-cmd"
      , "simple-cmd-args"
      , "tasty"
      , "tasty-hunit"
      , "text"
      , "time"
      , "turtle"
      , "vector"
      , "versions"
      , "wai"
      , "warp"
      , "yaml"
      ]

let stack =
      \(fedora-release : Natural) ->
        let --| TODO: change the resolver value based on fedora release
            resolver =
              "lts-16.15"

        in  "stack --system-ghc --resolver ${resolver}"

let fedora =
      \(fedora-release : Natural) ->
        "registry.fedoraproject.org/fedora:${Natural/show fedora-release}"

let StackBuilder =
      \(fedora-release : Natural) ->
      \(packages : List Text) ->
        let stack = stack fedora-release

        in    Containerfile.from (fedora fedora-release)
            # Containerfile.run
                "Install requirements"
                [ "dnf update -y"
                , "dnf install -y ghc stack gmp-devel make findutils openssl-devel rsync git python3 curl tar bzip2"
                , "dnf clean all"
                ]
            # Containerfile.run
                "Setup stack"
                [ "${stack} setup", "${stack} update" ]
            # Containerfile.run
                "Prepare build"
                [     "${stack} install --prefetch --dry-run "
                  ++  Text/concatSep " " packages
                ]
            # Containerfile.run
                "Install build dependencies"
                [ "${stack} install " ++ Text/concatSep " " packages ]

let StackContainer =
      \(fedora-release : Natural) ->
      \(builder-ref : Text) ->
      \(name : Text) ->
        let fedora-req = [ "glibc", "gmp", "ncurses-libs" ]

        let stack = stack fedora-release

        let git-rev = env:GIT_REV as Text ? "master"

        in    Containerfile.from (builder-ref ++ " as builder")
            # Containerfile.run
                "Build project"
                [ "cd /usr/src/${name}"
                , "echo Building ${name} ${git-rev}"
                , "${stack} clean"
                , "${stack} build --copy-bins"
                , "cp /root/.local/bin/${name} /tmp"
                ]
            # Containerfile.from (fedora fedora-release)
            # Containerfile.copyFrom
                "builder"
                [ "/tmp/${name}", "/bin/${name}" ]
            # Containerfile.volume [ "/data" ]
            # Containerfile.entrypoint [ "/bin/${name}" ]

let NodeBuilder =
      \(fedora-release : Natural) ->
          Containerfile.from (fedora fedora-release)
        # Containerfile.run
            "Install requirements"
            [ "dnf update -y"
            ,     "dnf install -y "
              ++  Text/concatSep
                    " "
                    (CiPackages # NpmPackages # RpmBuildPackages)
            , "dnf clean all"
            ]
        # Containerfile.copy
            [ "package.json", "/usr/libexec/shake/package.json" ]
        # Containerfile.run
            "Install node dependencies"
            [ "cd /usr/libexec/shake", "yarn install" ]

let NodeDependencies =
      NpmPackage::{
      , name = "builder"
      , version = "0.1.0"
      , private = Some True
      , dependencies = Some
          ( toMap
              { react = "^16.13.1"
              , react-dom = "^16.13.1"
              , reason-react = "^0.9.1"
              , bs-platform = "^8.2.0"
              , decco = "^1.3.0"
              , tablecloth-bucklescript =
                  "https://github.com/darklang/tablecloth"
              , `@justgage/reason-cookie` = "^0.1.2"
              , js-yaml = "^3.14.0"
              , `@reasonml-community/graphql-ppx` = "^1.0.0-beta.22"
              , `@patternfly/react-core` = "^4.50.2"
              , `@patternfly/react-table` = "^4.18.14"
              , `@patternfly/react-icons` = "^4.7.12"
              , `@patternfly/react-charts` = "^6.11.3"
              , `@glennsl/bs-jest` = "^0.5.1"
              , `@glennsl/bs-json` = "^5.0.2"
              , bs-fetch = "^0.6.2"
              , parcel = "^1.12.4"
              , jest = "^26.5.0"
              }
          )
      }

in  { Container =
      { Stack = StackContainer 33
      , StackBuilder = StackBuilder 33 StackPackages
      , NodeBuilder = NodeBuilder 33
      , NodeDependencies
      }
    }
