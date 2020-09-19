let Containerfile =
      https://raw.githubusercontent.com/softwarefactory-project/dhall-containerfile/0.1.0/package.dhall sha256:9ee58096e7ab5b30041c2a2ff0cc187af5bff6b4d7a6be8a6d4f74ed23fe7cdf

let Env =
      { fedora-release = env:FEDORA_RELEASE as Text
      , stack-path = env:STACK_PATH as Text
      , git-ver = env:GIT_REV as Text
      }

let StackContainer =
      \(Env : { fedora-release : Text, stack-path : Text, git-ver : Text }) ->
      \(name : Text) ->
      \(requirements : List Text) ->
        let fedora-req = [ "glibc", "gmp", "ncurses-libs" ]

        let Text/concatSep =
              https://prelude.dhall-lang.org/Text/concatSep.dhall sha256:e4401d69918c61b92a4c0288f7d60a6560ca99726138ed8ebc58dca2cd205e58

        in    Containerfile.from
                "registry.fedoraproject.org/fedora:${Env.fedora-release}"
            # Containerfile.run
                "Install requirements"
                [ "dnf update -y"
                ,     "dnf install -y "
                  ++  Text/concatSep " " (fedora-req # requirements)
                , "dnf clean all"
                ]
            # Containerfile.copy
                [ "${Env.stack-path}/bin/${name}", "/bin/${name}" ]
            # Containerfile.volume [ "/data" ]
            # Containerfile.entrypoint [ "/bin/${name}" ]

in  { Container.Stack = StackContainer Env }
