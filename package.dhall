let Containerfile =
      https://raw.githubusercontent.com/softwarefactory-project/dhall-containerfile/0.1.0/package.dhall sha256:9ee58096e7ab5b30041c2a2ff0cc187af5bff6b4d7a6be8a6d4f74ed23fe7cdf

let OpenShift =
      https://raw.githubusercontent.com/TristanCacqueray/dhall-openshift/c2ae2e5b0421c81b804cf0d6093a1b1e1a115f8c/mini-package.dhall sha256:7325790b3874ed8dc0459965a880f583c6b37a8871882590f5d6e4b5187f9a82

let Text/concatSep =
      https://prelude.dhall-lang.org/Text/concatSep.dhall sha256:e4401d69918c61b92a4c0288f7d60a6560ca99726138ed8ebc58dca2cd205e58

let Env =
      { fedora-release = env:FEDORA_RELEASE as Text ? "32"
      , stack-path = env:STACK_PATH as Text ? "."
      , git-ver = env:GIT_REV as Text ? "master"
      }

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

let StackBuilder =
      \(fedora-release : Natural) ->
      \(packages : List Text) ->
        let --| TODO: change the resolver value based on fedora release
            resolver =
              "lts-16.15"

        let stack = "stack --system-ghc --resolver ${resolver}"

        in    Containerfile.from
                "registry.fedoraproject.org/fedora:${Natural/show
                                                       fedora-release}"
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
      \(Env : { fedora-release : Text, stack-path : Text, git-ver : Text }) ->
      \(name : Text) ->
      \(requirements : List Text) ->
        let fedora-req = [ "glibc", "gmp", "ncurses-libs" ]

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

let DeploySimple =
      \(name : Text) ->
      \(image : Text) ->
      \(port : Natural) ->
      \(hostname : Text) ->
      \(config : OpenShift.ConfigMap.Type) ->
        let toto = 42

        let deployment =
              OpenShift.Deployment::{
              , metadata = OpenShift.ObjectMeta::{ name = Some name }
              , spec = Some OpenShift.DeploymentSpec::{
                , selector = OpenShift.LabelSelector::{
                  , matchLabels = Some (toMap { app = name })
                  }
                , replicas = Some 1
                , template = OpenShift.PodTemplateSpec::{
                  , metadata = OpenShift.ObjectMeta::{
                    , name = Some name
                    , labels = Some (toMap { app = name })
                    }
                  , spec = Some OpenShift.PodSpec::{
                    , volumes = Some
                      [ OpenShift.Volume::{
                        , name = "${name}-config"
                        , configMap = Some OpenShift.ConfigMapVolumeSource::{
                          , name = config.metadata.name
                          }
                        }
                      ]
                    , containers =
                      [ OpenShift.Container::{
                        , name
                        , image = Some image
                        , ports = Some
                          [ OpenShift.ContainerPort::{ containerPort = port } ]
                        , volumeMounts = Some
                          [ OpenShift.VolumeMount::{
                            , name = "${name}-config"
                            , mountPath = "/data/"
                            }
                          ]
                        }
                      ]
                    }
                  }
                }
              }

        let service =
              OpenShift.Service::{
              , metadata = OpenShift.ObjectMeta::{
                , name = Some "${name}-service"
                }
              , spec = Some OpenShift.ServiceSpec::{
                , selector = Some (toMap { app = name })
                , ports = Some
                  [ OpenShift.ServicePort::{
                    , targetPort = Some (OpenShift.IntOrString.Int port)
                    , port = 80
                    }
                  ]
                }
              }

        let route =
              OpenShift.Route::{
              , metadata = OpenShift.ObjectMeta::{ name = Some name }
              , spec = OpenShift.RouteSpec::{
                , host = hostname
                , path = Some "/"
                , port = Some OpenShift.RoutePort::{
                  , targetPort = OpenShift.IntOrString.Int port
                  }
                , tls = Some OpenShift.TLSConfig::{
                  , insecureEdgeTerminationPolicy = Some "Redirect"
                  , termination = "edge"
                  }
                , to = OpenShift.RouteTargetReference::{
                  , kind = "Service"
                  , name = "${name}-service"
                  , weight = 100
                  }
                }
              }

        in  { apiVersion = "v1"
            , kind = "List"
            , items =
              [ OpenShift.Resource.Route route
              , OpenShift.Resource.Service service
              , OpenShift.Resource.Deployment deployment
              , OpenShift.Resource.ConfigMap config
              ]
            }

in  { Container =
      { Stack = StackContainer Env, Builder = StackBuilder 33 StackPackages }
    , OpenShift =
      { Simple = DeploySimple
      , Config =
          \(name : Text) ->
          \(data : List { mapKey : Text, mapValue : Text }) ->
            OpenShift.ConfigMap::{
            , metadata = OpenShift.ObjectMeta::{ name = Some name }
            , data = Some data
            }
      }
    }
