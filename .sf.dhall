let Zuul =
        env:DHALL_ZUUL
      ? https://raw.githubusercontent.com/softwarefactory-project/dhall-zuul/0.1.0/package.dhall sha256:40c8a33ee920d12ac4b27571031e27722b4ef63771abaaaca471bc08654c31dc

let Prelude =
        env:DHALL_PRELUDE
      ? https://raw.githubusercontent.com/dhall-lang/dhall-lang/v17.0.0/Prelude/package.dhall sha256:10db3c919c25e9046833df897a8ffe2701dc390fa0893d958c3430524be5a43e

let nodeset =
      Zuul.Nodeset::{
      , name = Some "shake-factory-latest"
      , nodes = [ { name = "container", label = "pod-haskell-cabal-f32" } ]
      }

let hlint =
      { name = Some "shake-factory-hlint"
      , parent = Some "hlint"
      , nodeset = Some
          ( Zuul.Nodeset.Inline
              Zuul.Nodeset::{
              , nodes = [ { name = "container", label = "pod-haskell-f32" } ]
              }
          )
      }

let npm-test =
      { name = Some "shake-factory-npm-test"
      , nodeset = Some
          ( Zuul.Nodeset.Inline
              Zuul.Nodeset::{
              , nodes = [ { name = "container", label = "pod-haskell-f32" } ]
              }
          )
      , run = Some "playbooks/npm-test.yaml"
      }

let base-vars =
      [ { mapKey = "ghc_version", mapValue = Zuul.Vars.double 8.6 }
      , { mapKey = "shake_target", mapValue = Zuul.Vars.string "" }
      ]

let base =
      { name = Some "shake-factory-base"
      , abstract = Some True
      , nodeset = Some (Zuul.Nodeset.Name "shake-factory-latest")
      , run = Some "playbooks/shake.yaml"
      , vars = Some (Zuul.Vars.object base-vars)
      , roles = Some [ { zuul = "zuul-jobs" } ]
      , required-projects = Some
        [ { name = "softwarefactory-project.io/software-factory/shake-factory" }
        ]
      }

let test =
      { name = Some "shake-factory-test", parent = Some "shake-factory-base" }

let docs-vars =
      toMap
        { shake_target = Zuul.Vars.string "docs"
        , docs_dir = Zuul.Vars.string "build/docs"
        }

let docs =
      { name = Some "shake-factory-docs"
      , parent = Some "shake-factory-base"
      , post-run = Some [ "playbooks/docs.yaml" ]
      , vars = Some (Zuul.Vars.object docs-vars)
      }

let publish-docs =
          docs
      //  base
      //  { name = Some "shake-factory-publish-docs"
          , abstract = None Bool
          , parent = Some "docssf-publish"
          , vars = Some (Zuul.Vars.object (base-vars # docs-vars))
          }

let gate-jobs =
      [ Zuul.Job::hlint
      , Zuul.Job::base
      , Zuul.Job::test
      , Zuul.Job::docs
      , Zuul.Job::npm-test
      ]

let post-jobs = [ Zuul.Job::publish-docs ]

let mkPipeline =
      \(gate-jobs : List Zuul.Job.Type) ->
        let ci-pipeline =
              Zuul.Project.Pipeline
                ( Zuul.ProjectPipeline.mkSimpleJobs
                    ( Prelude.List.filter
                        Zuul.Job.Type
                        ( \(job : Zuul.Job.Type) ->
                            Prelude.Bool.not
                              (Prelude.Optional.default Bool False job.abstract)
                        )
                        gate-jobs
                    )
                )

        let post-pipeline =
              Zuul.Project.Pipeline
                (Zuul.ProjectPipeline.mkSimpleJobs post-jobs)

        in  [ toMap
                { check = ci-pipeline
                , gate = ci-pipeline
                , post = post-pipeline
                }
            ]

let shakeTest =
      \(required-projects : List Text) ->
        let required-projects-name =
              Prelude.List.map
                Text
                { name : Text }
                (\(name : Text) -> { name })
                required-projects

        in  Zuul.Job::(     test
                        //  { required-projects = Some required-projects-name }
                      )

in  { zuul =
          Zuul.Nodeset.wrap [ nodeset ]
        # Zuul.Job.wrap (gate-jobs # post-jobs)
        # Zuul.Project.wrap (mkPipeline gate-jobs)
    , mkPipeline
    , shakeTest
    }
