# Symbol table resolver for the Json serialization of JPL's Ontological Modeling Framework Schema.
 
[![Build Status](https://travis-ci.org/JPL-IMCE/gov.nasa.jpl.imce.oml.resolver.svg?branch=master)](https://travis-ci.org/JPL-IMCE/gov.nasa.jpl.imce.oml.resolver)
 [ ![Download](https://api.bintray.com/packages/jpl-imce/gov.nasa.jpl.imce/gov.nasa.jpl.imce.oml.resolver/images/download.svg) ](https://bintray.com/jpl-imce/gov.nasa.jpl.imce/gov.nasa.jpl.imce.oml.resolver/_latestVersion)

## Running

```shell
sbt "run <program> <absolute path to OMF Schema.json.zip>"
```

or:

```shell
sbt 
jpl.omf.schema.resolver(master)> run <program> <absolute path to OMF Schema.json.zip>
```

where `<program>` is currently `test.Test1`

### Developer notes

#### Cross-project source dependency

Assuming that this project and [jpl.omf.schema.tables](https://github.com/JPL-IMCE/jpl.omf.schema.tables) 
are cloned as siblings of a directory `<dir>`, then, in `build.sbt`, change:

```sbt
    libraryDependencies ++=
      Seq(
        "gov.nasa.jpl.imce" %% "jpl-omf-schema-tables" % Settings.versions.jpl_omf_schema_tables,

        "gov.nasa.jpl.imce" %% "imce.third_party.scala_graph_libraries"
        % Versions_scala_graph_libraries.version artifacts
        Artifact("imce.third_party.scala_graph_libraries", "zip", "zip", Some("resource"), Seq(), None, Map())
      )
  )
```

to:


```sbt
    libraryDependencies ++=
      Seq(
        //"gov.nasa.jpl.imce" %% "jpl-omf-schema-tables" % Settings.versions.jpl_omf_schema_tables,

        "gov.nasa.jpl.imce" %% "imce.third_party.scala_graph_libraries"
        % Versions_scala_graph_libraries.version artifacts
        Artifact("imce.third_party.scala_graph_libraries", "zip", "zip", Some("resource"), Seq(), None, Map())
      )
  )
  .dependsOn(ProjectRef(file("<dir>/jpl.omf.schema.tables"), "tablesJVM"))
```

Note that `<dir>` must be an absolute file directory path and 
it can't be either `baseDirectory.value / ".."` because this is not a settings per se.
