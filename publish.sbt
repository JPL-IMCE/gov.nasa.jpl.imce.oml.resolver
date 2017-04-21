enablePlugins(SignedAetherPlugin)

disablePlugins(AetherPlugin)

overridePublishSignedSettings

// do not include all repositories in the POM
// (this is important for staging since artifacts published to a staging repository
//  can be promoted (i.e. published) to another repository)
pomAllRepositories := false

// make sure no repositories show up in the POM file
pomIncludeRepository := { _ => false }

// include *.zip artifacts in the POM dependency section
makePomConfiguration :=
  makePomConfiguration.value.copy(includeTypes = Set(Artifact.DefaultType, Artifact.PomType, "zip"))

// publish Maven POM metadata (instead of Ivy);
// this is important for the UpdatesPlugin's ability to find available updates.
publishMavenStyle := true

PgpKeys.useGpg := true

PgpKeys.useGpgAgent := true

pgpSecretRing := file("local.secring.gpg")

pgpPublicRing := file("local.pubring.gpg")

git.baseVersion := Settings.version

versionWithGit
