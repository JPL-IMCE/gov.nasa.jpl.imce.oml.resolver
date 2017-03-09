
sbtPlugin := false

name := "gov.nasa.jpl.imce.oml.resolver"

description := "Symbol table resolver for the Json serialization of JPL's Ontological Modeling Framework Schema"

moduleName := name.value

organization := "gov.nasa.jpl.imce"

organizationName := "JPL-IMCE"

organizationHomepage := Some(url(s"https://github.com/${organizationName.value}"))

homepage := Some(url(s"https://jpl-imce.github.io/${moduleName.value}"))

git.remoteRepo := s"git@github.com:${organizationName.value}/${moduleName.value}"

scmInfo := Some(ScmInfo(
  browseUrl = url(s"https://github.com/${organizationName.value}/${moduleName.value}"),
  connection = "scm:"+git.remoteRepo.value))

developers := List(
  Developer(
    id="NicolasRouquette",
    name="Nicolas F. Rouquette",
    email="nicolas.f.rouquette@jpl.nasa.gov",
    url=url("https://github.com/NicolasRouquette")))

