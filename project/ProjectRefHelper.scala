import sbt._, Keys._
import spray.json._, DefaultJsonProtocol._

import scala.util.control.Exception._
import java.nio.file.Files

import scala.util.control.Exception.nonFatalCatch

class ProjectRefHelper(val p: Project) extends AnyVal {

  def dependsOnSourceProjectRefOrLibraryArtifacts
  (projectID: String,
   projectName: String,
   projectConf: Option[String],
   libs: Seq[ModuleID]): Project = {
    val linksFile = p.base / "links.json"
    val projectLink: Option[File] =
      nonFatalCatch[Option[File]]
        .withApply { (t: java.lang.Throwable) => Option.empty[File] }
        .apply({
          if (linksFile.exists && linksFile.isFile) {
            val linksJSon = scala.io.Source.fromFile(linksFile).mkString
            val linksAST = linksJSon.parseJson
            val linksMap = linksAST.convertTo[Map[String, String]]
            linksMap.get(projectName) match {
              case None =>
                Option.empty[File]
              case Some(projectPath) =>
                val projectPathLink = new File(projectPath)
                if (projectPathLink.isAbsolute)
                  Some(projectPathLink)
                else
                  Some(p.base.toPath.resolve(projectPath).normalize().toRealPath().toFile)
            }
          } else
            Option.empty[File]
        })

    projectLink match {
      case None =>
        p.settings(libraryDependencies ++= libs)
      case Some(projectDir) =>
        // This must be a RootProject(uri), not ProjectRef(uri, id)
        // With a ProjectRef, SBT sees only 1 level of source-to-source project dependency.
        // With a RootProject, SBT sees the transitive closure of all source-to-source project dependencies.
        val pref = ProjectRef(projectDir, projectID)
        val pdep = projectConf match {
          case None =>
            p.dependsOn(pref)
          case Some(conf) =>
            p.dependsOn(pref % conf)
        }
        pdep.settings(
          clean := clean dependsOn (clean in pref)
        )
    }
  }

}


object ProjectRefHelper {

  implicit def toProjectRefHelper(p: Project): ProjectRefHelper = new ProjectRefHelper(p)

}