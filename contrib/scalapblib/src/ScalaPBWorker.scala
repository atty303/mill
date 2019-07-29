package mill
package contrib.scalapblib

import java.io.File
import java.lang.reflect.Method
import java.net.URLClassLoader

import mill.api.PathRef

class ScalaPBWorker {

  private var scalaPBInstanceCache = Option.empty[(Long, ScalaPBWorkerApi)]

  private def scalaPB(scalaPBClasspath: Agg[os.Path]) = {
    val classloaderSig = scalaPBClasspath.map(p => p.toString().hashCode + os.mtime(p)).sum
    scalaPBInstanceCache match {
      case Some((sig, instance)) if sig == classloaderSig => instance
      case _ =>
        val cl = new URLClassLoader(scalaPBClasspath.map(_.toIO.toURI.toURL).toArray)
        val scalaPBCompilerClass = cl.loadClass("scalapb.ScalaPBC")
        val mainMethod = scalaPBCompilerClass.getMethod("main", classOf[Array[java.lang.String]])

        val instance = new ScalaPBWorkerApi {
          override def compileScalaPB(includes: Seq[File], source: File, scalaPBOptions: String, generatedDirectory: File) {
            val opts = if (scalaPBOptions.isEmpty) "" else scalaPBOptions + ":"
            val args = Seq(
              "--throw",
              s"--scala_out=${opts}${generatedDirectory.getCanonicalPath}") ++
                includes.map(f => s"-I${f.getCanonicalPath}") ++
                Seq(source.getCanonicalPath)
            mainMethod.invoke(null, args.toArray)
          }
        }
        scalaPBInstanceCache = Some((classloaderSig, instance))
        instance
    }
  }

  def compile(scalaPBClasspath: Agg[os.Path], scalaPBSources: Seq[os.Path], includes: Seq[os.Path], scalaPBOptions: String, dest: os.Path)
             (implicit ctx: mill.api.Ctx): mill.api.Result[PathRef] = {
    val compiler = scalaPB(scalaPBClasspath)

    def compileScalaPBDir(inputDir: os.Path) {
      // ls throws if the path doesn't exist
      if (inputDir.toIO.exists) {
        os.walk(inputDir).filter(_.last.matches(".*.proto"))
          .foreach { proto =>
            compiler.compileScalaPB(includes.map(_.toIO), proto.toIO, scalaPBOptions, dest.toIO)
          }
      }
    }

    scalaPBSources.foreach(compileScalaPBDir)

    mill.api.Result.Success(PathRef(dest))
  }
}

trait ScalaPBWorkerApi {
  def compileScalaPB(includes: Seq[File], source: File, scalaPBOptions: String, generatedDirectory: File)
}

object ScalaPBWorkerApi {

  def scalaPBWorker = new ScalaPBWorker()
}
