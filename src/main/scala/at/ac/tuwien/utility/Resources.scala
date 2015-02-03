package at.ac.tuwien.utility

import java.io.File

/**
 * Created by Aldo on 21/01/14.
 */
object Resources {

  def MATH_TEMPLATE = getFile("output/templates/MathTemplate.html")

  def MATH_QRELS_TEMPLATE = getFile("output/templates/MathQRelsTemplate.html")


  def get(path: String) = this.getClass.getClassLoader.getResourceAsStream(path)

  def getURL(path: String) = this.getClass.getClassLoader.getResource(path)

  def getFile(path: String) = convertToFile(this.getClass.getClassLoader.getResource(path))

  def getDir = new File("./src/main/resources/").getCanonicalPath()

  def getDir(resource: String) = new File("./src/main/resources").getCanonicalPath() + "/" + resource

  def convertToFile(url: java.net.URL): File =
    new File(url.toURI)

}
