/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.utils

import java.io.File
import java.util.Random

/**
 * Some simple file utilities.
 * @author ppedemon
 */
object FileUtils {
  
  private val maxAttempts = 10
  private lazy val tempDirBase = System getProperty "java.io.tmpdir"
  private lazy val random = new Random

  /**
   * Create a directory. Fails if the directory already exists.
   * @param dir    file describing folder to create
   */
  def createDir(dir:File) {
    def failMsg = "Could not create directory " + dir
    if(dir.isDirectory || dir.mkdirs())
      ()
    else if(dir.exists)
      sys.error(failMsg + ": file exists and is not a directory.")
    else
      sys.error(failMsg)
  }

  /**
   * Create a temp folder.
   * @return file describing created folder
   */
  def createTempDir:File = {
    def create(attempts:Int):File = {
      if(attempts > maxAttempts) {
        sys.error("Could not create temporary directory")
      } else {
        val randomName = "asm_" + Integer.toHexString(random.nextInt)
        val f = new File(tempDirBase, randomName)
        try { 
          createDir(f); f 
        } catch { 
          case e: Exception => create(attempts + 1) 
        }
      }
    }
    create(0)
  }

  /**
   * List contents of a folder.
   * @param dir    folder to list
   * @return       folder contents, as an array of files
   */
  def listFiles(dir:File):Array[File] = {
    val a = dir.listFiles()
    if (a == null) Array[File]() else a
  }

  /**
   * Delete a file. If file is a folder, delete contents recursively.
   * @param file    file to delete
   */
  def delete(file:File) {
    if (file.isDirectory) {
      listFiles(file) foreach delete
      file.delete
    } else if (file.exists) {
      file.delete
    }
  }
}

