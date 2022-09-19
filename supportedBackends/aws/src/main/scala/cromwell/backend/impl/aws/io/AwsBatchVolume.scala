/*
 * Copyright 2018 Amazon.com, Inc. or its affiliates.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are met:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *  this list of conditions and the following disclaimer.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *  notice, this list of conditions and the following disclaimer in the
 *  documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the name of the copyright holder nor the names of its
 *  contributors may be used to endorse or promote products derived from
 *  this software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,
 *  BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
 *  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL
 *  THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 *  INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 *  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 *  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 *  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 *  STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
 *  IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 *  POSSIBILITY OF SUCH DAMAGE.
 */
package cromwell.backend.impl.aws.io

import cats.data.Validated._
import cats.syntax.validated._
import software.amazon.awssdk.services.batch.model.{Host, MountPoint, Volume}
import cromwell.core.path.{DefaultPathBuilder, Path}
import common.exception.MessageAggregation
import common.validation.ErrorOr._
import cromwell.backend.DiskPatterns
import wom.values._
import org.slf4j.{Logger, LoggerFactory}
import scala.util.Try
import scala.util.matching.Regex


/*
 * This will handle volumes that are defined in the configuration. It will
 * *not* attach new block storage, as that is handled a priori as part of the
 * compute environment made available to run the jobs. This differs from
 * some other providers that create the entire compute environment on demand.
 */

object AwsBatchVolume {
  def parse(s: String): Try[AwsBatchVolume] = {
    Log.info("AwsBatchVolume parse mounts: {}", s)

    val volume_conf: Array[String] = s.trim.split("\\s+")
    val volume_conf_length: Int = volume_conf.length
    val hostPath: String = volume_conf(0).trim
    if (!hostPath.startsWith("/")){
        throw new UnsupportedOperationException with MessageAggregation {
          val exceptionContext = ""
          val errorMessages: List[String] = List(s"Docker mount source path must start with '/' but got: '$s'")
        }
    }
    
    val validation: ErrorOr[AwsBatchVolume] = volume_conf_length match {
      case 1 =>
        Valid(AwsBatchEmptyMountedDisk(DefaultPathBuilder.get(hostPath), DefaultPathBuilder.get(hostPath)))
      case volume_conf_length if (volume_conf_length>1) =>
        val two_value: String = volume_conf(1).trim
        var readOnly: Boolean = true
        var mountPath: String  = hostPath
        if (two_value.toLowerCase == "false"){
          readOnly = false
        }
        else if (two_value.toLowerCase == "true"){
          readOnly = true
        } 
        else if (!two_value.startsWith("/")){
          s"Docker mount destination path must start with '/' but got: '$s'".invalidNel
        }
        else{
          mountPath = two_value
        }
        if (volume_conf_length>=3 && volume_conf(2).trim.toLowerCase == "false"){
          readOnly = false
        }

        Valid(AwsBatchEmptyMountedDisk(DefaultPathBuilder.get(hostPath), DefaultPathBuilder.get(mountPath), readOnly))
    } 

    Try(validation match {
      case Valid(localDisk) => localDisk
      case Invalid(nels) =>
        throw new UnsupportedOperationException with MessageAggregation {
          val exceptionContext = ""
          val errorMessages: List[String] = nels.toList
        }
    })
  }
}

trait AwsBatchVolume {
  def name: String
  def hostPoint: Path
  def mountPoint: Path
  def readOnly: Boolean
  def fsType: String
  def toVolume(id: Option[String]=None): Volume = {
    Volume
      .builder
      .name(name)
      .host(Host.builder.sourcePath(hostPoint.toAbsolutePath.pathAsString).build)
      .build
  }
  def toMountPoint: MountPoint = {
    MountPoint
      .builder
      .readOnly(readOnly)
      .containerPath(mountPoint.toAbsolutePath.pathAsString)
      .sourceVolume(name)
      .build
  }
}

case class AwsBatchEmptyMountedDisk(hostPoint: Path, mountPoint: Path, readOnly: Boolean=true) extends AwsBatchVolume {
  val name = s"d-${mountPoint.pathAsString.md5Sum}"
  val fsType=  "ebs"
  override def toString: String = s"$hostPoint $mountPoint $readOnly"
}

object AwsBatchWorkingDisk {
  val MountPoint: Path = DefaultPathBuilder.get("/cromwell_root")
  val Name = "local-disk"
  val fsType=  "ebs"
  val Default = AwsBatchWorkingDisk()
}

case class AwsBatchWorkingDisk() extends AwsBatchVolume {
  val mountPoint = AwsBatchWorkingDisk.MountPoint
  val name = AwsBatchWorkingDisk.Name
  val fsType = AwsBatchWorkingDisk.fsType
  val hostPoint = mountPoint
  val readOnly = false
  override def toString: String = s"$hostPoint $mountPoint $readOnly"
}
