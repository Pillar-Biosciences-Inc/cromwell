package cromwell.docker.registryv2.flows.awsecr

import cats.effect.IO
import cromwell.docker.registryv2.DockerRegistryV2Abstract
import cromwell.docker.{DockerImageIdentifier, DockerRegistryConfig}
import cromwell.docker.DockerInfoActor.DockerInfoContext
import org.http4s.AuthScheme
import org.http4s.client.Client
import org.http4s.Header
import org.http4s.util.CaseInsensitiveString
import software.amazon.awssdk.services.ecr.EcrClient

import scala.jdk.CollectionConverters._


class AWSCloudCRRegistry(config: DockerRegistryConfig) extends DockerRegistryV2Abstract(config) {

  override protected def authScheme: CaseInsensitiveString = AuthScheme.Basic
  
  override def accepts(dockerImageIdentifier: DockerImageIdentifier): Boolean = {
    dockerImageIdentifier.hostAsString.contains(".dkr.ecr.")
  }

  override protected def getToken(dockerInfoContext: DockerInfoContext)(implicit client: Client[IO]): IO[Option[String]] = {
    IO(EcrClient.create().getAuthorizationToken().authorizationData().asScala.headOption.map(_.authorizationToken()))
  }

  override protected def registryHostName(dockerImageIdentifier: DockerImageIdentifier): String = dockerImageIdentifier.host.flatMap(_.split("/").headOption).getOrElse("")
  override protected def authorizationServerHostName(dockerImageIdentifier: DockerImageIdentifier): String = dockerImageIdentifier.host.flatMap(_.split("/").headOption).getOrElse("")
  override protected def buildTokenRequestHeaders(dockerInfoContext: DockerInfoContext): List[Header] = List.empty
}

