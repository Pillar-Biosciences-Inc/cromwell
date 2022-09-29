package cromwell.docker.registryv2.flows.awsecr

import cats.effect.IO
import cromwell.docker.DockerHashResult
import cromwell.docker.DockerInfoActor._
import cromwell.docker._
import cromwell.docker.registryv2.DockerRegistryV2Abstract
import org.http4s.Header
import org.http4s.client.Client
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}
import com.amazonaws.auth._
import com.amazonaws.auth.profile.ProfileCredentialsProvider
import com.amazonaws.services.ecr.model._

class AWSCloudCRRegistry(config: DockerRegistryConfig) extends DockerRegistryV2Abstract(config) {
  val HashAlg = "sha256"
  val validAWSCloudCRHosts: Regex = s"""\d+.dkr.ecr.[^\s]+.amazonaws.com|com.cn""".r

  def isValidAWSCloudCRHost(host: Option[String]): Boolean = {
    host.exists {
      _ match {
        case validAWSCloudCRHosts(_) => true
        case _ => false
      }
    }
  }

  def credentialsProvider(): AWSCredentialsProvider =
    new AWSCredentialsProviderChain(
      new EnvironmentVariableCredentialsProvider(),
      new SystemPropertiesCredentialsProvider(),
      new ProfileCredentialsProvider(sys.env.getOrElse("AWS_DEFAULT_PROFILE", "default")),
      new EC2ContainerCredentialsProviderWrapper()
    )

  override def accepts(dockerImageIdentifier: DockerImageIdentifier): Boolean = isValidAWSCloudCRHost(dockerImageIdentifier.host)

  override protected def getToken(dockerInfoContext: DockerInfoContext)(implicit client: Client[IO]): IO[Option[String]] = {
    IO.pure(None)
  }

  override protected def registryHostName(dockerImageIdentifier: DockerImageIdentifier): String = ""
  override protected def authorizationServerHostName(dockerImageIdentifier: DockerImageIdentifier): String = ""
  override protected def buildTokenRequestHeaders(dockerInfoContext: DockerInfoContext): List[Header] = List.empty

  override protected def getDockerResponse(token: Option[String], dockerInfoContext: DockerInfoContext)(implicit client: Client[IO]): IO[DockerInfoSuccessResponse] = {
    getManifest(dockerInfoContext) match {
        case success: DockerInfoSuccessResponse => IO(success)
        case fail: DockerInfoFailedResponse => IO.raiseError(new Exception(fail.reason))
        case other => IO.raiseError(new Exception(s"Get manifest failed, $other"))
    }
  }

  private def getManifest(context: DockerInfoContext): DockerInfoResponse = {
    val dockerImageID = context.dockerImageID
    val client: AmazonECR  = AmazonECRClientBuilder.standard()
      .withCredentials(credentialsProvider())
      .build()
    val request: BatchGetImageRequest = new BatchGetImageRequest()
        .withRepositoryName(dockerImageID.image)
        .withImageIds(new ImageIdentifier().withImageTag(dockerImageID.reference))

    manifestResponseHandler(client, request, context) match {
      case Success(response) => response // may be DockerInfoSuccessResponse or DockerInfoFailedResponse
      case Failure(ex) => throw new Exception(s"Get AWSCr manifest failed for ${context.dockerImageID}", ex)
    }
  }

  private def matchTag(imageObject: Image, dockerHashContext: DockerInfoContext): Boolean = {
    val tag = dockerHashContext.dockerImageID.reference
    imageObject.getImageId() match {
      case Some(imageIdentifierObj: ImageIdentifier) if imageIdentifierObj.getImageTag() == tag => true
      case _ => false
    }
  }

  private def extractDigestFromBody(response: BatchGetImageResult, dockerHashContext: DockerInfoContext): DockerInfoResponse = {
    response.getImages() find {matchTag(_, dockerHashContext)} match {
        case Some(imageIdentifierObj: ImageIdentifier) =>
            DockerHashResult.fromString(HashAlg + ":" + imageIdentifierObj.getImageDigest()) match {
                case Success(r) => DockerInfoSuccessResponse(DockerInformation(r, None), dockerHashContext.request)
                case Failure(t) => DockerInfoFailedResponse(t, dockerHashContext.request)
            }
        case None => DockerInfoFailedResponse((new Exception(s"Manifest response did not contain a expected tag: ${dockerHashContext.dockerImageID.reference}, ${response.toString()}")), dockerHashContext.request)
    }
  }

  private def manifestResponseHandler(client: AmazonECR, request: BatchGetImageRequest, dockerHashContext: DockerInfoContext): Try[DockerInfoResponse] = {
    for {
      response <- Try(client.batchGetImage(request))
      dockInfoRes <- Try(extractDigestFromBody(response, dockerHashContext))
    } yield dockInfoRes
  }
}

