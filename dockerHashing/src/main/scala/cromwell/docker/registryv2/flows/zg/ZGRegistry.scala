package cromwell.docker.registryv2.flows.zg

import cats.effect.IO
import cromwell.docker.registryv2.DockerRegistryV2Abstract
import cromwell.docker.{DockerImageIdentifier, DockerRegistryConfig}
import org.http4s.client._
import org.http4s.Header
import scala.util.{Failure, Success, Try}
import cromwell.docker.DockerHashResult
import cromwell.docker.DockerInfoActor.DockerInfoContext
import org.apache.http.client.methods.{HttpGet}
import org.apache.http.impl.client.{HttpClients, CloseableHttpClient}
import org.apache.http.util.EntityUtils
import spray.json.DefaultJsonProtocol._
import spray.json._

class ZGRegistry(config: DockerRegistryConfig) extends DockerRegistryV2Abstract(config) {
  val HashAlg = "sha256"
  
  override def accepts(dockerImageIdentifier: DockerImageIdentifier): Boolean = {
    dockerImageIdentifier.hostAsString.contains(".zgbio.")
  }

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
  private def matchTag(field: String, jsObject: JsObject, dockerHashContext: DockerInfoContext): Boolean = {
    val tag = dockerHashContext.dockerImageID.reference
    jsObject.fields.get(field) match {
      case Some(tagObj: JsString) if tagObj.value.contains(tag)  => true
      case _ => false
    }
  }
  private def getManifest(context: DockerInfoContext): DockerInfoResponse = {
    val serviceName: String = sys.env.getOrElse("ZGCRSERVICE", "http://10.0.0.221:38081")
    val httpclient = HttpClients.createDefault
    val url: String = s"${serviceName}/service/rest/v1/search?format=docker&name=${context.dockerImageID.image}&version=${context.dockerImageID.reference}"
    logger.info(s"ZG Docker manifest url: ${url}")

    manifestResponseHandler(httpclient, url, context) match {
      case Success(response) => {
        httpclient.close()
        response
      } // may be DockerInfoSuccessResponse or DockerInfoFailedResponse
      case Failure(ex) => {
        httpclient.close()
        throw new Exception(s"Get ZGCr manifest failed for ${context.dockerImageID}", ex)
      }
    }
  }  

  private def doAction(httpclient: CloseableHttpClient, url: String): String = {
    val httpget = new HttpGet(url)
    val response = httpclient.execute(httpget)
    val entity = response.getEntity()
    EntityUtils.toString(entity, "utf-8")     
  }

  private def manifestResponseHandler(httpclient: CloseableHttpClient, url: String, dockerHashContext: DockerInfoContext): Try[DockerInfoResponse] = {
    for {
      response <- Try(doAction(httpclient, url))
      jsObj <- Try(JsonParser(response).asJsObject())
      dockInfoRes <- Try(extractDigestFromBody(jsObj, dockerHashContext))
    } yield dockInfoRes
  }

  private def extractDigestFromBody(jsObject: JsObject, dockerHashContext: DockerInfoContext): DockerInfoResponse = {
    val items = jsObject.fields.get("items") match {
      case Some(data) => data.convertTo[List[JsObject]]
      case None => throw new Exception(s"Manifest response did not contain a data field, Please make sure the existence of image, ${jsObject}")
    }

    items find { matchTag("version", _, dockerHashContext)} match {
      case Some(itemObj) => {
        val assets =  itemObj.fields.get("assets") match {
          case Some(data) => data.convertTo[List[JsObject]]
          case _ => throw new Exception(s"Manifest response did not contain a data field, Please make sure the existence of image, ${jsObject}")              
        }

        assets find { matchTag("path", _, dockerHashContext)} match {
          case Some(assetObj) =>{
            assetObj.fields.get("checksum") match {
              case Some(checksumObj: JsObject) => {
                checksumObj.fields.get("sha256") match {
                  case Some(digest: JsString) =>
                    DockerHashResult.fromString(HashAlg + ":" + digest.value) match {
                      case Success(r) => DockerInfoSuccessResponse(DockerInformation(r, None), dockerHashContext.request)
                      case Failure(t) => DockerInfoFailedResponse(t, dockerHashContext.request)
                    }
                  case _ => DockerInfoFailedResponse((new Exception(s"Manifest response did not contain a digest field, ${jsObject}")), dockerHashContext.request)
                }            
              }
              case _ => DockerInfoFailedResponse((new Exception(s"Manifest response did not contain a digest field, ${jsObject}")), dockerHashContext.request)
            }
          }
          case _ => DockerInfoFailedResponse((new Exception(s"Manifest response did not contain a expected tag: ${dockerHashContext.dockerImageID.reference}, ${jsObject}")), dockerHashContext.request)
        }
      }
      case _ => DockerInfoFailedResponse((new Exception(s"Manifest response did not contain a expected tag: ${dockerHashContext.dockerImageID.reference}, ${jsObject}")), dockerHashContext.request)
    }  
  }
}