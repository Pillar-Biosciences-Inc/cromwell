package cromwell.docker.registryv2

// From https://docs.docker.com/registry/spec/manifest-v2-2/
sealed trait DockerManifestResponse

case class DockerManifest(config: DockerManifestConfig, layers: Array[DockerLayer]) extends DockerManifestResponse{
  lazy val compressedSize: Long = layers.map(_.size).sum
}
case class DockerManifestConfig(mediaType: String, size: Int, digest: String)
case class DockerLayer(size: Long)
case class DockerManifestList(manifests: Array[DockerManifestReference]) extends DockerManifestResponse
case class DockerManifestReference(digest: String)
