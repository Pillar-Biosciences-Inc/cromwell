package cromwell.backend.impl.bcs


final case class BcsInstanceTypeQuota(instanceType: String, cpu: Int, memoryInGB: Int) extends Ordered[BcsInstanceTypeQuota] {
    override def compare(that: BcsInstanceTypeQuota): Int = this.memoryInGB.compareTo(that.memoryInGB)
}

object BcsInstanceTypeQuotas{
  val instanceTypeQuotasPattern = s"""(\\S+)\\s+(\\d+)\\s+(\\d+)""".r

  def parse(s: String): BcsInstanceTypeQuota = {
    s match {
      case instanceTypeQuotasPattern(instanceType, cpu, memoryInGB) => BcsInstanceTypeQuota(instanceType, cpu.toInt, memoryInGB.toInt)
      case _ => throw new IllegalArgumentException("BCS instance type quotas must be 'ecs.c6.large 2 4'")
    }
  }
}