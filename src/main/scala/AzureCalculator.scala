object AzureCalculator {

  case class AzureStat(name: String,
                       cores: Int,
                       ram: Int,
                       disk: Int,
                       pricing: Double)

  def main(args: Array[String]): Unit = {

    val core = 16
    val ram = 32
    val disk = 0

    val refinedData = data.split("\n").filter(_.nonEmpty).map(_.split(","))

    val points = refinedData
      .map(vm => {
        AzureStat(
          vm(1).split(">").last.trim,
          vm(2).replaceAll("Cores|Core", "").trim.toInt,
          Math.ceil(vm(3).replaceAll("GB RAM", "").trim.toDouble).toInt,
          vm(4).replaceAll("GB Temporary storage", "").trim.toInt,
          vm(5).split("US\\$").last.replace("/hour", "").toDouble * 24 * 31 * 12
        )
      })
      .toList

    filterVm(points, core, ram, disk)

    ()
  }

  def filterVm(azureStats: List[AzureStat],
               cores: Int,
               ram: Int,
               disk: Int): Unit = {
    val stats = azureStats
      .filter(x => clouderaSupported.contains(cleanName(x.name)))
      .filter(vm => {
        vm.cores >= cores && vm.ram >= ram && vm.disk >= disk
      })
    List(
      stats.minBy(x => (x.cores, x.ram, x.disk)),
      stats.minBy(x => (x.cores, x.disk, x.ram)),
      stats.minBy(x => (x.ram, x.cores, x.disk)),
      stats.minBy(x => (x.ram, x.disk, x.cores)),
      stats.minBy(x => (x.disk, x.cores, x.ram)),
      stats.minBy(x => (x.disk, x.ram, x.cores))
    ).sortBy(_.pricing)
      .foreach(x => {
        println(
          s"Name=${x.name}, cores=${x.cores}, ram=${x.ram}, disk=${x.disk}, pricing=${x.pricing}")
      })
  }

  def cleanName(name: String): String =
    name.replaceAll(" +", "").trim.toLowerCase

  val clouderaSupported: Seq[String] = List(
    "E16 v3",
    "DS4 v2",
    "DS5 v2",
    "E16s v3",
    "NC6s v2",
    "NC12s v2",
    "NC24s v2",
    "D8 v3",
    "D16 v3",
    "D32 v3",
    "D8s v3",
    "D16s v3",
    "D32s v3",
    "D8a v4",
    "D16a v4",
    "D32a v4",
    "D8as v4",
    "D16as v4",
    "D32as v4",
    "E8 v3",
    "E16 v3",
    "E32 v3",
    "E8a v4",
    "E16a v4",
    "E32a v4",
    "D13v2",
    "D14v2",
    "F8s v2",
    "F16s v2",
    "F32s v2",
    "L8s v2",
//    "L16s v2",
    "L32s v2",
    "L48s v2",
    "NC6",
    "NC24r"
  ).map(cleanName)

  val data =
    """
      |a0, A0, 1 Core, 0.75 GB RAM, 20 GB Temporary storage, US$0.08/hour
      |a1, A1, 1 Core, 1.75 GB RAM, 70 GB Temporary storage, US$0.12/hour
      |a2, A2, 2 Cores, 3.5 GB RAM, 135 GB Temporary storage, US$0.18/hour
      |a3, A3, 4 Cores, 7 GB RAM, 285 GB Temporary storage, US$0.3/hour
      |a4, A4, 8 Cores, 14 GB RAM, 605 GB Temporary storage, US$0.61/hour
      |a5, A5, 2 Cores, 14 GB RAM, 135 GB Temporary storage, US$0.31/hour
      |a6, A6, 4 Cores, 28 GB RAM, 285 GB Temporary storage, US$0.56/hour
      |a7, A7, 8 Cores, 56 GB RAM, 605 GB Temporary storage, US$1.13/hour
      |a8, A8, 8 Cores, 56 GB RAM, 382 GB Temporary storage, US$1.105/hour
      |a9, A9, 16 Cores, 112 GB RAM, 382 GB Temporary storage, US$2.08/hour
      |a10, A10, 8 Cores, 56 GB RAM, 382 GB Temporary storage, US$0.91/hour
      |a11, A11, 16 Cores, 112 GB RAM, 382 GB Temporary storage, US$1.69/hour
      |b1s, B1S, 1 Core, 1 GB RAM, 4 GB Temporary storage, US$0.0724/hour
      |b2s, B2S, 2 Cores, 4 GB RAM, 8 GB Temporary storage, US$0.1096/hour
      |b1ls, B1LS, 1 Core, 0.5 GB RAM, 4 GB Temporary storage, US$0.0662/hour
      |b1ms, B1MS, 1 Core, 2 GB RAM, 4 GB Temporary storage, US$0.0848/hour
      |b2ms, B2MS, 2 Cores, 8 GB RAM, 16 GB Temporary storage, US$0.1592/hour
      |b4ms, B4MS, 4 Cores, 16 GB RAM, 32 GB Temporary storage, US$0.258/hour
      |b8ms, B8MS, 8 Cores, 32 GB RAM, 64 GB Temporary storage, US$0.527/hour
      |b12ms, B12MS, 12 Cores, 48 GB RAM, 96 GB Temporary storage, US$0.725/hour
      |b16ms, B16MS, 16 Cores, 64 GB RAM, 128 GB Temporary storage, US$0.924/hour
      |b20ms, B20MS, 20 Cores, 80 GB RAM, 160 GB Temporary storage, US$1.122/hour
      |a1v2, A1 v2, 1 Core, 2 GB RAM, 10 GB Temporary storage, US$0.103/hour
      |a2v2, A2 v2, 2 Cores, 4 GB RAM, 20 GB Temporary storage, US$0.151/hour
      |a4v2, A4 v2, 4 Cores, 8 GB RAM, 40 GB Temporary storage, US$0.251/hour
      |a8v2, A8 v2, 8 Cores, 16 GB RAM, 80 GB Temporary storage, US$0.53/hour
      |a2mv2, A2m v2, 2 Cores, 16 GB RAM, 20 GB Temporary storage, US$0.209/hour
      |a4mv2, A4m v2, 4 Cores, 32 GB RAM, 40 GB Temporary storage, US$0.357/hour
      |a8mv2, A8m v2, 8 Cores, 64 GB RAM, 80 GB Temporary storage, US$0.724/hour
      |d1, D1, 1 Core, 3.5 GB RAM, 50 GB Temporary storage, US$0.137/hour
      |d2, D2, 2 Cores, 7 GB RAM, 100 GB Temporary storage, US$0.214/hour
      |d3, D3, 4 Cores, 14 GB RAM, 200 GB Temporary storage, US$0.368/hour
      |d4, D4, 8 Cores, 28 GB RAM, 400 GB Temporary storage, US$0.746/hour
      |d11, D11, 2 Cores, 14 GB RAM, 100 GB Temporary storage, US$0.253/hour
      |d12, D12, 4 Cores, 28 GB RAM, 200 GB Temporary storage, US$0.446/hour
      |d13, D13, 8 Cores, 56 GB RAM, 400 GB Temporary storage, US$0.901/hour
      |d14, D14, 16 Cores, 112 GB RAM, 800 GB Temporary storage, US$1.672/hour
      |d2av4, D2a v4, 2 Cores, 8 GB RAM, 50 GB Temporary storage, US$0.172/hour
      |d4av4, D4a v4, 4 Cores, 16 GB RAM, 100 GB Temporary storage, US$0.284/hour
      |d8av4, D8a v4, 8 Cores, 32 GB RAM, 200 GB Temporary storage, US$0.578/hour
      |d16av4, D16a v4, 16 Cores, 64 GB RAM, 400 GB Temporary storage, US$1.026/hour
      |d32av4, D32a v4, 32 Cores, 128 GB RAM, 800 GB Temporary storage, US$1.922/hour
      |d48av4, D48a v4, 48 Cores, 192 GB RAM, 1200 GB Temporary storage, US$2.818/hour
      |d64av4, D64a v4, 64 Cores, 256 GB RAM, 1600 GB Temporary storage, US$3.714/hour
      |d96av4, D96a v4, 96 Cores, 384 GB RAM, 2400 GB Temporary storage, US$5.506/hour
      |d2asv4, D2as v4, 2 Cores, 8 GB RAM, 16 GB Temporary storage, US$0.172/hour
      |d4asv4, D4as v4, 4 Cores, 16 GB RAM, 32 GB Temporary storage, US$0.284/hour
      |d8asv4, D8as v4, 8 Cores, 32 GB RAM, 64 GB Temporary storage, US$0.578/hour
      |d16asv4, D16as v4, 16 Cores, 64 GB RAM, 128 GB Temporary storage, US$1.026/hour
      |d32asv4, D32as v4, 32 Cores, 128 GB RAM, 256 GB Temporary storage, US$1.922/hour
      |d48asv4, D48as v4, 48 Cores, 192 GB RAM, 384 GB Temporary storage, US$2.818/hour
      |d64asv4, D64as v4, 64 Cores, 256 GB RAM, 512 GB Temporary storage, US$3.714/hour
      |d96asv4, D96as v4, 96 Cores, 384 GB RAM, 768 GB Temporary storage, US$5.506/hour
      |dc1sv2, DC1s v2, 1 Core, 4 GB RAM, 50 GB Temporary storage, US$0.317/hour
      |dc2sv2, DC2s v2, 2 Cores, 8 GB RAM, 100 GB Temporary storage, US$0.574/hour
      |dc4sv2, DC4s v2, 4 Cores, 16 GB RAM, 200 GB Temporary storage, US$1.088/hour
      |dc8sv2, DC8 v2, 8 Cores, 32 GB RAM, 400 GB Temporary storage, US$2.186/hour
      |d2dsv4, D2ds v4, 2 Cores, 8 GB RAM, 75 GB Temporary storage, US$0.193/hour
      |d4dsv4, D4ds v4, 4 Cores, 16 GB RAM, 150 GB Temporary storage, US$0.326/hour
      |d8dsv4, D8ds v4, 8 Cores, 32 GB RAM, 300 GB Temporary storage, US$0.662/hour
      |d16dsv4, D16ds v4, 16 Cores, 64 GB RAM, 600 GB Temporary storage, US$1.194/hour
      |d32dsv4, D32ds v4, 32 Cores, 128 GB RAM, 1200 GB Temporary storage, US$2.258/hour
      |d48dsv4, D48ds v4, 48 Cores, 192 GB RAM, 1800 GB Temporary storage, US$3.322/hour
      |d64dsv4, D64ds v4, 64 Cores, 256 GB RAM, 2400 GB Temporary storage, US$4.386/hour
      |d2dv4, D2d v4, 2 Cores, 8 GB RAM, 75 GB Temporary storage, US$0.193/hour
      |d4dv4, D4d v4, 4 Cores, 16 GB RAM, 150 GB Temporary storage, US$0.326/hour
      |d8dv4, D8d v4, 8 Cores, 32 GB RAM, 300 GB Temporary storage, US$0.662/hour
      |d16dv4, D16d v4, 16 Cores, 64 GB RAM, 600 GB Temporary storage, US$1.194/hour
      |d32dv4, D32d v4, 32 Cores, 128 GB RAM, 1200 GB Temporary storage, US$2.258/hour
      |d48dv4, D48d v4, 48 Cores, 192 GB RAM, 1800 GB Temporary storage, US$3.322/hour
      |d64dv4, D64d v4, 64 Cores, 256 GB RAM, 2400 GB Temporary storage, US$4.386/hour
      |d1s, D1s, 1 Core, 3.5 GB RAM, 7 GB Temporary storage, US$0.137/hour
      |d2s, D2s, 2 Cores, 7 GB RAM, 14 GB Temporary storage, US$0.214/hour
      |d3s, D3s, 4 Cores, 14 GB RAM, 28 GB Temporary storage, US$0.368/hour
      |d4s, D4s, 8 Cores, 28 GB RAM, 56 GB Temporary storage, US$0.746/hour
      |d11s, D11s, 2 Cores, 14 GB RAM, 28 GB Temporary storage, US$0.253/hour
      |d12s, D12s, 4 Cores, 28 GB RAM, 56 GB Temporary storage, US$0.446/hour
      |d13s, D13s, 8 Cores, 56 GB RAM, 112 GB Temporary storage, US$0.901/hour
      |d14s, D14s, 16 Cores, 112 GB RAM, 224 GB Temporary storage, US$1.672/hour
      |d1v2, D1 v2, 1 Core, 3.5 GB RAM, 50 GB Temporary storage, US$0.13/hour
      |d2v2, D2 v2, 2 Cores, 7 GB RAM, 100 GB Temporary storage, US$0.2/hour
      |d3v2, D3 v2, 4 Cores, 14 GB RAM, 200 GB Temporary storage, US$0.339/hour
      |d4v2, D4 v2, 8 Cores, 28 GB RAM, 400 GB Temporary storage, US$0.689/hour
      |d5v2, D5 v2, 16 Cores, 56 GB RAM, 800 GB Temporary storage, US$1.247/hour
      |d11v2, D11 v2, 2 Cores, 14 GB RAM, 100 GB Temporary storage, US$0.245/hour
      |d12v2, D12 v2, 4 Cores, 28 GB RAM, 200 GB Temporary storage, US$0.43/hour
      |d13v2, D13 v2, 8 Cores, 56 GB RAM, 400 GB Temporary storage, US$0.871/hour
      |d14v2, D14 v2, 16 Cores, 112 GB RAM, 800 GB Temporary storage, US$1.612/hour
      |d15v2, D15 v2, 20 Cores, 140 GB RAM, 1000 GB Temporary storage, US$1.982/hour
      |d15iv2, D15i v2, 20 Cores, 140 GB RAM, 1000 GB Temporary storage, US$1.982/hour
      |ds1v2, DS1 v2, 1 Core, 3.5 GB RAM, 7 GB Temporary storage, US$0.13/hour
      |ds2v2, DS2 v2, 2 Cores, 7 GB RAM, 14 GB Temporary storage, US$0.2/hour
      |ds3v2, DS3 v2, 4 Cores, 14 GB RAM, 28 GB Temporary storage, US$0.339/hour
      |ds4v2, DS4 v2, 8 Cores, 28 GB RAM, 56 GB Temporary storage, US$0.689/hour
      |ds5v2, DS5 v2, 16 Cores, 56 GB RAM, 112 GB Temporary storage, US$1.247/hour
      |ds11, -v2">DS11-1 v2, 1 Core, 14 GB RAM, 28 GB Temporary storage, US$0.245/hour
      |ds11v2, DS11 v2, 2 Cores, 14 GB RAM, 28 GB Temporary storage, US$0.245/hour
      |ds12, -v2">DS12-1 v2, 1 Core, 28 GB RAM, 56 GB Temporary storage, US$0.43/hour
      |ds12, -v2">DS12-2 v2, 2 Cores, 28 GB RAM, 56 GB Temporary storage, US$0.43/hour
      |ds12v2, DS12 v2, 4 Cores, 28 GB RAM, 56 GB Temporary storage, US$0.43/hour
      |ds13, -v2">DS13-2 v2, 2 Cores, 56 GB RAM, 112 GB Temporary storage, US$0.801/hour
      |ds13, -v2">DS13-4 v2, 4 Cores, 56 GB RAM, 112 GB Temporary storage, US$0.801/hour
      |ds13v2, DS13 v2, 8 Cores, 56 GB RAM, 112 GB Temporary storage, US$0.871/hour
      |ds14, -v2">DS14-4 v2, 4 Cores, 112 GB RAM, 224 GB Temporary storage, US$1.542/hour
      |ds14, -v2">DS14-8 v2, 8 Cores, 112 GB RAM, 224 GB Temporary storage, US$1.612/hour
      |ds14v2, DS14 v2, 16 Cores, 112 GB RAM, 224 GB Temporary storage, US$1.612/hour
      |ds15v2, DS15 v2, 20 Cores, 140 GB RAM, 280 GB Temporary storage, US$1.982/hour
      |ds15iv2, DS15i v2, 20 Cores, 140 GB RAM, 280 GB Temporary storage, US$1.982/hour
      |d2v3, D2 v3, 2 Cores, 8 GB RAM, 50 GB Temporary storage, US$0.177/hour
      |d4v3, D4 v3, 4 Cores, 16 GB RAM, 100 GB Temporary storage, US$0.294/hour
      |d8v3, D8 v3, 8 Cores, 32 GB RAM, 200 GB Temporary storage, US$0.598/hour
      |d16v3, D16 v3, 16 Cores, 64 GB RAM, 400 GB Temporary storage, US$1.066/hour
      |d32v3, D32 v3, 32 Cores, 128 GB RAM, 800 GB Temporary storage, US$2.002/hour
      |d48v3, D48 v3, 48 Cores, 192 GB RAM, 1200 GB Temporary storage, US$2.938/hour
      |d64v3, D64 v3, 64 Cores, 256 GB RAM, 1600 GB Temporary storage, US$3.874/hour
      |d2v4, D2 v4, 2 Cores, 8 GB RAM, 0 GB Temporary storage, US$0.172/hour
      |d4v4, D4 v4, 4 Cores, 16 GB RAM, 0 GB Temporary storage, US$0.284/hour
      |d8v4, D8 v4, 8 Cores, 32 GB RAM, 0 GB Temporary storage, US$0.578/hour
      |d16v4, D16 v4, 16 Cores, 64 GB RAM, 0 GB Temporary storage, US$1.026/hour
      |d32v4, D32 v4, 32 Cores, 128 GB RAM, 0 GB Temporary storage, US$1.922/hour
      |d48v4, D48 v4, 48 Cores, 192 GB RAM, 0 GB Temporary storage, US$2.818/hour
      |d64v4, D64 v4, 64 Cores, 256 GB RAM, 0 GB Temporary storage, US$3.714/hour
      |d2sv3, D2s v3, 2 Cores, 8 GB RAM, 16 GB Temporary storage, US$0.177/hour
      |d4sv3, D4s v3, 4 Cores, 16 GB RAM, 32 GB Temporary storage, US$0.294/hour
      |d8sv3, D8s v3, 8 Cores, 32 GB RAM, 64 GB Temporary storage, US$0.598/hour
      |d16sv3, D16s v3, 16 Cores, 64 GB RAM, 128 GB Temporary storage, US$1.066/hour
      |d32sv3, D32s v3, 32 Cores, 128 GB RAM, 256 GB Temporary storage, US$2.002/hour
      |d48sv3, D48s v3, 48 Cores, 192 GB RAM, 384 GB Temporary storage, US$2.938/hour
      |d64sv3, D64s v3, 64 Cores, 256 GB RAM, 512 GB Temporary storage, US$3.874/hour
      |d2sv4, D2s v4, 2 Cores, 8 GB RAM, 0 GB Temporary storage, US$0.172/hour
      |d4sv4, D4s v4, 4 Cores, 16 GB RAM, 0 GB Temporary storage, US$0.284/hour
      |d8sv4, D8s v4, 8 Cores, 32 GB RAM, 0 GB Temporary storage, US$0.578/hour
      |d16sv4, D16s v4, 16 Cores, 64 GB RAM, 0 GB Temporary storage, US$1.026/hour
      |d32sv4, D32s v4, 32 Cores, 128 GB RAM, 0 GB Temporary storage, US$1.922/hour
      |d48sv4, D48s v4, 48 Cores, 192 GB RAM, 0 GB Temporary storage, US$2.818/hour
      |d64sv4, D64s v4, 64 Cores, 256 GB RAM, 0 GB Temporary storage, US$3.714/hour
      |e2av4, E2a v4, 2 Cores, 16 GB RAM, 50 GB Temporary storage, US$0.2/hour
      |e4av4, E4a v4, 4 Cores, 32 GB RAM, 100 GB Temporary storage, US$0.34/hour
      |e8av4, E8a v4, 8 Cores, 64 GB RAM, 200 GB Temporary storage, US$0.69/hour
      |e16av4, E16a v4, 16 Cores, 128 GB RAM, 400 GB Temporary storage, US$1.25/hour
      |e20av4, E20a v4, 20 Cores, 160 GB RAM, 500 GB Temporary storage, US$1.53/hour
      |e32av4, E32a v4, 32 Cores, 256 GB RAM, 800 GB Temporary storage, US$2.37/hour
      |e48av4, E48a v4, 48 Cores, 384 GB RAM, 1200 GB Temporary storage, US$3.49/hour
      |e64av4, E64a v4, 64 Cores, 512 GB RAM, 1600 GB Temporary storage, US$4.61/hour
      |e96av4, E96a v4, 96 Cores, 672 GB RAM, 2400 GB Temporary storage, US$6.85/hour
      |e2asv4, E2as v4, 2 Cores, 16 GB RAM, 32 GB Temporary storage, US$0.2/hour
      |e4, -v4">E4-2as v4, 2 Cores, 32 GB RAM, 100 GB Temporary storage, US$0.34/hour
      |e4asv4, E4as v4, 4 Cores, 32 GB RAM, 64 GB Temporary storage, US$0.34/hour
      |e8, -v4">E8-2as v4, 2 Cores, 64 GB RAM, 200 GB Temporary storage, US$0.62/hour
      |e8, -v4">E8-4as v4, 4 Cores, 64 GB RAM, 200 GB Temporary storage, US$0.62/hour
      |e8asv4, E8as v4, 8 Cores, 64 GB RAM, 128 GB Temporary storage, US$0.69/hour
      |e16, -v4">E16-4as v4, 4 Cores, 128 GB RAM, 400 GB Temporary storage, US$1.18/hour
      |e16, -v4">E16-8as v4, 8 Cores, 128 GB RAM, 400 GB Temporary storage, US$1.25/hour
      |e16asv4, E16as v4, 16 Cores, 128 GB RAM, 256 GB Temporary storage, US$1.25/hour
      |e20asv4, E20as v4, 20 Cores, 160 GB RAM, 320 GB Temporary storage, US$1.53/hour
      |e32, -v4">E32-8as v4, 8 Cores, 256 GB RAM, 800 GB Temporary storage, US$2.37/hour
      |e32, -v4">E32-16as v4, 16 Cores, 256 GB RAM, 800 GB Temporary storage, US$2.37/hour
      |e32asv4, E32as v4, 32 Cores, 256 GB RAM, 512 GB Temporary storage, US$2.37/hour
      |e48asv4, E48as v4, 48 Cores, 384 GB RAM, 768 GB Temporary storage, US$3.49/hour
      |e64, -v4">E64-16as v4, 16 Cores, 512 GB RAM, 1600 GB Temporary storage, US$4.61/hour
      |e64, -v4">E64-32as v4, 32 Cores, 512 GB RAM, 1600 GB Temporary storage, US$4.61/hour
      |e64asv4, E64as v4, 64 Cores, 512 GB RAM, 1024 GB Temporary storage, US$4.61/hour
      |e96, -v4">E96-24as v4, 24 Cores, 672 GB RAM, 2400 GB Temporary storage, US$6.85/hour
      |e96, -v4">E96-48as v4, 48 Cores, 672 GB RAM, 2400 GB Temporary storage, US$6.85/hour
      |e96asv4, E96as v4, 96 Cores, 672 GB RAM, 1344 GB Temporary storage, US$6.85/hour
      |e2dsv4, E2ds v4, 2 Cores, 16 GB RAM, 75 GB Temporary storage, US$0.222/hour
      |e4, -v4">E4-2ds v4, 2 Cores, 32 GB RAM, 150 GB Temporary storage, US$0.384/hour
      |e4dsv4, E4ds v4, 4 Cores, 32 GB RAM, 150 GB Temporary storage, US$0.384/hour
      |e8, -v4">E8-2ds v4, 2 Cores, 64 GB RAM, 300 GB Temporary storage, US$0.708/hour
      |e8, -v4">E8-4ds v4, 4 Cores, 64 GB RAM, 300 GB Temporary storage, US$0.708/hour
      |e8dsv4, E8ds v4, 8 Cores, 64 GB RAM, 300 GB Temporary storage, US$0.778/hour
      |e16, -v4">E16-4ds v4, 4 Cores, 128 GB RAM, 600 GB Temporary storage, US$1.356/hour
      |e16, -v4">E16-8ds v4, 8 Cores, 128 GB RAM, 600 GB Temporary storage, US$1.426/hour
      |e16dsv4, E16ds v4, 16 Cores, 128 GB RAM, 600 GB Temporary storage, US$1.426/hour
      |e20dsv4, E20ds v4, 20 Cores, 160 GB RAM, 750 GB Temporary storage, US$1.75/hour
      |e32, -v4">E32-8ds v4, 8 Cores, 256 GB RAM, 1200 GB Temporary storage, US$2.722/hour
      |e32, -v4">E32-16ds v4, 16 Cores, 256 GB RAM, 1200 GB Temporary storage, US$2.722/hour
      |e32dsv4, E32ds v4, 32 Cores, 256 GB RAM, 1200 GB Temporary storage, US$2.722/hour
      |e48dsv4, E48ds v4, 48 Cores, 384 GB RAM, 1800 GB Temporary storage, US$4.018/hour
      |e64, -v4">E64-16ds v4, 16 Cores, 504 GB RAM, 2400 GB Temporary storage, US$5.314/hour
      |e64, -v4">E64-32ds v4, 32 Cores, 504 GB RAM, 2400 GB Temporary storage, US$5.314/hour
      |e64dsv4, E64ds v4, 64 Cores, 504 GB RAM, 2400 GB Temporary storage, US$5.314/hour
      |e2dv4, E2d v4, 2 Cores, 16 GB RAM, 75 GB Temporary storage, US$0.222/hour
      |e4dv4, E4d v4, 4 Cores, 32 GB RAM, 150 GB Temporary storage, US$0.384/hour
      |e8dv4, E8d v4, 8 Cores, 64 GB RAM, 300 GB Temporary storage, US$0.778/hour
      |e16dv4, E16d v4, 16 Cores, 128 GB RAM, 600 GB Temporary storage, US$1.426/hour
      |e20dv4, E20d v4, 20 Cores, 160 GB RAM, 750 GB Temporary storage, US$1.75/hour
      |e32dv4, E32d v4, 32 Cores, 256 GB RAM, 1200 GB Temporary storage, US$2.722/hour
      |e48dv4, E48d v4, 48 Cores, 384 GB RAM, 1800 GB Temporary storage, US$4.018/hour
      |e64dv4, E64d v4, 64 Cores, 504 GB RAM, 2400 GB Temporary storage, US$5.314/hour
      |e2v3, E2 v3, 2 Cores, 16 GB RAM, 50 GB Temporary storage, US$0.208/hour
      |e4v3, E4 v3, 4 Cores, 32 GB RAM, 100 GB Temporary storage, US$0.356/hour
      |e8v3, E8 v3, 8 Cores, 64 GB RAM, 200 GB Temporary storage, US$0.69/hour
      |e16v3, E16 v3, 16 Cores, 128 GB RAM, 400 GB Temporary storage, US$1.25/hour
      |e20v3, E20 v3, 20 Cores, 160 GB RAM, 500 GB Temporary storage, US$1.61/hour
      |e32v3, E32 v3, 32 Cores, 256 GB RAM, 800 GB Temporary storage, US$2.37/hour
      |e48v3, E48 v3, 48 Cores, 384 GB RAM, 1200 GB Temporary storage, US$3.49/hour
      |e64iv3, E64i v3, 64 Cores, 432 GB RAM, 1600 GB Temporary storage, US$4.162/hour
      |e64v3, E64 v3, 64 Cores, 432 GB RAM, 1600 GB Temporary storage, US$4.162/hour
      |e2v4, E2 v4, 2 Cores, 16 GB RAM, 0 GB Temporary storage, US$0.2/hour
      |e4v4, E4 v4, 4 Cores, 32 GB RAM, 0 GB Temporary storage, US$0.34/hour
      |e8v4, E8 v4, 8 Cores, 64 GB RAM, 0 GB Temporary storage, US$0.69/hour
      |e16v4, E16 v4, 16 Cores, 128 GB RAM, 0 GB Temporary storage, US$1.25/hour
      |e32v4, E32 v4, 32 Cores, 256 GB RAM, 0 GB Temporary storage, US$2.37/hour
      |e48v4, E48 v4, 48 Cores, 384 GB RAM, 0 GB Temporary storage, US$3.49/hour
      |e64v4, E64 v4, 64 Cores, 504 GB RAM, 0 GB Temporary storage, US$4.61/hour
      |e2sv3, E2s v3, 2 Cores, 16 GB RAM, 32 GB Temporary storage, US$0.208/hour
      |e4, -v3">E4-2s v3, 2 Cores, 32 GB RAM, 64 GB Temporary storage, US$0.356/hour
      |e4sv3, E4s v3, 4 Cores, 32 GB RAM, 64 GB Temporary storage, US$0.356/hour
      |e8, -v3">E8-2s v3, 2 Cores, 64 GB RAM, 128 GB Temporary storage, US$0.62/hour
      |e8, -v3">E8-4s v3, 4 Cores, 64 GB RAM, 128 GB Temporary storage, US$0.62/hour
      |e8sv3, E8s v3, 8 Cores, 64 GB RAM, 128 GB Temporary storage, US$0.69/hour
      |e16, -v3">E16-4s v3, 4 Cores, 128 GB RAM, 256 GB Temporary storage, US$1.18/hour
      |e16, -v3">E16-8s v3, 8 Cores, 128 GB RAM, 256 GB Temporary storage, US$1.25/hour
      |e16sv3, E16s v3, 16 Cores, 128 GB RAM, 256 GB Temporary storage, US$1.25/hour
      |e20sv3, E20s v3, 20 Cores, 160 GB RAM, 320 GB Temporary storage, US$1.61/hour
      |e32, -v3">E32-8s v3, 8 Cores, 256 GB RAM, 512 GB Temporary storage, US$2.37/hour
      |e32, -v3">E32-16s v3, 16 Cores, 256 GB RAM, 512 GB Temporary storage, US$2.37/hour
      |e32sv3, E32s v3, 32 Cores, 256 GB RAM, 512 GB Temporary storage, US$2.37/hour
      |e48sv3, E48s v3, 48 Cores, 384 GB RAM, 768 GB Temporary storage, US$3.49/hour
      |e64, -v3">E64-16s v3, 16 Cores, 432 GB RAM, 864 GB Temporary storage, US$4.162/hour
      |e64, -v3">E64-32s v3, 32 Cores, 432 GB RAM, 864 GB Temporary storage, US$4.162/hour
      |e64isv3, E64is v3, 64 Cores, 432 GB RAM, 864 GB Temporary storage, US$4.162/hour
      |e64sv3, E64s v3, 64 Cores, 432 GB RAM, 864 GB Temporary storage, US$4.162/hour
      |e2sv4, E2s v4, 2 Cores, 16 GB RAM, 0 GB Temporary storage, US$0.2/hour
      |e4, -v4">E4-2s v4, 2 Cores, 32 GB RAM, 0 GB Temporary storage, US$0.34/hour
      |e4sv4, E4s v4, 4 Cores, 32 GB RAM, 0 GB Temporary storage, US$0.34/hour
      |e8, -v4">E8-2s v4, 2 Cores, 64 GB RAM, 0 GB Temporary storage, US$0.62/hour
      |e8, -v4">E8-4s v4, 4 Cores, 64 GB RAM, 0 GB Temporary storage, US$0.62/hour
      |e8sv4, E8s v4, 8 Cores, 64 GB RAM, 0 GB Temporary storage, US$0.69/hour
      |e16, -v4">E16-4s v4, 4 Cores, 128 GB RAM, 0 GB Temporary storage, US$1.18/hour
      |e16, -v4">E16-8s v4, 8 Cores, 128 GB RAM, 0 GB Temporary storage, US$1.25/hour
      |e16sv4, E16s v4, 16 Cores, 128 GB RAM, 0 GB Temporary storage, US$1.25/hour
      |e20sv4, E20s v4, 20 Cores, 160 GB RAM, 0 GB Temporary storage, US$1.53/hour
      |e32, -v4">E32-8s v4, 8 Cores, 256 GB RAM, 0 GB Temporary storage, US$2.37/hour
      |e32, -v4">E32-16s v4, 16 Cores, 256 GB RAM, 0 GB Temporary storage, US$2.37/hour
      |e32sv4, E32s v4, 32 Cores, 256 GB RAM, 0 GB Temporary storage, US$2.37/hour
      |e48sv4, E48s v4, 48 Cores, 384 GB RAM, 0 GB Temporary storage, US$3.49/hour
      |e64, -v4">E64-16s v4, 16 Cores, 504 GB RAM, 0 GB Temporary storage, US$4.61/hour
      |e64, -v4">E64-32s v4, 32 Cores, 504 GB RAM, 0 GB Temporary storage, US$4.61/hour
      |e64sv4, E64s v4, 64 Cores, 504 GB RAM, 0 GB Temporary storage, US$4.61/hour
      |f1, F1, 1 Core, 2 GB RAM, 16 GB Temporary storage, US$0.122/hour
      |f2, F2, 2 Cores, 4 GB RAM, 32 GB Temporary storage, US$0.184/hour
      |f4, F4, 4 Cores, 8 GB RAM, 64 GB Temporary storage, US$0.309/hour
      |f8, F8, 8 Cores, 16 GB RAM, 128 GB Temporary storage, US$0.628/hour
      |f16, F16, 16 Cores, 32 GB RAM, 256 GB Temporary storage, US$1.126/hour
      |f1s, F1s, 1 Core, 2 GB RAM, 4 GB Temporary storage, US$0.122/hour
      |f2s, F2s, 2 Cores, 4 GB RAM, 8 GB Temporary storage, US$0.184/hour
      |f4s, F4s, 4 Cores, 8 GB RAM, 16 GB Temporary storage, US$0.309/hour
      |f8s, F8s, 8 Cores, 16 GB RAM, 32 GB Temporary storage, US$0.628/hour
      |f16s, F16s, 16 Cores, 32 GB RAM, 64 GB Temporary storage, US$1.126/hour
      |f2sv2, F2s v2, 2 Cores, 4 GB RAM, 16 GB Temporary storage, US$0.166/hour
      |f4sv2, F4s v2, 4 Cores, 8 GB RAM, 32 GB Temporary storage, US$0.272/hour
      |f8sv2, F8s v2, 8 Cores, 16 GB RAM, 64 GB Temporary storage, US$0.554/hour
      |f16sv2, F16s v2, 16 Cores, 32 GB RAM, 128 GB Temporary storage, US$0.978/hour
      |f32sv2, F32s v2, 32 Cores, 64 GB RAM, 256 GB Temporary storage, US$1.826/hour
      |f48sv2, F48s v2, 48 Cores, 96 GB RAM, 384 GB Temporary storage, US$2.674/hour
      |f64sv2, F64s v2, 64 Cores, 128 GB RAM, 512 GB Temporary storage, US$3.522/hour
      |f72sv2, F72s v2, 72 Cores, 144 GB RAM, 576 GB Temporary storage, US$3.946/hour
      |g1, G1, 2 Cores, 28 GB RAM, 384 GB Temporary storage, US$0.67/hour
      |g2, G2, 4 Cores, 56 GB RAM, 768 GB Temporary storage, US$1.28/hour
      |g3, G3, 8 Cores, 112 GB RAM, 1536 GB Temporary storage, US$2.57/hour
      |g4, G4, 16 Cores, 224 GB RAM, 3072 GB Temporary storage, US$5.01/hour
      |g5, G5, 32 Cores, 448 GB RAM, 6144 GB Temporary storage, US$8.82/hour
      |gs4, ">Gs4-4, 4 Cores, 224 GB RAM, 3072 GB Temporary storage, US$4.94/hour
      |gs4, ">Gs4-8, 8 Cores, 224 GB RAM, 3072 GB Temporary storage, US$5.01/hour
      |gs5, ">Gs5-8, 8 Cores, 448 GB RAM, 6144 GB Temporary storage, US$8.82/hour
      |gs5, ">Gs5-16, 16 Cores, 448 GB RAM, 6144 GB Temporary storage, US$8.82/hour
      |gs1, Gs1, 2 Cores, 28 GB RAM, 56 GB Temporary storage, US$0.67/hour
      |gs2, Gs2, 4 Cores, 56 GB RAM, 112 GB Temporary storage, US$1.28/hour
      |gs3, Gs3, 8 Cores, 112 GB RAM, 224 GB Temporary storage, US$2.57/hour
      |gs4, Gs4, 16 Cores, 224 GB RAM, 448 GB Temporary storage, US$5.01/hour
      |gs5, Gs5, 32 Cores, 448 GB RAM, 896 GB Temporary storage, US$8.82/hour
      |h8, H8, 8 Cores, 56 GB RAM, 1000 GB Temporary storage, US$1.101/hour
      |h16, H16, 16 Cores, 112 GB RAM, 2000 GB Temporary storage, US$2.071/hour
      |h8m, H8m, 8 Cores, 112 GB RAM, 1000 GB Temporary storage, US$1.431/hour
      |h16m, H16m, 16 Cores, 224 GB RAM, 2000 GB Temporary storage, US$2.731/hour
      |h16mr, H16mr, 16 Cores, 224 GB RAM, 2000 GB Temporary storage, US$2.991/hour
      |h16r, H16r, 16 Cores, 112 GB RAM, 2000 GB Temporary storage, US$2.266/hour
      |h8promo, H8 Promo, 8 Cores, 56 GB RAM, 1000 GB Temporary storage, US$0.712/hour
      |h16promo, H16 Promo, 16 Cores, 112 GB RAM, 2000 GB Temporary storage, US$1.295/hour
      |h8mpromo, H8m Promo, 8 Cores, 112 GB RAM, 1000 GB Temporary storage, US$0.91/hour
      |h16mpromo, H16m Promo, 16 Cores, 224 GB RAM, 2000 GB Temporary storage, US$1.691/hour
      |h16mrpromo, H16mr Promo, 16 Cores, 224 GB RAM, 2000 GB Temporary storage, US$1.847/hour
      |h16rpromo, H16r Promo, 16 Cores, 112 GB RAM, 2000 GB Temporary storage, US$1.411/hour
      |l4s, L4s, 4 Cores, 32 GB RAM, 678 GB Temporary storage, US$0.404/hour
      |l8s, L8s, 8 Cores, 64 GB RAM, 1388 GB Temporary storage, US$0.818/hour
      |l16s, L16s, 16 Cores, 128 GB RAM, 2807 GB Temporary storage, US$1.506/hour
      |l32s, L32s, 32 Cores, 256 GB RAM, 5630 GB Temporary storage, US$2.882/hour
      |l8sv2, L8s v2, 8 Cores, 64 GB RAM, 80 GB Temporary storage, US$0.818/hour
      |l16sv2, L16s v2, 16 Cores, 128 GB RAM, 160 GB Temporary storage, US$1.506/hour
      |l32sv2, L32s v2, 32 Cores, 256 GB RAM, 320 GB Temporary storage, US$2.882/hour
      |l48sv2, L48s v2, 48 Cores, 384 GB RAM, 480 GB Temporary storage, US$4.258/hour
      |l64sv2, L64s v2, 64 Cores, 512 GB RAM, 640 GB Temporary storage, US$5.634/hour
      |l80sv2, L80s v2, 80 Cores, 640 GB RAM, 800 GB Temporary storage, US$7.01/hour
      |nc4ast4v3, NC4as T4 v3, 4 Cores, 28 GB RAM, 180 GB Temporary storage, US$0.691/hour
      |nc8ast4v3, NC8as T4 v3, 8 Cores, 56 GB RAM, 360 GB Temporary storage, US$1.032/hour
      |nc16ast4v3, NC16as T4 v3, 16 Cores, 112 GB RAM, 360 GB Temporary storage, US$1.575/hour
      |nc64ast4v3, NC64as T4 v3, 64 Cores, 448 GB RAM, 2880 GB Temporary storage, US$5.352/hour
      |nc6promo, NC6 Promo, 6 Cores, 56 GB RAM, 340 GB Temporary storage, US$0.605/hour
      |nc12promo, NC12 Promo, 12 Cores, 112 GB RAM, 680 GB Temporary storage, US$1.08/hour
      |nc24promo, NC24 Promo, 24 Cores, 224 GB RAM, 1440 GB Temporary storage, US$2.031/hour
      |nc24rpromo, NC24r Promo, 24 Cores, 224 GB RAM, 1440 GB Temporary storage, US$2.221/hour
      |nv12sv3, NV12s v3, 12 Cores, 112 GB RAM, 736 GB Temporary storage, US$1.27/hour
      |nv24sv3, NV24s v3, 24 Cores, 224 GB RAM, 1474 GB Temporary storage, US$2.41/hour
      |nv48sv3, NV48s v3, 48 Cores, 448 GB RAM, 2948 GB Temporary storage, US$4.69/hour
      |m64, M64, 64 Cores, 1024 GB RAM, 7168 GB Temporary storage, US$8.8/hour
      |m128, M128, 128 Cores, 2048 GB RAM, 14336 GB Temporary storage, US$17.469/hour
      |m32ls, M32ls, 32 Cores, 256 GB RAM, 1024 GB Temporary storage, US$3.331/hour
      |m64ls, M64ls, 64 Cores, 512 GB RAM, 2048 GB Temporary storage, US$6.533/hour
      |m64m, M64m, 64 Cores, 1792 GB RAM, 7168 GB Temporary storage, US$17.477/hour
      |m128m, M128m, 128 Cores, 3892 GB RAM, 14336 GB Temporary storage, US$34.824/hour
      |m8, ">M8-2ms, 2 Cores, 219 GB RAM, 256 GB Temporary storage, US$2.057/hour
      |m8, ">M8-4ms, 4 Cores, 219 GB RAM, 256 GB Temporary storage, US$2.057/hour
      |m8ms, M8ms, 8 Cores, 219 GB RAM, 256 GB Temporary storage, US$2.127/hour
      |m16, ">M16-4ms, 4 Cores, 438 GB RAM, 256 GB Temporary storage, US$4.055/hour
      |m16, ">M16-8ms, 8 Cores, 438 GB RAM, 256 GB Temporary storage, US$4.125/hour
      |m16ms, M16ms, 16 Cores, 438 GB RAM, 512 GB Temporary storage, US$4.125/hour
      |m32, ">M32-8ms, 8 Cores, 875 GB RAM, 1024 GB Temporary storage, US$8.12/hour
      |m32, ">M32-16ms, 16 Cores, 875 GB RAM, 1024 GB Temporary storage, US$8.12/hour
      |m32ms, M32ms, 32 Cores, 875 GB RAM, 1024 GB Temporary storage, US$8.12/hour
      |m64, ">M64-16ms, 16 Cores, 1750 GB RAM, 2048 GB Temporary storage, US$17.477/hour
      |m64, ">M64-32ms, 32 Cores, 1750 GB RAM, 2048 GB Temporary storage, US$17.477/hour
      |m64ms, M64ms, 64 Cores, 1792 GB RAM, 2048 GB Temporary storage, US$17.477/hour
      |m128, ">M128-32ms, 32 Cores, 3800 GB RAM, 4096 GB Temporary storage, US$34.824/hour
      |m128, ">M128-64ms, 64 Cores, 3800 GB RAM, 4096 GB Temporary storage, US$34.824/hour
      |m128ms, M128ms, 128 Cores, 3892 GB RAM, 4096 GB Temporary storage, US$34.824/hour
      |m64s, M64s, 64 Cores, 1024 GB RAM, 2048 GB Temporary storage, US$8.8/hour
      |m128s, M128s, 128 Cores, 2048 GB RAM, 4096 GB Temporary storage, US$17.469/hour
      |m32ts, M32ts, 32 Cores, 192 GB RAM, 1024 GB Temporary storage, US$3.146/hour
      |m208msv2, M208ms v2, 208 Cores, 5700 GB RAM, 4096 GB Temporary storage, US$58.136/hour
      |m416msv2, M416ms v2, 416 Cores, 11400 GB RAM, 8192 GB Temporary storage, US$129.03/hour
      |m208sv2, M208s v2, 208 Cores, 2850 GB RAM, 4096 GB Temporary storage, US$29.133/hour
      |m416sv2, M416s v2, 416 Cores, 5700 GB RAM, 8192 GB Temporary storage, US$64.584/hour
      |""".stripMargin
}
