package rosettaCode

import scala.util.matching.Regex
import java.net.Inet6Address

/**
 * Attempted solution to http://rosettacode.org/wiki/Parse_an_IP_Address
 */

object IpAddressParser extends App {

  val IPV4 = 4
  val IPV6 = 6
  type ResultType = (Int, (Long, Long), Int) // (IPV4 or IPV6, (addr-part1, addr-part2), port)

  var ipAddresses: Array[(String, ResultType)] = Array(
    ("0.0.0.0", (IPV4, (0L, 0L), -1))
    , ("127.0.0.1", (IPV4, (0x0L, 0x7f000001L), -1))
    , ("127.0.0.1:80", (IPV4, (0x0L, 0x7f000001L), 80))
//            ,("::1", (IPV6, (0x0L, 0x0000000000000001L), -1))
//            ,("[::1]:80", (IPV6, (0x0L, 0x0000000000000001L), 80))
//            ,("2605:2700:0:3::4713:93e3", (IPV6, (0x2605270000000003L, 0x00000000471393e3L), -1))
//            ,("[2605:2700:0:3::4713:93e3]:80", (IPV6, (0x2605270000000003L, 0x00000000471393e3L), 80))
  , ("1080:0:0:0:8:800:200C:417A", (IPV6, (0x1080000000000000L, 0x00080800200c417aL), -1))
  )

  for (ipAddress <- ipAddresses) {
    val expectedResult = ipAddress._2
    val ipType = expectedResult._1
    val ipAddressPart1 = expectedResult._2._1
    val ipAddressPart2 = expectedResult._2._2
    val port = expectedResult._3
    try {
      val result: Option[ResultType] = parse(ipAddress._1)
      val tuple = result.getOrElse((0, (0L, 0L), -1))
      println(s"expected: $ipType,($ipAddressPart1,$ipAddressPart2),$port")
      println("actual  : " + tuple._1 + ",(" + tuple._2._1 + "," + tuple._2._2 + ")," + tuple._3)

      assert(tuple._1 == ipType,
        s"wrong IP version returned:$tuple")

      assert(tuple._2._1 == ipAddressPart1 && tuple._2._2 == ipAddressPart2,
        s"wrong IP address returned:$tuple")

      assert(tuple._3 == port,
        s"wrong port number returned:$tuple")

    }
    catch {
      case e: AssertionError => println(e.toString); e.printStackTrace
      case e: Exception => println(e.toString); e.printStackTrace
    }
  }

  def parse(ipAddress: String): Option[ResultType] = {
    val classified = classifyIpAddress(ipAddress)
    var ipvType = classified._1
    var port = classified._2
    var result: Option[ResultType] = None
    ipvType match {
      case IPV4 => result = parseIPV4(ipAddress, port)
      case IPV6 => result = parseIPV6(ipAddress, port)
      case _ => throw new RuntimeException("Unrecognized IP version: " + ipvType)
    }
    result
  }

  def parseIPV4(ipAddress: String, port: Int) : Option[ResultType] = {
    var result: Option[ResultType] = None
    var matched = isIPV4withPort(ipAddress)
    if (matched.hasNext) {
      val str = matched.next
      println("matched=" + str)
      val partitioned = str.split("\\:")
      result = Some[ResultType](IPV4, (0, makeIntFromIpv4Addr(partitioned(0))), partitioned(1).toInt)
    }
    else {
      matched = isIPV4(ipAddress)
      if (matched.hasNext) {
        val str = matched.next
        println("matched=" + str)
        result = Some[ResultType](IPV4, (0, makeIntFromIpv4Addr(str)), port)
      }
    }
    result
  }

  // Consider using regular expressions from
  // https://stackoverflow.com/questions/53497/regular-expression-that-matches-valid-ipv6-addresses
  //
  // Reference: https://tools.ietf.org/html/rfc4291

  def parseIPV6(ipAddress: String, port: Int): Option[ResultType] = {
    val ipvType = IPV6
    var addrPart1: Long = 0
    var addrPart2: Long = 0
    if (port == -1) {
      val parts = ipAddress.split(":")
      if (parts.size == 8) {
        // The simple case
        for(part <- 0 to 3) {
          part match {
            case 0 => addrPart1 |= hexToLong(parts(0))<<48
            case 1 => addrPart1 |= hexToLong(parts(1))<<32
            case 2 => addrPart1 |= hexToLong(parts(2))<<16
            case 3 => addrPart1 |= hexToLong(parts(3))
          }
        }
        for(part <- 4 to 7) {
          part match {
            case 4 => addrPart2 |= hexToLong(parts(4))<<48
            case 5 => addrPart2 |= hexToLong(parts(5))<<32
            case 6 => addrPart2 |= hexToLong(parts(6))<<16
            case 7 => addrPart2 |= hexToLong(parts(7))
          }
        }
      }
    }
    Some[ResultType](ipvType,(addrPart1,addrPart2),port)
  }

  /**
   * Convert hex string to numeric value.
   * @param hex string limited to 4 digits in length
   * @return numeric value as Long.
   * @throws RuntimeException if hex string is too long.
   */
  def hexToLong(hex: String): Long = {
    var result: Long = 0
    if( hex.length > 4 ) throw new RuntimeException( s"hex string is too long: $hex")
    if (hex.length > 0) {
      val x = lpad4(hex)
      for (nybble <- 0 until x.length) {
        var n: Int = x.charAt(nybble) - '0'
        if (n < 0 || n > 9) {
          x.toUpperCase.charAt(nybble) match {
            case 'A' => n = 10
            case 'B' => n = 11
            case 'C' => n = 12
            case 'D' => n = 13
            case 'E' => n = 14
            case 'F' => n = 15
          }
        }
        nybble match {
          case 0 => result += n << 12
          case 1 => result += n << 8
          case 2 => result += n << 4
          case 3 => result += n
          case _ => throw new RuntimeException("hex string > 4 digits")
        }
      }
    }
    result
  }

  /**
   * Pad string on the left with zeros.
   * @param str String to be padded
   * @return Padded string.
   */
  def lpad4(str:String): String = {
    var padded = ""
    str.length match {
      case 0 => padded = "0000"
      case 1 => padded = "000" + str
      case 2 => padded = "00" + str
      case 3 => padded = "0" + str
      case _ => padded = str
    }
    padded
  }

  def isIPV4(ipAddress:String): Regex.MatchIterator = {
    val regex = """\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}""".r
    val matched = regex.findAllIn(ipAddress)
    matched
  }

  def isIPV4withPort(ipAddress:String): Regex.MatchIterator = {
    val regex = """\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}\:\d+""".r
    val matched = regex.findAllIn(ipAddress)
    matched
  }

  /**
   * Convert IPV4 address to a 32-bit integer.
   * @param ipAddress
   * @return
   */
  def makeIntFromIpv4Addr(ipAddress:String):Int = {
    val parts = ipAddress.split("\\.")
    val result:Int = parts(0).toInt<<24 | parts(1).toInt<<16 | parts(2).toInt<<8 | parts(3).toInt
    result
  }

  /**
   * A quick-and-dirty IP address classifier.
   * @param ipAddress
   * @return Tuple of (IPV type, port number)
   */
  def classifyIpAddress(ipAddress: String): (Int, Int) = {
    var port = -1
    var ndx = -1
    var ipvType = 0
    if (ipAddress.startsWith("[")) {
      ndx = ipAddress.indexOfSlice("]:")
      if (ndx != -1) {
        port = ipAddress.substring(ndx + 2).toInt
      }
      ipvType = IPV6
    } else if (countChars(ipAddress, ':') > 1) {
      ipvType = IPV6
    } else {
      if (countChars(ipAddress,'.') == 3)
      {
        ndx = ipAddress.indexOf(':')
        if (ndx != -1) {
          port = ipAddress.substring(ndx + 1).toInt
        }
        ipvType = IPV4
      }
    }
    (ipvType,port)
  }

  /**
   * Count how many times char appears in str.
   * @param str
   * @param char
   * @return The count.
   */
  def countChars(str:String,char:Char):Int = {
    var count = 0
    for(i <- 0 until str.length()) {
      if(str.charAt(i) == char) count += 1
    }
    count
  }
}
