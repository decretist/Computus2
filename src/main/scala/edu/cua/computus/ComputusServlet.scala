/*
 * Paul Evans (10evans@cua.edu)
 * February 2019
 * May 2017
 * October-December 2012
 */

package edu.cua.computus

import org.scalatra._
import scala.math

class ComputusServlet extends ScalatraServlet {
  get("/") {
    <html>
      <head>
        <title>Computus Simulation Front-end</title>
      </head>
      <body>
        <form method="post">
          <input type="text" name="startDate" value="703" size="3"/>
          <input type="text" name="endDate" value="721" size="3"/>
          <input type="submit" value="Submit"/>
        </form>
      </body>
    </html>
  }

  post("/") {
    val startDate:Int = params.get("startDate").getOrElse("703").toInt
    val endDate:Int = params.get("endDate").getOrElse("721").toInt
    <html>
      <head>
        <title>Computus Simulation Back-end</title>
      </head>
      <body style="font-family: monospace;">
        <table border="1" style="border-collapse: collapse;">
          <tr>
            <th>Year</th>
            <th>Indiction</th>
            <th>Epact</th>
            <th>Concurrent</th>
            <th bgcolor="a0a0a0">19-Year Cycle</th>
            <th bgcolor="a0a0a0">Golden Number</th>
            <th>Lunar Cycle</th>
            <th>Paschal Full Moon (Bede)</th>
            <th bgcolor="a0a0a0">Paschal Full Moon (Table)</th>
            <th bgcolor="a0a0a0">Paschal Full Moon (Mosshammer)</th>
            <th>Easter (Meeus)</th>
          </tr>
          { for (year <- startDate to endDate) yield {
          <tr>
            <td>{year}</td>
            <td>{indiction(year)}</td>
            <td>{epact(year)}</td>
            <td>{concurrent(year)}</td>
            <td bgcolor="a0a0a0">{nineteenYearCycle(year)}</td>
            <td bgcolor="a0a0a0">{goldenNumber(year)}</td>
            <td>{lunarCycle(year)}</td>
            <td>{bedePFM(year)}</td>
            <td bgcolor="a0a0a0">{tablePFM(year)}</td>
            <td bgcolor="a0a0a0">{mosshammerPFM(year)}</td>
            <td>{easter(year)}</td>
          </tr>
          }
          }
        </table>
      </body>
    </html>
  }

  /*
   * If you want to know what the indiction is,
   * take the years of the Lord and add three; divide by 15,
   * and what remains is the indiction of the present year.
   * Bede, DT 14
   */
  def indiction(year: Int): Int = {
    val remainder = (year + 3) % 15
    if (remainder == 0) 15 else remainder
  }

  /*
   * If you wish to know what the lunar epact is,
   * divide the years of the Lord by 19 and multiply the remainder by 11;
   * then divide by 30, and what is left over is the epact.
   * Bede, DT 14
   */
  def epact(year: Int): Int = {
    ((year % 19) * 11) % 30
  }

  /*
   * If you wish to know the concurrent days of the week,
   * take the years of the Lord and add one-fourth of them;
   * then add four to these because in the year of
   * the birth of the Lord the concurrent was five;
   * divide these by seven, and what remains is the epact of the sun.
   * Bede, DT 14
   */
  def concurrent(year: Int): Int = {
    val remainder = ((year + (year / 4)) + 4) % 7
    if (remainder == 0) 7 else remainder
  }

  /*
   * If you want to know what year of the nineteen-year cycle it is,
   * take the years of the Lord, and, adding one,
   * because the Lord was born in the second year of that cycle, divide by 19.
   * The remainder is the year of the nineteen-year cycle.
   * Bede, DT 14
   * This is the number of Dionysius' and Bede's nineteen-year cycle,
   * not the lunar cycle which appears in column 5 of Bede's tables.
   * Kendall and Wallis, ONT/OT, p. 116, fn 61.
   */
  def nineteenYearCycle(year: Int): Int = {
    val remainder = (year + 1) % 19
    if (remainder == 0) 19 else remainder
  }

  /*
   * The 19-year cycle is the same as the golden number,
   * although Bede does not call it that.
   * The formula commonly used to compute the golden number is shown
   * in the function below.
   * PLE
   */
  def goldenNumber(year: Int): Int = {
    (year % 19) + 1
  }

  /*
   * If you wish to know the number of the year of the lunar cycle,
   * take the years of the Lord, subtract two, and divide by 19;
   * what remains is the year of the lunar cycle.
   * Bede, DT 14
   */
  def lunarCycle(year: Int): Int = {
    val remainder = (year - 2) % 19
    if (remainder == 0) 19 else remainder
  }

  /*
   * The skilful computist ought to memorize the fourteenth Moons of
   * the first month, just as he memorizes the annual lunar epacts.
   * Bede, DTR 60
   */
  def tablePFM(year: Int):String = {
    val fourteenthMoons = Array(
      "April  5", "March 25", "April 13", "April  2", "March 22",
      "April 10", "March 30", "April 18", "April  7", "March 27",
      "April 15", "April  4", "March 24", "April 12", "April  1",
      "March 21", "April  9", "March 29", "April 17"
    )
    fourteenthMoons(year % 19)
  }

  /*
   * But if anyone wishes to find it by means of a formula, let him look at the
   * epacts are of the year he wishes to compute, and if they are 14 or 15, let
   * him know that the fourteenth Moon will come on the 11th or 12th kalends of
   * April [22 or 21 March]. For as has often been mentioned, the 11th kalends
   * of this month is the proper date of all the epacts.
   * Bede, DTR 60
   */
  def bedePFM(year: Int): String = {
    val e = epact(year)
    var offset = 0
    /*
     * If the epacts are less than 14, let him count upwards day by day until
     * he reaches the number 14, and let him not doubt that he has there the
     * fourteenth Moon of Easter.
     * Bede, DTR 60
     */
    if (e < 14) offset = 14 - e
    else if (e == 14) offset = 0
    else if (e == 15) offset = -1
    /*
     * But if the epacts are more than 15, let him count upwards day by day up
     * to 30, which is the end of this month. Then beginning at the first Moon
     * and arriving in sequence at the fourteenth, he will find the day duly
     * ordained for the Paschal rites.
     * Bede, DTR 60
     */
    else if (e > 15) offset = (30 - e) + 14
    val daysAfterProper = Array(
      "March 21", "March 22", "", "March 24", "March 25", "", "March 27", "",
      "March 29", "March 30", "", "April  1", "April  2", "", "April  4",
      "April  5", "", "April  7", "", "April  9", "April 10", "", "April 12",
      "April 13", "", "April 15", "", "April 17", "April 18"
    )
    daysAfterProper(offset + 1)
  }

  /*
   * The epact corresponds to the age of the moon on 22 March.  Therefore,
   * the fourteenth day of the moon expressed as a date in March will
   * be 22 + 14 - e, or P = 36 - e. In the 17th year, for example, the
   * epact is 26 and the date of the 14th moon in 9 April. The date can
   * be expressed as 36 - 26 = 10. Since the Paschal full moon cannot
   * fall before 21 March, one adds 30. The 40th of March is equivalent
   * to 9 April. In the 10th year the epact is 9, so P = 36 - 9 = 27 March.
   * Alden A. Mosshammer,
   * The Easter Computus and the Origins of the Christian Era, 96.
   */
  def mosshammerPFM(year: Int): String = {
    val e = epact(year)
    var P = 36 - e
    if (P < 21) P = P + 30
    var dateString = ""
    if ((P / 31) == 0) dateString = "March " + P.toString
    else if ((P / 31) == 1) dateString = "April " + (P % 31).toString
    dateString
  }

  /*
   * Jean Meeus, Astronomical Algorithms, p. 69
   * http://en.wikipedia.org/wiki/Computus#Meeus_Julian_algorithm
   */
  def easter(year: Int): String = {
    val a = year % 4
    val b = year % 7
    val c = year % 19
    val d = ((19 * c) + 15) % 30
    val e = ((2 * a) + (4 * b) - d + 34) % 7
    val month = math.floor((d + e + 114) / 31)
    val day = ((d + e + 114) % 31) + 1
    var dateString = ""
    if (month == 3) dateString = "March " + day.toString
    else if (month == 4) dateString = "April " + day.toString
    dateString
  }
}

